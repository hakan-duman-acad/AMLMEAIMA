############################################################
#            YAŞAM STANDARTI İNDEKSİ VERİ SETİ             #
############################################################

# setwd("...YOL.../AMLMEAIMA")
set.seed(2023)
source("source_functions.R")
source("source_maps.R")



############################################################
#                   VERİLERİN YÜKLENMESİ                   #
############################################################

# Kaynak kodların test edilmesi için sahte veriler paylaşılmıştır.
# Orijinal verilere aşağıdaki adres üzerinden ulaşılabilir.
# https://data.tuik.gov.tr/Bulten/DownloadIstatistikselTablo?
# p=WLV4Hw5gRJhTxk1yS9sBMxYQvqsl66GVcnNVPUMFm8bLUpkCgUiOtZqn7AMsoQ50

yas <- read_tsv(file = "data/yasIndex.tsv")
iller <- read_tsv(file = "data/il_adlari.tsv")


############################################################
#                  VERİLERİN DÜZENLENMESİ                  #
############################################################

yas <- yas %>%
  pivot_wider(
    names_from = c("degisken"),
    values_from = "deger"
  ) %>%
  left_join(iller)

############################################################
#                    EKSİK VERİ ANALİZİ                    #
############################################################

full <- yas %>% select(
  "IK", "IA", "IMO", "AMA", "SIMO",
  "STSS", "GYYKGHO", "MD"
)
introText(full %>% select(-IK, -IA))

eksik_plot(full)

############################################################
#                    KORELASYON GRAFİĞİ                    #
############################################################

cor_plot(full)

############################################################
#                KÜMELEME ÖNCESİ ÖN İŞLEMLER               #
############################################################

if (!require("recipes")) {
  install.packages("recipes",
    dependencies = TRUE
  )
}
clustRec <- recipe(~., full) %>%
  step_select(-IK, -IA) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), threshold = .99) %>%
  prep()
clustBaked <- bake(clustRec, full) %>% as.data.frame()
rownames(clustBaked) <- full$IA


# hopkins testi
if (!require("hopkins")) {
  install.packages("hopkins",
    dependencies = TRUE
  )
}
hp <- hopkins(X = clustBaked)
print(paste0("Hopkins İstatistiği :", hp)) # hopkins istatistiği
print(paste0(
  "Hopkins p değeri ",
  hopkins.pval(x = hp, nrow(clustBaked) / 10)
)) # p değeri
introduce(data = clustBaked)

# VAT grafiği

vat_plot(clustBaked)

############################################################
#                      KÜMELEME İŞLEMİ                     #
############################################################

bestclust <- kum_bul(clustBaked)

clust <- eclust(
  x = clustBaked, FUNcluster = "kmeans", k = 2,
  hc_metric = "euclidean", hc_method = "ward"
)
# silüyet grafiği
fviz_silhouette(clust,
  palette = "jco",
  ggtheme = theme_clean()
) +
  ggtitle(label = "", subtitle = "")

# kümeleme grafiği
kum_plot(clust)

cate <- tibble(CAT = clust$cluster, IA = rownames(clust$data)) %>%
  left_join(iller) %>%
  select(-IA) %>%
  mutate(CAT = factor(CAT)) %>%
  select(IK, CAT)
fulldata <- full %>%
  left_join(cate) %>%
  select(-c(IK, IA))

############################################################
#                  H2O İLE MODEL OLUŞTURMA                 #
############################################################

options(OutDec = ".")
m <- h2o(fulldata, n = 40)

############################################################
#                  DALEX İLE XAI GRAFİĞİ                   #
############################################################

kumeDALEX_plot(model = m, data = fulldata)

############################################################
#        KÜMELERİN İSİMLENDİRİLMESİ VE İNCELENMESİ         #
############################################################

levels(cate$CAT) <- c("1. bölge", "2. bölge")

############################################################
#                      COĞRAFİ GÖRÜNÜM                     #
############################################################

kumeleme_cog_gor(x = cate, harita = tr)

############################################################
#                OZ NİTELİK FREKANS GRAFİĞİ                #
############################################################

sigir <- read_rds(file = "data/temel_veri_son.rds")
newData <- cate %>%
  left_join(sigir) %>%
  select(CAT, OV) %>%
  filter(!is.na(OV)) %>%
  mutate(CAT = factor(CAT))


frekans_plot(dt = newData, var = CAT, lab1 = "Mevsimsel Kümeler")

############################################################
#             NORMALLİK VE DİĞER İSTATİSTİKLER             #
############################################################

normallik(x = newData, var = CAT)

############################################################
#               PERMUTASYON HİPOTEZ TESTLERİ               #
############################################################

verim_permutasyon(x = newData, var = CAT, label = "Mevsimsel Kümeler")
box_plot(x = newData, var = CAT, label = "Mevsimsel Kümeler")

stopCluster(cl)
############################################################
#          HAZIRLANAN VERİ SETİNİN KAYIT EDİLMESİ          #
############################################################

# write_rds(cate %>% rename(YSI=CAT)  %>% select(IK,YSI),
#           file = "data/ysi.Rds",
#           compress = "bz")
