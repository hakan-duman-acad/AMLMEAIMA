############################################################
#                  METEOROLOJİK VERİ SETİ                  #
############################################################

#setwd("...YOL.../AMLMEAIMA")
# bazı yardımcı fonksiyonlar
source("ortakfonks.R")


############################################################
#                   VERİLERİN YÜKLENMESİ                   #
############################################################

# Kaynak kodların test edilmesi için sahte veriler paylaşılmıştır.
# Orijinal verilere aşağıdaki adres üzerinden ulaşılabilir.
# https://www.mgm.gov.tr/veridegerlendirme/il-ve-ilceler-istatistik.aspx?k=A&m=ANKARA

meteo <-  read_tsv(file = "data/meteo.tsv")
iller <- read_tsv(file = "data/meteo_I.tsv") 


############################################################
#                  VERİLERİN DÜZENLENMESİ                  #
############################################################


yaz = c("Nisan","Mayıs","Haziran","Temmuz","Ağustos","Eylül")
kis <- c("Ekim", "Kasım","Aralık", "Ocak","Şubat","Mart")
hep <- c(yaz,kis)
meteoL <- meteo   %>% 
  pivot_wider(names_from = c("degisken","donem"),values_from = "deger")   %>% 
  group_by(IK)  %>% 
  summarise(
    T = ortSıcak_Yıllık, P = ayTopYağMik_Yıllık,
    TSOGUK = min(pick(paste0("ortSıcak_",hep))), 
    TSICAK = max(pick(paste0("ortSıcak_",hep))), 
    PWMIN = min(pick(paste0("ayTopYağMik_",kis))),
    PWMAX = max(pick(paste0("ayTopYağMik_",kis))),
    PSMIN = min(pick(paste0("ayTopYağMik_",yaz))),
    PSMAX = max(pick(paste0("ayTopYağMik_",yaz))))  %>% 
  left_join(iller)  

############################################################
#                    EKSİK VERİ ANALİZİ                    #
############################################################

full <- meteoL 
introText(full  %>% select(-IK,-IA))

eksik_plot(full)

############################################################
#                    KORELASYON GRAFİĞİ                    #
############################################################


cor_plot(full)

# yüksek korelasyon katsayısına sahip olan değişkenler
# veri setinden çıkartılıyor.
full <- full  %>% select(-P,-T)

############################################################
#                KÜMELEME ÖNCESİ ÖN İŞLEMLER               #
############################################################

if (!require("recipes")) install.packages("recipes",
                                             dependencies = TRUE)
clustRec <- recipe(~.,full) %>%
  step_select(-IK,-IA)  %>%
  step_YeoJohnson(all_predictors())  %>% 
  step_normalize(all_predictors())  %>% 
  step_pca(all_predictors(),threshold = .99)  %>% 
  prep()
clustBaked <- bake(clustRec,full) %>% as.data.frame()
rownames(clustBaked) <- full$IA

set.seed(2023)
# hopkins testi
if (!require("hopkins")) install.packages("hopkins",
                                          dependencies = TRUE)
print(hp<-hopkins(X = clustBaked)) # hopkins istatistiği
print(hopkins.pval(x = hp, nrow(clustBaked)/10)) #p değeri
introduce(data =clustBaked)

# VAT grafiği


vat_plot(clustBaked)

############################################################
#                      KÜMELEME İŞLEMİ                     #
############################################################

bestclust <- kum_bul(clustBaked)

clust <- eclust(x = clustBaked, FUNcluster = "diana",k = 4, 
                hc_metric = "euclidean", hc_method = "ward")
# silüyet grafiği
fviz_silhouette(clust, palette = "jco",
                ggtheme = theme_clean())+
  ggtitle(label = "",subtitle = "") 

# kümeleme grafiği
kum_plot(clust)

cate <- tibble(CAT = clust$cluster, IA = rownames(clust$data))  %>% 
  left_join(iller)  %>% 
  select(-IA)  %>% 
  mutate(CAT = factor(CAT)) %>% 
  select(IK,CAT)
fulldata <- full  %>% left_join(cate)  %>% select(-c(IK,IA))

############################################################
#                  H2O İLE MODEL OLUŞTURMA                 #
############################################################

options(OutDec= ".")
m <- h2o(fulldata, n = 40)

############################################################
#                  DALEX İLE XAI GRAFİĞİ                   #
############################################################

kumeDALEX_plot(model = m, data = fulldata)

############################################################
#        KÜMELERİN İSİMLENDİRİLMESİ VE İNCELENMESİ         #
############################################################

levels(cate$CAT) <- c( "sıcak ve kurak","sıcak ve yazları yağışlı",
                       "soğuk ve kurak","ılıman ve kışları yağışlı")

############################################################
#                      COĞRAFİ GÖRÜNÜM                     #
############################################################

kumeleme_cog_gor(x = cate, harita = tr)

############################################################
#                OZ NİTELİK FREKANS GRAFİĞİ                #
############################################################

sigir <- read_rds(file = "data/temel_veri_son.rds")
newData <- cate  %>%  left_join(sigir)  %>% 
  select(CAT,OV)  %>% filter(!is.na(OV))  %>%  mutate(CAT=factor(CAT))


frekans_plot(dt = newData, var = CAT, lab1 = "Mevsimsel Kümeler")

############################################################
#             NORMALLİK VE DİĞER İSTATİSTİKLER             #
############################################################

normallik(x = newData, var = CAT)

############################################################
#               PERMUTASYON HİPOTEZ TESTLERİ               #
############################################################

verim_permutasyon(x = newData,var = CAT,label = "Mevsimsel Kümeler")
box_plot(x = newData, var = CAT, label = "Mevsimsel Kümeler" )

stopCluster(cl)
############################################################
#          HAZIRLANAN VERİ SETİNİN KAYIT EDİLMESİ          #
############################################################

# write_rds(cate %>% rename(MET=CAT)  %>% select(IK,MET),
#           file = "data/met.Rds",
#           compress = "bz")