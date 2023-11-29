############################################################
#        SÜT SIĞIRI VE ÇİFTLİKLERİNE AİT VERİ SETİ         #
############################################################

#setwd("...YOL.../AMLMEAIMA")
# bazı yardımcı fonksiyonlar
source("ortakfonks.R")


############################################################
#                  VERİ SETİNİN YÜKLENMESİ                 #
############################################################
#

# kodları test etmek için oluşturulan sahte veriler yükleniyor
temel_veri <- read_csv(file = "data/simTemel.csv")  %>% 
  rename(
    IK = il_kod,
    IRK = irk, 
    SGS = sagim_sis,
    TSS = top_sigir_say,
    IS = inek_say,
    TS = toh_say,
    OLS = ortLak_sur,
    BS = buzagilama_say,
    OV = ort_verim
  )  %>% 
  distinct()  %>% 
  filter(TSS>0,IS >0)

############################################################
#      VERİ SETİNİN İŞLETME BAZINA GÖRE DÖNÜŞTÜRÜLMESİ     #
############################################################




summary(temel_veri)
if (!require(DataExplorer)) install.packages("DataExplorer", 
                                             dependencies = TRUE)
introduce(temel_veri)

temel_veri <- temel_veri  %>% 
  group_by(IK,isl_no,IRK,SGS,IBM)  %>% 
  summarise_at(vars(TSS, IS, TS, BS, OLS, VLO, SOA, IBTA, OV), 
               median, na.rm=TRUE)  %>% 
  ungroup() %>% 
  select(-isl_no)


############################################################
#            TANIMLAYICI İSTATİSTİKLER TABLOSU             #
############################################################


if (!require(skimr)) install.packages("skimr", dependencies = TRUE)
desc_temel <- skim(temel_veri  %>% 
                     select(-c(IRK,SGS,IK,IBM))) %>% as_tibble()
tablo <- desc_temel  %>% filter(skim_type == "numeric")  %>%  
  select(O = 2,
         EGS = 3,
         BO = 4,
         ORT = 5,
         SS = 6,
         K0 = 7,
         K25 = 8,
         K50 = 9,
         K75 = 10,
         K100 = 11)
print(tablo)

############################################################
#                    EKSİK VERİ ANALİZİ                    #
############################################################

eksik_plot(temel_veri)


# kayıt etmek için (pdf, jpg, png vb)
# ggsave(filename = paste0("../../images/misstemel.pdf"), 
#        width = 6,
#        height = 3.5,
#        device = cairo_pdf )  

# Eksik verilerin coğrafi dağılım grafiği

a <- temel_veri  %>% filter(is.na(OV))  %>%  count(IK)
b <- temel_veri  %>% count(IK)  %>%  left_join(a, by = c("IK"))  %>% 
  mutate(BO  = 1-(n.y/n.x))  
temp <- tr  %>%  left_join(b)
ggplot(temp) + geom_sf(aes(fill=BO)) +
  geom_sf_text(mapping = aes(label=str_to_title(name_tr)),size=1.25, 
               alpha = .7,check_overlap = TRUE) +
  xlab("")+
  ylab("")+ 
  labs(fill = "*BO")+
  scale_fill_distiller(palette = "YlOrBr")+
  theme(legend.position="right")+ 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(plot.background = element_rect(color = "black"))

# OLS öz niteliği eksik verilerinin coğrafi dağılımı
a <- temel_veri  %>% filter(is.na(OLS))  %>%  count(IK)
b <- temel_veri  %>% count(IK)  %>%  left_join(a, by = c("IK"))  %>% 
  mutate(BO  = 1-(n.y/n.x)) 
temp <- tr  %>%  left_join(b)
ggplot(temp) + geom_sf(aes(fill=BO)) +
  geom_sf_text(mapping = aes(label=name_tr),size=1.25, 
               alpha = .7,check_overlap = TRUE) +
  xlab("")+
  ylab("")+ 
  labs(fill = "*BO")+
  scale_fill_distiller(palette = "YlOrBr")+
  theme(legend.position="right")+ 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(plot.background = element_rect(color = "black"))

# eksik veriler siliniyor
temel_veri_baked <- temel_veri %>% na.omit()
introduce(temel_veri_baked)

############################################################
#                    KORELASYON GRAFİĞİ                    #
############################################################

cor_plot(temel_veri_baked)

# Gerçek veri setinde eş doğrusallık problemi bulunan 
# TSS ve IS değişkenleri arasından IS veri setinden çıkartılıyor
temel_veri_baked <- temel_veri_baked %>% select(-IS)


############################################################
#                     UÇ DEĞER ANALİZİ                     #
############################################################

# Bağımlı değişken için IQR (Boxplot) yöntemi

lower <- quantile(x = temel_veri_baked$OV, probs = 0.25)
upper <- quantile(x = temel_veri_baked$OV, probs = 0.75)
iqr <- IQR(temel_veri_baked$OV)
ucdeger <- temel_veri_baked$OV < (lower-1.5*iqr) | temel_veri_baked$OV > (upper+1.5*iqr)
sum(ucdeger)
sum(ucdeger) /nrow(temel_veri_baked)

# Bağımsız değişkenler için extereme forest yöntemi
# Python kullanılmıştır.
# Kullanılacak condaenv içinde scikit-learn kurulmuş olmalıdır.
if (!require(reticulate)) install.packages("reticulate", 
                                           dependencies = TRUE)
# Eğer kurulu değilse mini conda kurulmalı
# veya use_condaenv() komutu ile mevcut bir conda ortamı seçilmeli
# install_miniconda() # bir kez çalıştıktan sonra comment olarakişaretlenmeli
# aşağıda gerekli python modüllerü kuruluyor
paketler <- readLines("pyFiles/requirements.txt")
conda_install(packages = paketler[!grepl(paketler,pattern = "^#")])

source_python("pyFiles/sigirOutlier.py")
outliers = Outlier(r_to_py(temel_veri_baked %>% select(-IK) %>% select_if(is.numeric)))
outliers$fit()
anomalies = outliers$predict() == -1

sum(anomalies)
sum(anomalies)/nrow(temel_veri_baked)

sum(ucdeger | anomalies)
sum(ucdeger | anomalies) / nrow(temel_veri_baked)
sum(ucdeger&anomalies)

# uç değerler siliniyor
newData <- temel_veri_baked[!(anomalies | ucdeger),]

introduce(newData  %>% select(-IK))


############################################################
#                   HİSTOGRAM GRAFİKLERİ                   #
############################################################


dt1 <- newData  %>% select_if(is.numeric)  %>% 
  select(-IK) %>% gather()

ggplot(dt1, aes(value, fill = key),color="darkgray") +
  geom_histogram(bins=30) +
  ylab("Frekans")+
  xlab("Değer")+
  theme_clean()+
  facet_wrap(~key, scales = 'free_x')+
  theme(legend.position = "none")


############################################################
#             JARQUE BERA TEST NORMALLİK TESTİ             #
############################################################

if (!require("tseries")) install.packages("tseries",
                                          dependencies = TRUE)

newData %>% select_if(is.numeric)  %>% select(-IK)  %>% 
  apply(2, function(x) print(jarque.bera.test(x)))


############################################################
#              ÇARPIKLIK VE EĞRİLİK DEĞERLERİ              #
############################################################

if (!require("psych")) install.packages("psych",
                                          dependencies = TRUE)

dt1 <- newData  %>% select_if(is.numeric)  %>%  
  select(-IK)  %>% describe()
print(dt1)


############################################################
#      TEMİZLENEN VERİ SETİ TANIMLAYICI İSTATİSTİKLER      #
############################################################

desc_temel <- skim(newData  %>% 
                     select(-c(IRK,SGS,IK,IBM))) %>% as_tibble()
tablo <- desc_temel  %>% filter(skim_type == "numeric")  %>%  
  select(O = 2,
         EGS = 3,
         BO = 4,
         ORT = 5,
         SS = 6,
         K0 = 7,
         K25 = 8,
         K50 = 9,
         K75 = 10,
         K100 = 11) 
print(tablo)

############################################################
#              IRK DEĞİŞKENİ FREKANS GRAFİĞİ               #
############################################################

frekans_plot(dt = newData, var = IRK,lab1 = "Sığır Irkı")

# az sayıda gözleme sahip ırklar 
# "diğer" adı altında birleştiriliyor

nrow(newData)*0.01
sel1 <- newData  %>% count(IRK)  %>% 
  filter(n > nrow(newData)*0.01)  %>% 
  select(IRK)  %>% 
  distinct()  %>%  unlist   %>% as.vector
newData <- newData  %>% 
  mutate(IRK=ifelse(IRK %in% sel1, as.character(IRK),"Diğer"))
newData <- newData  %>% 
  mutate(IRK = str_to_title(IRK,locale = "tr")) 

# IRK kategorileri birleştirildikten sonra

frekans_plot(dt = newData, var = IRK,lab1 = "Sığır Irkı")

############################################################
#      IRK DEĞİŞKENİ NORMALLİK VE DİĞER İSTATİSTİKLER      #
############################################################

normallik(x = newData, var = IRK)

############################################################
#    IRK DEĞİŞKENİ SINIFLAR ARASI VERİM FARKI TESTLERİ     #
############################################################

verim_permutasyon(x = newData,var = IRK,label = "Süt Sığırı Irkı")

box_plot(x = newData,var = IRK,label = "Süt Sığırı Irkı")

############################################################
#              SAĞIM SİSTEMİ FREKANS GRAFİĞİ               #
############################################################

frekans_plot(dt = newData, var = SGS, lab1 = "Sağım Sistemi")

############################################################
#      SGS DEĞİŞKENİ NORMALLİK VE DİĞER İSTATİSTİKLER      #
############################################################

normallik(x = newData, var = SGS)

############################################################
#    SGS DEĞİŞKENİ SINIFLAR ARASI VERİM FARKI TESTLERİ     #
############################################################

verim_permutasyon(x = newData,var = SGS,
                  label = "Sağım Sistemi")

box_plot(x = newData,var = SGS,
         label = "Sağım Sistemi")

############################################################
#          İLK BUZAĞILAMA MEVSİMİ FREKANS GRAFİĞİ          #
############################################################

frekans_plot(dt = newData, var = IBM, 
             lab1 = "İlk Buzağılama Mevsimi")

############################################################
#      IBM DEĞİŞKENİ NORMALLİK VE DİĞER İSTATİSTİKLER      #
############################################################

normallik(x = newData, var = IBM)

############################################################
#    IBM DEĞİŞKENİ SINIFLAR ARASI VERİM FARKI TESTLERİ     #
############################################################

verim_permutasyon(x = newData,var = IBM,
                  label = "İlk Buzağılama Mevsimi")

box_plot(x = newData,var = IBM,
         label = "İlk Buzağılama Mevsimi")

############################################################
#            TEMİZLENEN VERİNİN KAYIT EDİLMESİ             #
############################################################

# newData  %>%  write_rds("data/temel_veri_son.rds",
  # compress = "bz")
