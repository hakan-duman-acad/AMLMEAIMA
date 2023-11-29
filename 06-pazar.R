############################################################
#                POTANSİYEL PAZAR VERİLERİ                 #
############################################################

#setwd("...YOL.../AMLMEAIMA")
source("ortakfonks.R")
set.seed(2023)

############################################################
#                   VERİLERİN YÜKLENMESİ                   #
############################################################

# Kaynak kodların test edilmesi için sahte veriler paylaşılmıştır.
# Orijinal verilere aşağıdaki adresler üzerinden ulaşılabilirsiniz.
# https://biruni.tuik.gov.tr/medas/?kn=95&locale=tr
# https://www.kgm.gov.tr/SiteCollectionDocuments/KGMdocuments/
# Root/Uzakliklar/ilmesafe.xlsx

paz <- read_tsv(file = "data/potansiyal_pazar.tsv")
iller <- read_tsv(file = "data/il_adlari.tsv") 

############################################################
#                    EKSİK VERİ ANALİZİ                    #
############################################################

full <- paz 
introText(full  %>% select(-IK,-IA))

eksik_plot(full)

stopCluster(cl)
############################################################
#          HAZIRLANAN VERİ SETİNİN KAYIT EDİLMESİ          #
############################################################

write_rds(full,file = "data/paz.Rds", compress = "bz")

