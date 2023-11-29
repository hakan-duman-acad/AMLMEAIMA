############################################################
#               TARIMSAL FİNANSMAN KULLANIMI               #
############################################################

#setwd("...YOL.../AMLMEAIMA")
source("ortakfonks.R")
set.seed(2023)

############################################################
#                   VERİLERİN YÜKLENMESİ                   #
############################################################

# Kaynak kodların test edilmesi için sahte veriler paylaşılmıştır.
# Orijinal verilere aşağıdaki adres üzerinden ulaşılabilir.
# https://verisistemi.tbb.org.tr/index.php?/tbb/report%5C_bolgeler


bank <- read_tsv(file = "data/bankacilik.tsv")
iller <- read_tsv(file = "data/il_adlari.tsv") 


############################################################
#                  VERİLERİN DÜZENLENMESİ                  #
############################################################

bank <- bank   %>% 
  pivot_wider(names_from = c("bD"),
              values_from = "deger")%>% 
  left_join(iller)  
bank <- bank  %>% rename(
                         TKM ='Ticari Kuruluşlar Mevduatı',
                         IKT= 'İhtisas Kredileri/ Tarım',
                         IKM = 'İhtisas Kredileri/ Mesleki',
                         IKD ='İhtisas Kredileri/ Denizcilik',
                         IKTU ='İhtisas Kredileri/ Turizm',
                         IKDI = 'İhtisas Kredileri/ Diğer')  %>%  
  mutate(TKO = IKT/(IKT+IKM+IKD+IKTU+IKDI)*100,
         IK= as.integer(IK))

############################################################
#                    EKSİK VERİ ANALİZİ                    #
############################################################

full <- bank %>% 
  select(IK,IA, IKT, TKO)
introText(full  %>% select(-IK,-IA))

eksik_plot(full)

############################################################
#                    KORELASYON GRAFİĞİ                    #
############################################################

cor_plot(full)

stopCluster(cl)
############################################################
#          HAZIRLANAN VERİ SETİNİN KAYIT EDİLMESİ          #
############################################################

write_rds(full,file = "data/fin.Rds", compress = "bz")

