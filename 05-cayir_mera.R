############################################################
#               ÇAYIR MERA ALANLARI VERİLERİ               #
############################################################

# setwd("...YOL.../AMLMEAIMA")
set.seed(2023)
source("source_functions.R")



############################################################
#                   VERİLERİN YÜKLENMESİ                   #
############################################################

# Kaynak kodların test edilmesi için sahte veriler paylaşılmıştır.
# Orijinal verilere aşağıdaki adres üzerinden ulaşılabilirsiniz.
# https://data.tuik.gov.tr/Bulten/DownloadIstatistikselTablo?p=COz7eWH1mxNzz
# 09UnlgBeJaTYHImx7wV4EqEfmCoK4UzIp5MTaOh0wG1asIcd1wW


mera <- read_tsv(file = "data/caymer.tsv")
iller <- read_tsv(file = "data/il_adlari.tsv")


############################################################
#                  VERİLERİN DÜZENLENMESİ                  #
############################################################

mera <- mera %>%
  pivot_wider(
    names_from = c("degisken"),
    values_from = "deger"
  ) %>%
  left_join(iller)

############################################################
#                    EKSİK VERİ ANALİZİ                    #
############################################################

full <- mera <- mera %>%
  mutate(
    OTLO = PAS / (TARA + SEBA + MEYA) * 100,
    MERAO = MEAA / (TARA + SEBA + MEYA) * 100,
    IK = as.integer(IK)
  ) %>%
  rename(MERA = MEAA, OTL = PAS) %>%
  select(IK, IA, MERA, MERAO, OTL, OTLO)

introText(full %>% select(-IK, -IA))

eksik_plot(full)

############################################################
#                    KORELASYON GRAFİĞİ                    #
############################################################

cor_plot(full)

stopCluster(cl)
############################################################
#          HAZIRLANAN VERİ SETİNİN KAYIT EDİLMESİ          #
############################################################

write_rds(full, file = "data/mer.Rds", compress = "bz")
