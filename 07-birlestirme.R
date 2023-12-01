############################################################
#              VERİ SETLERİNİN BİRLEŞTİRİLMESİ             #
############################################################
source("source_functions.R")
if (!require(tidyverse)) {
  install.packages("tidyverse",
    dependencies = TRUE
  )
}
tm <- read_rds("data/temel_veri_son.rds")
mer <- read_rds(file = "data/mer.Rds") %>% select(-IA)
paz <- read_rds(file = "data/paz.Rds") %>% select(-IA)
ysi <- read_rds(file = "data/ysi.Rds")
fin <- read_rds(file = "data/fin.Rds")
met <- read_rds(file = "data/met.Rds")

tumveri <- tm %>%
  left_join(mer) %>%
  left_join(paz) %>%
  left_join(ysi) %>%
  left_join(fin) %>%
  left_join(met)
introText(tumveri %>% select(-IK, -IA))
write.csv(tumveri %>% select(-IK, IA),
  file = "data/tum_data.csv",
  row.names = FALSE
) #
