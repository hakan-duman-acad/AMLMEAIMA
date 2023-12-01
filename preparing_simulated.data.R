library(tidyverse)
library(faux)
library(reticulate)


library(lubridate)
setwd("/home/hakan/Nextcloud/git-tez/AMLMEAIMA")

temel_veri <- read_csv("../../araCalisma3/tik3-analiz/data/veri.csv") %>% 
  mutate_at(vars(c(dogum_tar,cikis_tar,ilk_tohumlama_tar,ilk_buzagilama_tar)),dmy)  %>% 
  rename(
    IK = il_kod,
    IRK = irk, 
    SGS = sagim_sis,
    TSS = top_sigir_say,
    IS = inek_say,
    TS = toh_say,
    OLS = ortLak_sur,
    BS = buzagilama_say,
    OV = ort_verim,
    SCN= suru_cikis_nedeni_yeni,
    TVLS = top_ver_lak_say,
    EYSV = en_yuk_sut_ver,
    EYLS =en_yuk_lak_sure,
    TSM = top_sut_mik,
    TLS = toplam_lak_say,
  )  %>% 
  distinct()  %>% 
  distinct(kulak_no,.keep_all = TRUE)  %>% 
  #   mutate(ISO = (IS/(TSS))*100)   %>% 
  filter(TSS>0,IS >0)  %>% 
  filter(!is.na(kulak_no))  %>% 
  mutate(IBA = factor(month(ilk_buzagilama_tar)),
         IBTA = interval(dogum_tar,ilk_buzagilama_tar)  %/% months(1),
         SOA = interval(dogum_tar,cikis_tar)  %/% months(1),
         VLO = TVLS/TLS,
         IBM = ifelse(IBA %in% c(12,1,2),"Kış",
                      ifelse(IBA %in% c(3:5),"İlkbahar",
                             ifelse(IBA %in% c(6:9),"Yaz","Sonbahar"))))  %>% 
  select(-IBA)  %>% 
  select(-c(baba_kulak_no,ana_kulak_no, öl_buz_say, dogum_yil,dogum_ay,kulak_no,
            ilk_dam_kul_yas,sur_om_ay,sur_om_gun,ilk_buz_ay,cikis_ay,cikis_yil,no,bolge,orijin,dogum_tar,cikis_tar,
            ilk_tohumlama_tar,ilk_buzagilama_tar,SCN,EYLS,TLS,TVLS,TSM,EYSV))  %>% 
  filter(!is.na(IK)) 
anah <- read_csv(file="../../araCalisma3/tik3-analiz/data//anahtar.csv")
sgs <- anah  %>% select(SağımSistemi)  %>%  
  na.omit  %>% unlist  %>% as.vector
sgs <- sgs  %>% str_split("\\.",simplify = TRUE)  %>% 
  as_tibble()
irk <- anah  %>% select(IRK)  %>%  
  na.omit  %>% unlist  %>% as.vector
irk <- irk %>% str_split("\\.",simplify = TRUE)  %>% 
  as_tibble()
temel_veri <- temel_veri  %>% left_join(sgs  %>% mutate(V1 = as.numeric(V1)),by = c("SGS" = "V1"))  %>% 
  mutate(SGS = V2)  %>%  select(-V2)
temel_veri <- temel_veri  %>% left_join(irk %>% mutate(V1 = as.numeric(V1)),by = c("IRK" = "V1"))  %>% 
  mutate(IRK = V2)  %>%  select(-V2)
temel_veri   <- temel_veri  %>%  select(-V3)
  fctrs <- c("IRK")
temel_veri <- temel_veri %>% mutate_at(vars(IK,ilce_kod,isl_no,IRK,SGS,IBM),as.factor) 
  
temel_veri <- temel_veri %>% mutate_at(vars(TSS,IS,TS,BS,OLS,IBTA,SOA),as.integer)
mns <- apply(temel_veri[,c(6:14)],2,mean,na.rm=TRUE)
sds <- apply(temel_veri[,c(6:14)],2,sd,na.rm=TRUE)
temel_veri[,c(6:14)] <- abs(temel_veri[,c(6:14)] + sapply(sds, function(x) rnorm(n = nrow(temel_veri),mean = 0,sd = x/5)))
temel_veri <- temel_veri %>% mutate_at(vars(TSS,IS,TS,BS,OLS,IBTA,SOA),round)
write_csv(temel_veri,file = "data/simTemel.csv")

sim_data <- sim_df(data = temel_veri,  missing = TRUE, dv = "OV", n = 50000) %>% 
  filter_all(all_vars(. > 0))

set.seed(2023)
library(h2o)
library(recipes)

dataRec <- recipe(SGS ~ TSS+IS+TS+BS+OLS+IBTA+SOA+VLO,temel_veri) %>%
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>%
  prep()

localH2O = h2o.init(nthreads = -1)
fulldata.hex <- as.h2o(bake(dataRec,temel_veri))
split <- h2o.splitFrame(data = fulldata.hex, ratios = 0.50)
train.hex <- split[[1]]
test.hex <- split[[2]]

y <- "SGS"
x <- setdiff(names(fulldata.hex), y)

aml <- h2o.automl(x = x,
                  y = y,
                  training_frame = train.hex,
                  max_models = 20, nfolds = 3,
                  seed = 2023)
m <- h2o.get_best_model(aml)
h2o.performance(model = m,newdata = train.hex)
h2o.performance(model = m,newdata = test.hex)

prd <- function(m = m,dt) {
  predict(m,as.h2o(dt)) %>% .[,("predict")] %>% 
    unlist() %>% as.vector()
}
sim_data <- sim_data %>% mutate_at(vars(TSS,IS,TS,BS,IBTA,SOA,OLS),as.integer) %>% 
  mutate(SGS = prd(m,.))
write_csv(sim_data,file = "data/sim.csv")


dataRec <- recipe(IRK ~ TSS+IS+TS+BS+OLS+IBTA+SOA+VLO,temel_veri) %>%
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>%
  prep()
fulldata.hex <- as.h2o(bake(dataRec,temel_veri))
split <- h2o.splitFrame(data = fulldata.hex, ratios = 0.50)
train.hex <- split[[1]]
test.hex <- split[[2]]
y <- "IRK"
x <- setdiff(names(fulldata.hex), y)

aml <- h2o.automl(x = x,
                  y = y,
                  training_frame = train.hex,
                  max_models = 10, nfolds = 3,
                  seed = 2023)
m <- h2o.get_best_model(aml)
h2o.performance(model = m,newdata = train.hex)
h2o.performance(model = m,newdata = test.hex)

sim_data <- sim_data %>% 
  mutate(IRK = prd(m,.))
write_csv(sim_data,file = "data/sim.csv")


dataRec <- recipe(IBM ~ TSS+IS+TS+BS+OLS+IBTA+SOA+VLO,temel_veri) %>%
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>%
  prep()
fulldata.hex <- as.h2o(bake(dataRec,temel_veri))
split <- h2o.splitFrame(data = fulldata.hex, ratios = 0.50)
train.hex <- split[[1]]
test.hex <- split[[2]]
y <- "IBM"
x <- setdiff(names(fulldata.hex), y)

aml <- h2o.automl(x = x,
                  y = y,
                  training_frame = train.hex,
                  max_models = 20, nfolds = 3,
                  seed = 2023)
m <- h2o.get_best_model(aml)
h2o.performance(model = m,newdata = train.hex)
h2o.performance(model = m,newdata = test.hex)

sim_data <- sim_data %>% 
  mutate(IBM = prd(m,.))
write_csv(sim_data,file = "data/sim.csv")


sim_data <- read_csv(file = "data/sim.csv")
dataRec <- recipe(IK ~ TSS+IS+TS+BS+OLS+IBTA+SOA+VLO,temel_veri) %>%
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>%
  prep()
h2o.init()
fulldata.hex <- as.h2o(bake(dataRec,temel_veri))
split <- h2o.splitFrame(data = fulldata.hex, ratios = 0.50)
train.hex <- split[[1]]
test.hex <- split[[2]]
y <- "IK"
x <- setdiff(names(fulldata.hex), y)

m <- h2o.deeplearning(x = x,
                 y = y,
                 hidden = c(50,100,75),
                 epochs = 1000,
                 reproducible = TRUE, validation_frame = test.hex,
                 stopping_metric = "AUTO", stopping_rounds = 3,
                 #balance_classes = TRUE,
                 training_frame = train.hex)



h2o.performance(model = m,newdata = train.hex)
h2o.performance(model = m,newdata = test.hex)

sim_data <- sim_data %>% 
  mutate(IK = prd(m,.))
write_csv(sim_data,file = "data/sim.csv")


  
meteo <- read_tsv("data/meteo.tsv")
meteo %>% mutate(deger = deger + rnorm(nrow(meteo),sd= deger*.3)) %>% 
  write_tsv("data/meteo.tsv")


yas <- read_tsv(file = "../../araCalisma3/tik3-analiz/data/yasIndex.tsv")
yas %>% pivot_longer(cols = 3:43,names_to = "degisken",values_to = "deger") %>% 
  mutate(deger = deger + rnorm(nrow(.),sd= deger*.3)) %>% 
  select(-il_adi) %>% 
  rename(IK = il_kod) %>% 
  write_tsv("data/yasIndex.tsv")

iller <- read_tsv("data/il_adlari.tsv")
bank <- read_tsv(file = "../../araCalisma3/tik3-analiz/data/bankacilik.tsv")
bank %>% mutate(deger = bdeger + ifelse(is.na(rnorm(nrow(.), sd= bdeger*.3)),
                                        bdeger*.01,rnorm(nrow(.), sd= bdeger*.3))) %>% 
  rename(IA = il_adi) %>% 
  left_join(iller) %>% 
  select(IK,bD,deger) %>% 
  write_tsv("data/bankacilik.tsv")  

read_tsv(file = "../../tez/tez-analiz/data/agri.tsv") %>% 
  rename(IA = province) %>%
  pivot_longer(cols = 2:29,names_to = "degisken",values_to = "deger") %>% 
  mutate(deger = deger + rnorm(nrow(.),sd= deger*.3)) %>% 
  left_join(iller) %>% 
  select(IK, IA, degisken, deger) %>% 
  write_tsv("data/caymer.tsv")
  


read_rds(file = "../../tez/tez-analiz/temiz-data/paz.Rds") %>% 
  mutate(PPZR = PPZR + rnorm(nrow(.),sd= PPZR*.3)) %>% 
  write_tsv("data/potansiyal_pazar.tsv")
