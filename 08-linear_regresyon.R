############################################################
#                 ÇOKLU DOĞRUSAL REGRESYON                 #
############################################################

# setwd("...YOL.../AMLMEAIMA")
source("source_functions.R")


set.seed(2023)
require(recipes)
require(ggthemes)
tumveri <- read_csv(file = "data/tum_data.csv") %>%
  mutate_if(is.character, as.factor)

fit1Rec <- recipe(OV ~ IRK + SGS + TSS + TS + BS + OLS + IBM + VLO + SOA + IBTA,
  data = tumveri
) %>%
  prep()

fit2Rec <- recipe(
  OV ~ IRK + SGS + TSS + TS + BS + OLS + IBM + VLO + SOA + IBTA +
    YSI + MET + TKO + IKT + MERAO + OTLO + PPZR,
  data = tumveri
) %>%
  prep()

############################################################
#                       1. REGRESYON                       #
############################################################

fit1 <- lm(OV ~ ., data = bake(fit1Rec, tumveri))

# hata artıklarının normal dağılım kontrolü
if (!require(tseries)) {
  install.packages("tseries",
    dependencies = TRUE
  )
}
jarque.bera.test(fit1$residuals)

# homojenite koşulu kontrolü
if (!require(lmtest)) {
  install.packages("lmtest",
    dependencies = TRUE
  )
}
bptest(fit1)

# VIF değerleri
if (!require(car)) {
  install.packages("car",
    dependencies = TRUE
  )
}
car::vif(fit1)

# varsayım kontrol grafikleri
varsayim_plot(fit1)

############################################################
#                       2. REGRESYON                       #
############################################################

fit2 <- lm(OV ~ ., data = bake(fit2Rec, tumveri))

# hata artıklarının normal dağılım kontrolü

jarque.bera.test(fit2$residuals)

# homojenite koşulu kontrolü

bptest(fit2)

# VIF değerleri

car::vif(fit2)

# varsayım kontrol grafikleri
varsayim_plot(fit2)

############################################################
#          İKİ REGRESYON MODELİNİN KARŞILAŞTIRMASI         #
############################################################

if (!require(broom)) {
  install.packages("broom",
    dependencies = TRUE
  )
}
glance(fit1) %>% rbind(glance(fit2))

############################################################
#  1. REGRESYON MODELİ BOOTSTRAP STANDART HATA DEĞERLERİ   #
############################################################

bootstrap_se(fit1)

############################################################
#  2. REGRESYON MODELİ BOOTSTRAP STANDART HATA DEĞERLERİ   #
############################################################

bootstrap_se(fit2)
