############################################################
#                  MODELLERİN EĞİTİLMESİ                   #
############################################################

setwd("...YOL.../AMLMEAIMA")
# bazı yardımcı fonksiyonlar
source("ortakfonks.R")

if (!require(reticulate)) install.packages("reticulate", 
                                           dependencies = TRUE)
# Eğer kurulu değilse mini conda kurulmalı
# veya use_condaenv() komutu ile mevcut bir conda ortamı seçilmeli
# install_miniconda() # bir kez çalıştıktan sonra comment olarak işaretlenmeli
# aşağıda gerekli python modüllerü kuruluyor
paketler <- readLines("pyFiles/requirements.txt")
conda_install(packages = paketler[!grepl(paketler,pattern = "^#")])

############################################################
#      VERİ SETİNİN EĞİTİM VE TEST SETLERİNE AYRILMASI     #
############################################################
data <- read_csv(file = "data/tum_data.csv")

if (!require(caret)) install.packages("caret", 
                                           dependencies = TRUE)
set.seed(2023)
egitimIndex <- createDataPartition(data$OV, 
                                   p = .75,groups = 20,list = FALSE) %>% 
  as.vector()
# sonradan kullanılmak üzere kayıt ediliyor
write_rds(x = egitimIndex, file = "data/indexEgitim.Rds",compress = "bz")

egitimSeti <- data[egitimIndex,]

############################################################
#             PYTHON KODUNUN R'DAN ÇAĞIRILMASI             #
############################################################

source_python("pyFiles/model_egitim.py")

egitim <- ModelEgitim(X = egitimSeti %>% select(-OV), 
                      y = egitimSeti %>% select(OV) %>% unlist(), 
                      modelName = "ALL",useParams = TRUE)
egitim$fit(cv=10)


