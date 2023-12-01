############################################################
#    MODELLERİN UYUM İYİLİĞİ KRİTERLERİNİN HESAPLANMASI    #
############################################################


# setwd("...YOL.../AMLMEAIMA")
# bazı yardımcı fonksiyonlar
source("source_functions.R")
options(OutDec = ".")
if (!require(reticulate)) {
  install.packages("reticulate",
    dependencies = TRUE
  )
}
if (!require(caret)) {
  install.packages("caret",
    dependencies = TRUE
  )
}
if (!require(MLmetrics)) {
  install.packages("MLmetrics",
    dependencies = TRUE
  )
}
use_miniconda("r-reticulate")
############################################################
#      YENİDEN ÖRNEKLEME İNDEKSLERİNİN OLUŞTURULMASI       #
############################################################
set.seed(2023)
data <- read_csv(file = "data/tum_data.csv") %>%
  mutate_if(is.character, factor)
egitimIndex <- read_rds(file = "data/indexEgitim.Rds")
egitim <- data[egitimIndex, ]
test <- data[-egitimIndex, ]
kfoldEgitim <- createFolds(egitim$OV, k = 5, list = FALSE)
kfoldTest <- createFolds(test$OV, k = 5, list = FALSE)

############################################################
#    ÖNCEDEN EĞİTİLEN MODELERİN UYUM İYİLİĞİ KRİTERLERİ    #
############################################################

models <- c("MLR", "DT", "KNN", "SVR", "RF", "MLP")
joblib <- import(module = "joblib")
scores <- NULL
for (i in models) {
  mod <- joblib$load(paste0("results/", i, "_mod.pkl"))
  for (j in 1:5) {
    preds <- mod$predict(X = egitim[kfoldEgitim == j, ])
    obs <- egitim$OV[kfoldEgitim == j]
    tmp <- rbind(
      c(i, "train", paste0("Blok", j), "R2", R2_Score(y_true = obs, y_pred = preds)),
      c(i, "train", paste0("Blok", j), "MAE", MAE(y_true = obs, y_pred = preds)),
      c(i, "train", paste0("Blok", j), "RMSE", RMSE(y_true = obs, y_pred = preds)),
      c(i, "train", paste0("Blok", j), "MAPE", MAPE(y_true = obs, y_pred = preds)),
      c(i, "train", paste0("Blok", j), "RMSPE", RMSPE(y_true = obs, y_pred = preds))
    )
    scores <- rbind(scores, as.data.frame(tmp))
    preds <- mod$predict(X = test[kfoldTest == j, ])
    obs <- test$OV[kfoldTest == j]
    tmp <- rbind(
      c(i, "test", paste0("Blok", j), "R2", R2_Score(y_true = obs, y_pred = preds)),
      c(i, "test", paste0("Blok", j), "MAE", MAE(y_true = obs, y_pred = preds)),
      c(i, "test", paste0("Blok", j), "RMSE", RMSE(y_true = obs, y_pred = preds)),
      c(i, "test", paste0("Blok", j), "MAPE", MAPE(y_true = obs, y_pred = preds)),
      c(i, "test", paste0("Blok", j), "RMSPE", RMSPE(y_true = obs, y_pred = preds))
    )
    scores <- rbind(scores, tmp)
  }
  cat("Model", i, "tamamlandı.\n")
}
colnames(scores) <- c("model", "type", "blok", "metric", "score")
scores <- scores %>% mutate(score = as.numeric(score))

############################################################
#            ELDE EDİLEN DEĞERLERİN SAKLANMASI             #
############################################################

write_csv(x = scores, file = "results/results.csv")
