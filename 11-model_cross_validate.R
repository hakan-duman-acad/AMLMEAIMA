############################################################
#                 ÇAPRAZ DOĞRULAMA ANALİZİ                 #
############################################################

# setwd("...YOL...")
# bazı yardımcı fonksiyonlar
source("source_functions.R")

if (!require("rcompanion")) {
  install.packages("rcompanion",
    dependencies = TRUE
  )
}
if (!require("coin")) {
  install.packages("coin",
    dependencies = TRUE
  )
}
data <- read_csv(file = "results/results.csv") %>%
  mutate_if(is.character, as.factor)

str <- NULL
smetric <- "RMSE"
cvTest <- function(mod) {
  t_test <- function(data) {
    tryCatch(shap2 <- shapiro.test(data$score),
      error = function(e) {
        message("Hata mesajı:\n", e)
        shap2 <- NULL
        shap2$p.value <- 0
        shap2$statistics <- "hata"
        shap2$parameter <- "hata"
      },
      warning = function(w) {
        message("Uyarı mesajı:\n", w)
      }
    )
    lev <- car::leveneTest(score ~ type, data = data)
    var.eq <- ifelse(lev$`Pr(>F)`[1] > 0.05, TRUE, FALSE)
    if (shap2$p.value > 0.05) {
      tt <- t.test(score ~ type, data = data, var.equal = var.eq)
      tst <- "t.test"
    } else {
      tt <- oneway_test(score ~ type,
        data = data,
        distribution = approximate(nresample = 10000)
      )
      tst <- "perm"
    }
    return(list(norm = shap2, var = lev, test = tt, tye = tst))
  }
  modData <- data %>% filter(metric == smetric, model == mod)
  result <- t_test(modData)
  lev <- sprintf(
    "Levene Testi[F(%s) = %s, p = %.3f] homojenite varsayımı %s.",
    paste(result$var$Df, collapse = ", "),
    fm(result$var$`F value`[1]),
    result$var$`Pr(>F)`[1],
    ifelse(result$var$`Pr(>F)`[1] > 0.05, "sağlanıyor", "sağlanmıyor")
  )
  shap <- sprintf(
    "Shapiro Wilk Testi [W = %s, p = %.3f] normallik koşulu %s.",
    fm(result$norm$statistic),
    result$norm$p.value,
    ifelse(result$norm$p.value > 0.05, "sağlanıyor", "sağlanmıyor")
  )
  if (result$tye == "perm") {
    result$tye <- result$test@method
    sta <- fm(result$test@statistic@teststatistic)
    test <- result$test
  } else {
    sta <- fm(result$test$statistic)
    para <- as.character(result$test$parameter)
    pval <- result$test$p.value
    durum <- ifelse(result$test$p.value > 0.05, "yok", "var")
    test <- sprintf(
      "%s [t(%s) = %s, p = %.3f] aşırı veya az uyum problemi %s.",
      result$tye, para, sta, pval, durum
    )
  }
  testtype <- sprintf("Uygulanan test : %s", result$tye)
  cat(paste0(rep("*", 50)), "\n")
  cat("Model :", mod, "\n")
  cat(shap, "\n")
  cat(lev, "\n")
  cat(testtype, "\n")
  print(test)
  cat(paste0(rep("-", 50)), "\n\n")
}

models <- c("MLR", "DT", "KNN", "SVR", "RF", "MLP")

for (i in models) {
  cvTest(i)
}
