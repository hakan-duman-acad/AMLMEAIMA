if (!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
if (!require(knitr)) install.packages("knitr", dependencies = TRUE)
if (!require(kableExtra)) install.packages("kableExtra", dependencies = TRUE)
if (!require(reticulate)) install.packages("reticulate", dependencies = TRUE)
if (!require(doParallel)) install.packages("doParallel", dependencies = TRUE)
if (!require(DALEX)) install.packages("DALEX", dependencies = TRUE)
if (!require(DALEXtra)) install.packages("DALEXtra", dependencies = TRUE)
if (!require(tictoc)) install.packages("tictoc", dependencies = TRUE)
if (!require(patchwork)) install.packages("patchwork", dependencies = TRUE)
if (!require(lime)) install.packages("lime", dependencies = TRUE)
if (!require(clipr)) install.packages("clipr", dependencies = TRUE)
model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer
data <- read_csv(file = "data/tumveri.csv") %>%
  mutate_if(is.character, as.factor)
iller <- read_tsv(file = "data/il_adi.tsv")
data2 <- read_rds(file = "data/ilmedyan.rds")
data2 <- data2 %>%
  mutate(IK = as.numeric(as.character(IK))) %>%
  left_join(iller, by = c("IK" = "il_kod"))
data2 <- data2 %>%
  select(-adi) %>%
  select(il_adi, everything())
cl <- makePSOCKcluster(parallel::detectCores())
registerDoParallel(cl)

joblib <- import(module = "joblib")
dx <- import(module = "dalex")
mlp <- joblib$load("results/MLP_mod.pkl")
mlr <- joblib$load("results/MLR_mod.pkl")

predFunc <- function(model, data) model$predict(data)
expMLP <- DALEX::explain(
  model = mlp, data = data, label = "MLP",
  y = data$OV,
  predict_function = predFunc
)

expMLR <- DALEX::explain(
  model = mlr, data = data, label = "MLR",
  y = data$OV,
  predict_function = predFunc
)

data2 <- data2 %>%
  add_column(preds = mlp$predict(data2)) %>%
  relocate(OV, .after = MET)

ilplot <- function(il, exp = expMLP, data = data2) {
  obs <- data %>%
    filter(IK == il) %>%
    select(-IK, -OV)
  ebd <- predict_parts(
    explainer = expMLP,
    new_observation = obs,
    type = "break_down_interactions",
    keep_distributions = TRUE
  )
  eshap <- predict_parts(
    explainer = expMLP, n_features = ncol(obs),
    new_observation = obs,
    type = "shap"
  )
  ecp <- predict_profile(
    explainer = expMLP,
    new_observation = obs
  )
  elime <- predict_surrogate(
    explainer = expMLP,
    new_observation = obs,
    n_features = ncol(obs),
    n_permutations = 5000,
    type = "lime"
  )
  pltbd <- plot(ebd) + ggtitle("")
  pltshap <- plot(eshap) + ggtitle("")
  pltcp <- plot(ecp) + ggtitle("")
  pltlime <- plot(elime) + ggtitle("") + xlab("") + ylab("")
  return(list(
    data = obs, bd = ebd, shap = eshap, cp = ecp, lime = elime,
    pltbd = pltbd, pltshap = pltshap, pltcp = pltcp, pltlime = pltlime,
    pltall = pltbd / pltshap / pltcp
  ))
}

for (i in data2 %>%
  select(IK) %>%
  distinct() %>%
  unlist() %>%
  as.vector()) {
  ilplots <- ilplot(il = i)
  print(ilplots$pltbd)
  ggsave(
    filename = paste0("./images/ilplot_bd_", i, ".pdf"),
    width = 6,
    height = 6,
    device = cairo_pdf
  )
  print(ilplots$pltshap)
  ggsave(
    filename = paste0("./images/ilplot_shap_", i, ".pdf"),
    width = 6,
    height = 6,
    device = cairo_pdf
  )
  print(ilplots$pltcp)
  ggsave(
    filename = paste0("./images/ilplot_cp_", i, ".pdf"),
    width = 6,
    height = 6,
    device = cairo_pdf
  )
  print(ilplots$pltlime)
  ggsave(
    filename = paste0("./images/ilplot_lime_", i, ".pdf"),
    width = 6,
    height = 6,
    device = cairo_pdf
  )
  cat(i, "tamamlandı.\n")
}
