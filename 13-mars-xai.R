source("source_functions")

joblib <- import(module = "joblib")
modMLP <- joblib$load("results/MLP_mod.pkl")
tumveri <- read_csv(file = "data/original_data.csv") %>%
    select(-IK, -OV) %>%
    mutate_if(is.character, as.factor)
preds <- modMLP$predict(X = r_to_py(tumveri))
tumveri <- tumveri %>% add_column(preds = preds %>% unlist() %>% as.vector())
modlm <- lm(formula = preds ~ ., data = tumveri)
summary(modlm)
resettest(modlm, power = 2:4)
jarque.bera.test(modlm$residuals)
bptest(modlm)
car::vif(modlm)


gname <- "Kontrol grafikleri (vekil model - çoklu doğrusal regresyon)"
fname <- "xai-lm"
fit <- modlm
a <- summary(fit)
diagplot <- autoplot(fit, geom = "tile") +
    theme_clean() +
    theme(
        axis.text = element_text(size = 4),
        axis.title = element_text(size = 8),
        title = element_text(size = 4)
    ) +
    ggtitle(label = "", subtitle = "")
i <- 1
for (plot in diagplot@plots) {
    if (i == 4) {
        plot <- plot + geom_vline(
            xintercept = c(
                3 * length(a$coefficients) / nrow(tumveri),
                2 * length(a$coefficients) / nrow(tumveri)
            ),
            color = "orange", linetype = "dotted", linewidth = .2
        ) +
            geom_hline(yintercept = c(-3, 3), color = "orange", linetype = "dashed", linewidth = .2) +
            scale_y_continuous(breaks = c(-5, -3, 0, 3, 5))
    }
    ggsave(plot,
        filename = paste0("./images/", paste0(fname), "-", letters[i], ".png"),
        width = 3,
        height = 3,
        device = "png", dpi = 600
    )
    i <- i + 1
}
bhat <- Boot(modlm)
a <- S(modlm, vcov. = vcov(bhat))

tablo <- tibble(
    o = rownames(cfs), Katsayı = cfs[, 1],
    sh = cfs[, 2], pn = cut(
        x = cfs[, 4],
        breaks = c(-.01, 0.001, 0.01, 0.05, 1), labels = c("***", "**", "*", "ns")
    )
)
tablo[1, 1] <- "Sabit Terim"
colnames(tablo) <- c("Terim", "Katsayı", "\\gls{SH}$^\\ddager$", "\\gls{p}")
aa <- ccf %>%
    as_tibble() %>%
    mutate(Terim = paste0(V1, V2), G = V4)
bb <- cf2 %>%
    as_tibble() %>%
    rename(Terim = V1, G = V2)
as <- aa %>%
    select(Terim, G) %>%
    rbind(bb %>% select(Terim, G))
tablo <- tablo %>% left_join(as)
tablo$G[is.na(tablo$G)] <- ""
tablo <- tablo %>%
    mutate(Terim = paste0(Terim, "  ($", G, "$)")) %>%
    select(-G)
tablo %>%
    kable(
        caption = "Temel veri setinde bulunan sürekli değişkenlere ait tanımlayıcı istatistikler",
        format.args = list(decimal.mark = ",", big.mark = "."),
        label = "tem_con_ist",
        escape = FALSE,
        # col.names = cnames,
        booktabs = TRUE,
        digits = 3,
        format = "latex"
    ) %>%
    kable_styling(latex_options = "striped") %>%
    clipr::write_clip(allow_non_interactive = TRUE)

set.seed(2023)
moddt <- rpart(formula = preds ~ ., data = tumveri, method = "anova", control = list(cp = 0.01))
R2(pred = predict(moddt, tumveri), obs = tumveri$preds)
summary(moddt)
fname <- "xai-dt"
Cairo::CairoPDF(
    file = paste0("./images/", fname, ".pdf"), # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 4
) # The height of the plot in inches
rpart.plot(moddt)
dev.off()

set.seed(2023)
modmars <- earth(formula = preds ~ ., data = tumveri, degree = 2)
print(summary(modmars, digits = 8, ))
# install.packages("ehaGoF")
library(ehaGoF)

a1 <- GoF(Observations = tumveri$preds, Predicts = predict(modlm, tumveri), nTermInAppr = 32)
a2 <- GoF(Observations = tumveri$preds, Predicts = predict(moddt, tumveri), nTermInAppr = 14)
a3 <- GoF(Observations = tumveri$preds, Predicts = predict(modmars, tumveri), nTermInAppr = 48)

tab <- a1 %>%
    left_join(a2, by = c("criterion" = "criterion")) %>%
    left_join(a3, by = c("criterion" = "criterion"))
tab %>%
    kable(
        caption = "Vekil modellere ait iyi uyum kriterleri tablosu",
        format.args = list(decimal.mark = ",", big.mark = "."),
        label = "xai-gof",
        escape = FALSE,
        booktabs = TRUE,
        digits = 2,
        format = "latex"
    ) %>%
    kable_styling(latex_options = "striped") %>%
    clipr::write_clip(allow_non_interactive = TRUE)
