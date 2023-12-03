library(tidyverse)

tumveri <- read_csv("data/tumveri.csv") %>%
  mutate_if(is.character, as.factor) %>%
  mutate(IK = as.factor(IK))

Mode <- function(x) {
  vector <- unique(x)
  vector[which.max(tabulate(match(x, vector)))]
}
nums <- tumveri %>%
  group_by(IK) %>%
  summarise_if(is.numeric, mean)
cats <- tumveri %>%
  group_by(IK) %>%
  summarise_if(is.factor, Mode)
nums %>%
  left_join(cats) %>%
  write_rds(".data/ilmedyan.rds", compress = "bz")
