library(tidyverse)
library(ggpubr)
library(magrittr)
library(stargazer)
df <- read_csv("tanaka2015.csv")
sch_year <- df$修学年数 
ln_income <- df$`対数_年収（万円）`
income <- df$`年収（万円）`

fit <- lm(ln_income ~ sch_year, df)
stargazer(fit, type = "text", no.space = TRUE)
df %>% ggplot(aes(x = sch_year, y = ln_income)) +
  geom_point() +
  geom_abline(aes(intercept = 4.385, slope = 0.065), color="red") + 
  annotate(geom = "text", x = 11, y = 8, label = "y = 4.385 + 0.065x") + 
  theme_minimal()

fit2 <- lm(income ~ sch_year, df)
stargazer(fit2, type = "text", no.space = TRUE)
df %>% ggplot(aes(x = sch_year, y = income)) +
  geom_point() +
  geom_abline(aes(intercept = -56.893, slope =  23.151), color="red") + 
  annotate(geom = "text", x = 11, y = 1050, label = "y = -56.893 + 23.151x") + 
  theme_minimal()
