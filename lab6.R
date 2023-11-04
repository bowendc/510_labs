library(tidyverse)
library(haven)
library(anesr)
library(tidymodels)
library(modelsummary)
library(marginaleffects)


data("timeseries_2020")
myurl <- paste0("https://raw.githubusercontent.com/bowendc/510_labs/main/", "lab3_recodes.R")
download.file(url = myurl, "lab3recodes.R")
source("lab3recodes.R")

anes20 <- anes20 |> mutate(sex_fct = factor(sex, labels = sex_lbl))

m1 <- lm(welfare ~ age + income + sex_fct + pid7, data = anes20)
summary(m1)
m2 <- lm(welfare ~ age+ income + sex_fct + factor(pid7), data = anes20)
summary(m2)

modelsummary(list(m1, m2), estimate = "{estimate}{stars}",
                            statistic = "({std.error})")

m3 <- lm(welfare ~ age + income + sex_fct*pid7, data = anes20)
tidy(m3)

mfx <- avg_slopes(m3, variables = "sex_fct", by = "pid7")

ggplot(data = mfx, aes(x = pid7)) +
      geom_line(aes(y = estimate)) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                  color = "grey", alpha = .3)
