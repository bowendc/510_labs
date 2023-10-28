library(tidyverse)
library(haven)
library(knitr)
library(tidymodels)
library(modelsummary)
library(anesr)

data("timeseries_2020")
myurl <- paste0("https://raw.githubusercontent.com/bowendc/510_labs/main/", "lab3_recodes.R")
download.file(url = myurl, "lab3recodes.R")

source("lab3recodes.R")

anes20 <- anes20 |> mutate(welfare_ord = ordered(welfare, labels = welfare_lbl),
                           sex_fct = factor(sex, labels = sex_lbl))

ct1 <- table(anes20$welfare_ord, anes20$sex_fct)
chisq.test(ct1)

mc <- anes20 |> filter(!is.na(sex_fct)) |>
                group_by(sex_fct) |>
                summarize(Democratic = mean(dem_therm, na.rm=TRUE),
                          Republican = mean(rep_therm, na.rm=TRUE))
mc

wom_dem <- anes20 |> filter(sex_fct=="Women") |> select(dem_therm)
men_dem <- anes20 |> filter(sex_fct=="Men") |> select(dem_therm)
wom_rep <- anes20 |> filter(sex_fct=="Women") |> select(rep_therm)
men_rep <- anes20 |> filter(sex_fct=="Men") |> select(rep_therm)

t.test(wom_dem, men_dem, mu = 0)
t.test(wom_rep, men_rep, mu = 0)

vbm <- anes20 |> select(favor.vote.mail, pid3) |>
              filter(!is.na(pid3) & 
                      !is.na(favor.vote.mail) &
                     pid3 != "Independent")

prop.test(table(vbm$pid3, vbm$favor.vote.mail))

m1 <- lm(welfare ~ age + income + pid7, data = anes20)
tidy(m1)
modelsummary(m1, estimate = "{estimate}{stars}",
                 statistic = "se = {std.error} t = {statistic} [{conf.low}, {conf.high}]")
