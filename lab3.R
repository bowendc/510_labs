library(tidyverse)
library(haven)
library(devtools)
library(knitr)

install_github("jamesmartherus/anesr")

library(anesr)

data("timeseries_2020")

myurl <- paste0("https://raw.githubusercontent.com/bowendc/510_labs/main/", "lab4_recodes.R")

download.file(url = myurl, "lab4recodes.R")

source("lab4recodes.R")

anes20 <- anes20 |> mutate(welfare_ord = ordered(welfare, labels = welfare_lbl),
                           sex_fct = factor(sex, labels = sex_lbl))

ct1 <- table(anes20$welfare_ord, anes20$sex_fct)
round(prop.table(ct1, 2) * 100, digits = 1)

kable(prop.table(ct1, 2)*100, align = "lcc",
      format = "simple",
      digits = 1,
      caption = "Opinions about welfare spending by sex of respondent")


mc <- anes20 |> filter(!is.na(sex_fct)) |>
                group_by(sex_fct) |>
                summarize(Democratic = mean(dem_therm, na.rm=TRUE),
                          Republican = mean(rep_therm, na.rm=TRUE))

mc

write.csv(mc, file="lab4.mc.csv")


# scatterplot!

plot <- ggplot(data = anes20 |> filter(!is.na(pid3)), mapping = aes(x = age, y = trans_therm)) +
              geom_jitter(size = 2,
                          width = 2,
                          height = 2,
                          fill = "black",
                          alpha = .03,
                          stroke = 0) +
              labs(y = "Thermometer Rating: Transgender People",
                   x = "Age") +
              theme_minimal()
plot

plot + geom_smooth(method = "loess", se = FALSE)

plot + geom_smooth(method = "lm", se = FALSE, 
                   color = "black", linetype = "longdash",
                   linewidth = 1) + facet_grid(~pid3)
