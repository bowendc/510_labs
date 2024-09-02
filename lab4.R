library(tidyverse)
library(haven)
library(tidymodels)

hibbs <- read_csv("https://raw.githubusercontent.com/bowendc/510_labs/main/hibbs_1960_2023.csv") 

ggplot(data = hibbs, aes(x = chng.lnrdipc.last, y = inc.voteshare)) +
    geom_point()

cor(x = hibbs$chng.lnrdipc.all, y = hibbs$inc.voteshare, 
      use = "pairwise.complete.obs")
cor(x = hibbs$chng.lnrdipc.early, y = hibbs$inc.voteshare, 
    use = "pairwise.complete.obs")
r.saved <- cor(x = hibbs$chng.lnrdipc.last, y = hibbs$inc.voteshare, 
    use = "pairwise.complete.obs")
r.saved2 <- r.saved^2

m1 <- lm(inc.voteshare ~ chng.lnrdipc.last, data = hibbs, na.action = na.exclude)
tidy(m1)
glance(m1)

m2 <- lm(inc.voteshare ~ chng.lnrdipc.early, data = hibbs, na.action = na.exclude)
tidy(m2)
glance(m2)

m3 <- lm(inc.voteshare ~ chng.lnrdipc.last + chng.lnrdipc.early + fatalities, data = hibbs, na.action = na.exclude)
tidy(m3)
glance(m3)

postest <- hibbs |> mutate(yhat.m3 = fitted(m3),
                           resid.m3 = resid(m3))
