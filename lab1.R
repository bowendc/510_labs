# this is a comment

# you could create a header for your files

# name: lab1.R
# author: Bowen
# date: Sep 7, 2023
# class: PUBG 510

library(tidyverse) # this function loads tidyverse package

x <- c(1,2,3,4,5,6,7,8,9,10)
x2 <- 1:10 
rm(x2)
mean(x)
median(x)
sd(x, na.rm = TRUE)

x.mean <- mean(x)

27 + x.mean^2
y <- x - x.mean

nj.counties <- read_csv("https://raw.githubusercontent.com/bowendc/510_labs/main/nj_poverty.csv")
mean(nj.counties$totalpop)

# using tidyverse syntax

nj.counties |> summarize(mean.pop = mean(totalpop),
                         median.pop = median(totalpop),
                         mean.inc = mean(medincome),
                         median.inc = median(medincome))

nj.counties.recode <- nj.counties |> mutate(pct.poverty = 100*(poverty.count / totalpop))
mean(nj.counties.recode$pct.poverty)
