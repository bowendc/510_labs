library(tidyverse)
library(vtable)

nj.grad <- read_csv("https://raw.githubusercontent.com/bowendc/510_labs/main/nj_schools_grad_rate.csv",
                    name_repair = make.names)

class(nj.grad$Cohort.Count)

nj.grad <- nj.grad |> mutate(Graduation.Rate.Num = as.numeric(Graduation.Rate),
                             Cohort.Count.Num = as.numeric(Cohort.Count)) 

nj.grad.wide <- nj.grad |> pivot_wider(names_from = Student.Group,
                                       values_from = c(Graduation.Rate,
                                                      Cohort.Count,
                                                      Graduated,
                                                      Graduation.Rate.Num,
                                                      Cohort.Count.Num),
                                       names_repair = make.names)

nj.grad.wide |> select(starts_with(c("Graduation.Rate.Num", "Cohort.Count.Num"))) |>
  sumtable(out = "browser")

hs <- nj.grad.wide |> filter(School.Name != "District" & County.Name != "State")

hist1 <- ggplot(data = hs, mapping = aes(x = Graduation.Rate.Num_Total)) +
    geom_histogram(aes(y = 100*(..count.. / sum(..count..))), 
                   fill = "navy", binwidth = 5, alpha = .2) + 
    geom_histogram(aes(x = Graduation.Rate.Num_Economically.Disadvantaged.Students,
                      y = 100*(..count.. / sum(..count..))), 
                 fill = "gold", binwidth = 5, alpha = .2) + 
  labs(title = "2022 Adjusted Cohort Graduation Rate, NJ High Schools",
       x = "Graduation Rate",
       y = "Percentage") +
  theme_minimal()

hist1

density1 <- ggplot(data = hs, mapping = aes(x = Graduation.Rate.Num_Total)) +
  geom_density(aes(y = 100*(..count.. / sum(..count..))), 
                 fill = "navy", alpha = .2) + 
  geom_density(aes(x = Graduation.Rate.Num_Economically.Disadvantaged.Students,
                     y = 100*(..count.. / sum(..count..))), 
                 fill = "gold", alpha = .2) + 
  labs(title = "2022 Adjusted Cohort Graduation Rate, NJ High Schools",
       x = "Graduation Rate",
       y = "Percentage") +
  theme_minimal()

density1
