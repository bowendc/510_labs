# Script for recodeing ANES variables for 
# use in PUBG 510 Lab 3

anes20 <- timeseries_2020 |> mutate(welfare = case_when(
  V201314x>0 & V201314x < 997 ~ V201314x,
  TRUE ~ NA_real_),
  income = if_else(V202468x>0, V202468x, NA),
  age = if_else(V201507x>0 & V201507x<81, V201507x, NA),
  dem_therm = if_else(V201156>=0 & V201156<=100, V201156, NA),
  rep_therm = if_else(V201157>=0 & V201157<=100, V201157, NA),
  trans_therm = if_else(V202172>=0 & V202172<=100, V202172, NA), 
  pid7 = case_when(V201231x > 0 ~ V201231x,
                   V201231x == -8 ~ 4,
                   TRUE ~ NA_real_),
  pid3 = case_when(
    V201228 == 1 ~ "Democrat",
    V201228 == 2 ~ "Republican",
    V201228 == 3 | 
      V201228 == 5 | 
      V201228 == -9 | 
      V201228 == -8 |
      V201228 == 0 ~ "Independent",
    TRUE ~ NA),
  sex = if_else(V201600>0, V201600, NA),
  favor.vote.mail = case_when(
    V201354==1 ~ 1,
    V201354>1 ~ 0,
    V201354<1 ~ NA,
    TRUE ~ NA))

welfare_lbl <- c("Increase a lot", 
                 "Increase a little",
                 "Kept the same",
                 "Decrease a little",
                 "Decrease a lot")
pid7_lbl <- c("Strong Democrat", 
              "Not very strong Democrat",
              "Lean Democrat",
              "Independent",
              "Lean Republican",
              "Not very strong Republican",
              "Strong Republican")
sex_lbl <- c("Men", "Women")