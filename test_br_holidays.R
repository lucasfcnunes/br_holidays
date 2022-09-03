source("br_holidays.R")

test_business_days <- c("2022-01-01", "2022-01-02", "2022-01-03", "2022-01-04", "2022-01-05", "2022-02-28")
stopifnot(
    business_days(test_business_days) == c(F, F, T, T, T, F)
  )
stopifnot(
    count_business_days(test_business_days) == 3
  )
stopifnot(
    count_business_days_in_month(c("2022-02", "2022-03", "2022-01-02", "2020-05")) == c(19, 22, 21, 20)
  )

month_inflation <- read.csv("data/month_inflation.csv")
month2day_inflation(month_inflation)
