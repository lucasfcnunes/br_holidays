library(lubridate)
library(magrittr)
library(rlog)

get_br_holidays <- function(anos) {
  # https://github.com/MichelMeyer/scripts-e-funcoes-uteis-no-R/blob/master/feriados.R
  anos <- as.numeric(anos)
  if (!is.numeric(anos) | any(is.na(anos))) stop("A variável anos deve estar em formato numérico")

  anos <- sort(anos)

  if (!"rvest" %in% installed.packages()) install.packages("rvest")
  require(rvest)

  feriados <- lapply(anos, function(ano) {
    feriados <- read_html(paste0("https://www.anbima.com.br/feriados/fer_nacionais/", ano, ".asp"))
    feriados <- html_table(feriados, fill = T)
    feriados <- feriados[[4]]
    colnames(feriados) <- feriados[1,]
    feriados <- feriados[-1,]
    return(feriados)
  })

  feriados <- do.call(rbind, feriados)

  feriados$Data <- as.Date(feriados$Data, "%d/%m/%y")

  return(feriados)
}

br_holidays <- c()
br_holidays$file <- 'br_holidays.rds'
br_holidays$data <- tryCatch({
  rlog::log_info(paste0('Trying to load "', br_holidays$file, '"...'))
  readRDS(br_holidays$file)
},
error = function(err) {
  rlog::log_warn(paste0('"', br_holidays$file, '"not found. Downloading from web...'))
  br_holidays$data <- get_br_holidays(c(2001:2022))
  saveRDS(br_holidays$data, br_holidays$file)
  br_holidays$data
}
)
rlog::log_info(paste0('Loaded!'))

# >> functions

business_days <- function(date) {
  date <- as.Date(date)
  !(date %in% br_holidays$data$Data) & c(F, T, T, T, T, T, F)[wday(date)]
}

count_business_days <- function(date) {
  date %>% business_days %>% sum
}

business_days_in_month <- function(month) {
  month <- ymd(month, truncated = 1)
  args <- data.frame(from = floor_date(month, unit = "month"), to = ceiling_date(month, "month") - 1, by = "days")
  date <- mapply(seq, from = args$from, to = args$to, by = args$by)
  mapply(business_days, date)
}

count_business_days_in_month <- function(month) {
  mapply(
    sum,
    month %>% business_days_in_month
  )
}


# >> random utils

month2day_inflation <- function(month_inflation, relative = T) {
  month <- month_inflation$month
  inflation <- month_inflation$inflation
  if (relative) {
    inflation <- inflation + 1
  }
  `^`(inflation, 1 / (month %>% count_business_days_in_month))
}
