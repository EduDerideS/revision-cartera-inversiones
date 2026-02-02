# R/3_indicadores.R
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(quantmod)
  library(TTR)
})

safe_last_num <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  tail(x, 1)
}

# Calcula indicadores "último valor" para un xts (un ticker)
calc_indicadores_xts <- function(x) {
  if (is.null(x) || NROW(x) < 210) {
    return(list(
      SMA  = NA_real_,  # SMA(200)
      SMA1 = NA_real_,  # SMA(50)
      SlowD = NA_real_,
      RSI = NA_real_
    ))
  }
  
  close <- Cl(x)
  
  sma200 <- SMA(close, n = 200)
  sma50  <- SMA(close, n = 50)
  rsi14  <- RSI(close, n = 14)
  
  st <- stoch(HLC(x), nFastK = 14, nFastD = 3, nSlowD = 3)
  slowD <- st[, "slowD"]
  
  list(
    SMA  = safe_last_num(sma200),
    SMA1 = safe_last_num(sma50),
    SlowD = safe_last_num(slowD),
    RSI = safe_last_num(rsi14)
  )
}

# Toma lista xts (por ticker) y devuelve tibble con indicadores por Nemotecnico
get_indicadores_from_prices_list <- function(prices_list) {
  tickers <- names(prices_list)
  
  purrr::map_dfr(tickers, function(tk) {
    x <- prices_list[[tk]]
    ind <- calc_indicadores_xts(x)
    
    tibble(
      Nemotecnico = tk,
      SMA  = ind$SMA,
      SMA1 = ind$SMA1,
      SlowD = ind$SlowD,
      RSI = ind$RSI
    )
  })
}