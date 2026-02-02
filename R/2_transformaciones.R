# R/02_transformaciones.R
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(quantmod)
})

safe_last_num <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  tail(x, 1)
}

# Extrae el último precio de cierre (Close) por ticker desde lista xts
# Devuelve tibble: Nemotecnico, current_price
get_last_close_prices <- function(prices_list) {
  tickers <- names(prices_list)
  
  purrr::map_dfr(tickers, function(tk) {
    x <- prices_list[[tk]]
    if (is.null(x) || NROW(x) == 0) {
      return(tibble(Nemotecnico = tk, current_price = NA_real_))
    }
    tibble(
      Nemotecnico = tk,
      current_price = safe_last_num(Cl(x))
    )
  })
}

# Consolida posiciones si tienes varias compras del mismo Nemotecnico:
# - quantity: suma
# - purchase_price: promedio ponderado por cantidad
# - dividendos_recibidos: suma
# - purchase_date: la más antigua (proxy para TIR simple)
consolidate_positions <- function(cartera) {
  cartera %>%
    group_by(Nemotecnico) %>%
    summarise(
      quantity = sum(quantity, na.rm = TRUE),
      purchase_price = ifelse(sum(quantity, na.rm = TRUE) > 0,
                              sum(purchase_price * quantity, na.rm = TRUE) / sum(quantity, na.rm = TRUE),
                              NA_real_),
      dividendos_recibidos = sum(dividendos_recibidos, na.rm = TRUE),
      purchase_date = suppressWarnings(min(purchase_date, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      purchase_date = ifelse(is.infinite(purchase_date), NA, purchase_date),
      purchase_date = as.Date(purchase_date, origin = "1970-01-01")
    )
}

# Calcula valores base (sin indicadores técnicos)
# Requiere current_price ya unido
calc_base_values <- function(cartera_consolidada_con_precio) {
  cartera_consolidada_con_precio %>%
    mutate(
      initial_value = purchase_price * quantity,
      current_value = current_price * quantity,
      ganancia_perdida = (current_value - initial_value) + dividendos_recibidos
    )
}