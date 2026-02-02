# R/4_portfolio.R
# -----------------------------------------
# Responsabilidad: Portfolio (construcción cuadro_indicadores)
# - Lee cartera
# - Consolida posiciones (1 fila por activo)
# - Calcula dividendos con la NUEVA definición:
#     dividendos_recibidos = sum(dividendo_por_accion_en_periodo) * acciones_actuales
#     - si ticker chileno (.SN): convierte a CLP con FX_USDCLP = 900 (definido en 1_ data.R)
# - Descarga precios + indicadores
# - Calcula base + TIR mensual con dividendos
# -----------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
})

source("R/1_ data.R")
source("R/2_transformaciones.R")
source("R/3_indicadores.R")

# ------------------------------------------------------------
# TIR mensual vectorizada (sirve para mutate con columnas)
# ------------------------------------------------------------
irr_mensual_simple <- function(initial_value, final_value, purchase_date, end_date = Sys.Date()) {
  initial_value <- as.numeric(initial_value)
  final_value   <- as.numeric(final_value)
  purchase_date <- as.Date(purchase_date)
  
  n <- length(initial_value)
  out <- rep(NA_real_, n)
  
  months <- suppressWarnings(lubridate::time_length(lubridate::interval(purchase_date, end_date), "month"))
  
  valid <- !is.na(purchase_date) &
    !is.na(initial_value) & is.finite(initial_value) & initial_value > 0 &
    !is.na(final_value) & is.finite(final_value) &
    is.finite(months) & months > 0.1
  
  out[valid] <- (final_value[valid] / initial_value[valid])^(1 / months[valid]) - 1
  out
}

# ------------------------------------------------------------
# Construye EXACTAMENTE el cuadro_indicadores solicitado
# ------------------------------------------------------------
build_cuadro_indicadores <- function(cartera_path = "data/cartera.csv",
                                     lookback_years = 2,
                                     dividend_period_mode = c("since_purchase", "ytd", "last12m"),
                                     fx_usdclp = FX_USDCLP,
                                     use_cache = TRUE,
                                     refresh_cache = FALSE) {
  
  dividend_period_mode <- match.arg(dividend_period_mode)
  
  # 1) Leer cartera (transacciones / filas de compras)
  cartera_raw <- read_cartera(cartera_path)
  
  # 2) Consolidar por Nemotecnico (1 fila por activo)
  #    - Se asume que consolidate_positions deja:
  #        Nemotecnico, purchase_price (prom), quantity (acciones actuales), purchase_date (primera compra o efectiva)
  cartera_pos <- consolidate_positions(cartera_raw)
  
  # 2.1) Dividendos con NUEVA definición (acciones actuales)
  #      - Sobrescribe dividendos_recibidos (force=TRUE)
  cartera_pos <- add_dividends_to_positions(
    positions = cartera_pos,
    current_date = Sys.Date(),
    period_mode = dividend_period_mode,
    force = TRUE,
    fx_usdclp = fx_usdclp,
    cache_dir = "output/cache_dividends",
    use_cache = use_cache,
    refresh_cache = refresh_cache
  )
  
  # 3) Descargar precios (para indicadores y precio actual)
  prices_list <- download_prices_yahoo(
    tickers = cartera_pos$Nemotecnico,
    lookback_years = lookback_years,
    to = Sys.Date(),
    cache_dir = "output/cache_prices",
    use_cache = use_cache,
    refresh_cache = refresh_cache
  )
  
  # 4) Precio actual (último close)
  last_prices <- get_last_close_prices(prices_list)
  
  # 5) Indicadores técnicos (último valor)
  indic <- get_indicadores_from_prices_list(prices_list)
  
  # 6) Unir y calcular valores base + TIR mensual con dividendos
  base <- cartera_pos %>%
    left_join(last_prices, by = "Nemotecnico") %>%
    left_join(indic, by = "Nemotecnico") %>%
    calc_base_values() %>%
    mutate(
      TIR_mensual_con_dividendos = irr_mensual_simple(
        initial_value = initial_value,
        final_value   = current_value + dividendos_recibidos,
        purchase_date = purchase_date,
        end_date      = Sys.Date()
      )
    )
  
  # 7) Construcción EXACTA del output final con nombres pedidos
  cuadro_indicadores <- data.frame(
    Nemotecnico = base$Nemotecnico,
    purchase_price = base$purchase_price,
    current_price  = base$current_price,
    initial_value  = base$initial_value,
    current_value  = base$current_value,
    ganancia_perdida = base$ganancia_perdida,
    dividendos_recibidos = base$dividendos_recibidos,
    TIR_mensual_con_dividendos = base$TIR_mensual_con_dividendos,
    SMA  = base$SMA,    # SMA (200)
    SMA1 = base$SMA1,   # SMA (50)
    SlowD = base$SlowD,
    RSI = base$RSI
  )
  
  cuadro_indicadores
}