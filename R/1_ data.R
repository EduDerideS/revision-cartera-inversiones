# R/1_ data.R  (aka "data.R")
# -----------------------------------------
# Responsabilidad: Data
# - Leer cartera desde data/cartera.csv
# - Descargar precios desde Yahoo (con caché)
# - Descargar dividendos desde Yahoo (con caché)
#
# DIVIDENDOS (definición de negocio solicitada):
#   "Dividendos reales recibidos" = sum(dividendo_por_accion_en_periodo) * acciones_actuales
#   - Para acciones chilenas: convertir a CLP usando tipo de cambio fijo 900 CLP/USD
#     (TC fijo explícito: FX_USDCLP = 900)
# -----------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(quantmod)
})

# -----------------------------
# Parámetros globales del módulo
# -----------------------------
FX_USDCLP <- 900  # Tipo de cambio fijo (CLP por USD) para acciones chilenas (según requerimiento)

# Heurística simple para identificar tickers chilenos en Yahoo:
# - Muchos tickers de Chile terminan en ".SN" (Bolsa de Santiago en Yahoo)
# Ajusta aquí si usas otro sufijo.
is_chilean_ticker <- function(ticker) {
  ticker <- as.character(ticker)
  grepl("\\.SN$", ticker, ignore.case = TRUE)
}

# -----------------------------
# 1) Leer cartera (input)
# -----------------------------
read_cartera <- function(path = "data/cartera.csv") {
  cartera <- read_csv(path, show_col_types = FALSE)
  
  # Obligatorias
  if (!"Nemotecnico" %in% names(cartera)) stop("Falta columna obligatoria: Nemotecnico")
  if (!"purchase_price" %in% names(cartera)) stop("Falta columna obligatoria: purchase_price")
  if (!"quantity" %in% names(cartera)) stop("Falta columna obligatoria: quantity")
  
  # Opcionales (si faltan, las creamos)
  if (!"dividendos_recibidos" %in% names(cartera)) cartera$dividendos_recibidos <- NA_real_
  if (!"purchase_date" %in% names(cartera)) cartera$purchase_date <- NA_character_
  
  cartera %>%
    mutate(
      Nemotecnico = as.character(Nemotecnico),
      purchase_price = as.numeric(purchase_price),
      quantity = as.numeric(quantity),
      dividendos_recibidos = as.numeric(dividendos_recibidos),
      purchase_date = suppressWarnings(as.Date(purchase_date))
    )
}

# -----------------------------
# 2) Descargar precios Yahoo (con caché)
# -----------------------------
download_prices_yahoo <- function(tickers,
                                  lookback_years = 2,
                                  to = Sys.Date(),
                                  cache_dir = "output/cache_prices",
                                  use_cache = TRUE,
                                  refresh_cache = FALSE) {
  suppressWarnings(dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE))
  
  from <- to - lubridate::years(lookback_years)
  tickers <- unique(na.omit(tickers))
  
  cache_file <- function(tk) {
    file.path(cache_dir, paste0(gsub("[^A-Za-z0-9_.-]", "_", tk), ".rds"))
  }
  
  prices_list <- purrr::map(tickers, function(tk) {
    cf <- cache_file(tk)
    
    # 1) usar caché si existe y no se pidió refresh
    if (use_cache && !refresh_cache && file.exists(cf)) {
      x_cached <- tryCatch(readRDS(cf), error = function(e) NULL)
      if (!is.null(x_cached) && NROW(x_cached) > 0) return(x_cached)
    }
    
    # 2) descargar desde Yahoo
    x <- tryCatch(
      quantmod::getSymbols(
        Symbols = tk, src = "yahoo",
        from = from, to = to,
        auto.assign = FALSE, warnings = FALSE
      ),
      error = function(e) NULL
    )
    
    # 3) guardar caché si ok
    if (!is.null(x) && NROW(x) > 0) {
      if (use_cache) tryCatch(saveRDS(x, cf), error = function(e) NULL)
      return(x)
    }
    
    # 4) fallback: si falló Yahoo pero hay caché, úsalo
    if (use_cache && file.exists(cf)) {
      x_cached <- tryCatch(readRDS(cf), error = function(e) NULL)
      if (!is.null(x_cached) && NROW(x_cached) > 0) return(x_cached)
    }
    
    NULL
  })
  
  names(prices_list) <- tickers
  prices_list
}

# -----------------------------
# 3) Dividendos Yahoo (con caché)
#    Definición: sum(div_ps) * acciones_actuales
#    - Permite elegir período:
#        mode = "since_purchase" (desde primera compra)
#        mode = "ytd"            (año calendario)
#        mode = "last12m"        (últimos 12 meses)
# -----------------------------
dividends_period_start <- function(purchase_date, mode = c("since_purchase", "ytd", "last12m"), current_date = Sys.Date()) {
  mode <- match.arg(mode)
  current_date <- as.Date(current_date)
  
  if (mode == "since_purchase") {
    pd <- suppressWarnings(as.Date(purchase_date))
    if (is.na(pd)) return(NA_Date_)
    return(pd)
  }
  
  if (mode == "ytd") {
    return(as.Date(paste0(format(current_date, "%Y"), "-01-01")))
  }
  
  if (mode == "last12m") {
    return(current_date - 365)
  }
  
  NA_Date_
}

get_dividends_total_yahoo <- function(ticker_yahoo,
                                      period_start,
                                      current_date = Sys.Date(),
                                      shares_current = 0,
                                      fx_usdclp = FX_USDCLP,
                                      cache_dir = "output/cache_dividends",
                                      use_cache = TRUE,
                                      refresh_cache = FALSE) {
  suppressWarnings(dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE))
  
  period_start <- as.Date(period_start)
  current_date <- as.Date(current_date)
  
  # si no hay fecha o shares no válido, no podemos calcular
  shares_current <- as.numeric(shares_current)
  if (is.na(period_start) || !is.finite(shares_current) || shares_current <= 0) return(0)
  
  # Caché por (ticker + periodo + fecha fin + shares actuales + FX)
  # (porque tu definición depende explícitamente de shares actuales)
  cf <- file.path(
    cache_dir,
    paste0(
      gsub("[^A-Za-z0-9_.-]", "_", ticker_yahoo), "__",
      format(period_start, "%Y%m%d"), "__to__", format(current_date, "%Y%m%d"), "__",
      "sh", shares_current, "__fx", fx_usdclp, ".rds"
    )
  )
  
  # 1) usar caché
  if (use_cache && !refresh_cache && file.exists(cf)) {
    cached <- tryCatch(readRDS(cf), error = function(e) NULL)
    if (!is.null(cached) && is.numeric(cached) && length(cached) == 1) return(cached)
  }
  
  # 2) descargar dividendos por acción desde Yahoo
  div_ps <- tryCatch(
    quantmod::getDividends(
      ticker_yahoo, src = "yahoo",
      from = period_start, to = current_date,
      auto.assign = FALSE
    ),
    error = function(e) NULL
  )
  
  # 3) fallback a caché o 0
  if (is.null(div_ps)) {
    if (use_cache && file.exists(cf)) {
      cached <- tryCatch(readRDS(cf), error = function(e) NULL)
      if (!is.null(cached) && is.numeric(cached) && length(cached) == 1) return(cached)
    }
    return(0)
  }
  
  div_ps <- na.omit(div_ps)
  div_per_share_sum <- if (NROW(div_ps) == 0) 0 else sum(as.numeric(div_ps), na.rm = TRUE)
  
  # 4) Total "recibido" según acciones actuales
  dividends_total <- div_per_share_sum * shares_current
  
  # 5) Conversión explícita para acciones chilenas a CLP usando TC fijo 900
  #    (según requerimiento del proyecto)
  if (is_chilean_ticker(ticker_yahoo)) {
    dividends_total <- dividends_total * fx_usdclp
  }
  
  if (use_cache) {
    tryCatch(saveRDS(dividends_total, cf), error = function(e) NULL)
  }
  
  dividends_total
}

# -----------------------------
# 4) Enriquecer cartera con dividendos Yahoo
#    IMPORTANTE: como tu definición usa ACCIONES ACTUALES,
#    primero debes consolidar posiciones antes de llamar a esto,
#    o bien indicar explícitamente el "shares_current" por Nemotecnico.
#
#    Esta función asume que 'cartera' ya viene a nivel de posición actual:
#      1 fila por Nemotecnico con columnas:
#      - Nemotecnico
#      - purchase_date  (puede ser la primera compra o fecha efectiva)
#      - quantity       (acciones actuales)
# -----------------------------
add_dividends_to_positions <- function(positions,
                                       current_date = Sys.Date(),
                                       period_mode = c("since_purchase", "ytd", "last12m"),
                                       force = TRUE,
                                       fx_usdclp = FX_USDCLP,
                                       cache_dir = "output/cache_dividends",
                                       use_cache = TRUE,
                                       refresh_cache = FALSE) {
  
  period_mode <- match.arg(period_mode)
  
  needed <- c("Nemotecnico", "purchase_date", "quantity")
  miss <- setdiff(needed, names(positions))
  if (length(miss) > 0) stop("Faltan columnas para dividendos (positions): ", paste(miss, collapse = ", "))
  
  if (!"dividendos_recibidos" %in% names(positions)) {
    positions$dividendos_recibidos <- NA_real_
  }
  
  # Definir inicio del período según mode
  period_start_vec <- purrr::map_chr(
    positions$purchase_date,
    ~ as.character(dividends_period_start(.x, mode = period_mode, current_date = current_date))
  )
  period_start_vec <- as.Date(period_start_vec)
  
  div_yahoo <- purrr::pmap_dbl(
    list(positions$Nemotecnico, period_start_vec, positions$quantity),
    function(tk, pstart, sh_current) {
      get_dividends_total_yahoo(
        ticker_yahoo = tk,
        period_start = pstart,
        current_date = current_date,
        shares_current = sh_current,
        fx_usdclp = fx_usdclp,
        cache_dir = cache_dir,
        use_cache = use_cache,
        refresh_cache = refresh_cache
      )
    }
  )
  
  positions <- positions %>%
    mutate(dividendos_yahoo = div_yahoo)
  
  # Con force=TRUE (recomendado para consistencia del reporte):
  # siempre usamos el cálculo Yahoo (con acciones actuales)
  if (force) {
    positions <- positions %>% mutate(dividendos_recibidos = dividendos_yahoo)
  } else {
    positions <- positions %>%
      mutate(dividendos_recibidos = ifelse(is.na(dividendos_recibidos),
                                           dividendos_yahoo,
                                           dividendos_recibidos))
  }
  
  positions
}