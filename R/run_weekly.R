# R/run_weekly.R
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(knitr)
})

source("R/4_portfolio.R")

dir.create("output", showWarnings = FALSE)
dir.create("logs", showWarnings = FALSE)

fecha <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path("logs", paste0("runlog_", fecha, ".txt"))
sink(log_file, split = TRUE)

cat("=== RUN SEMANAL CARTERA ===\n")
cat("Timestamp:", as.character(Sys.time()), "\n\n")

cuadro_indicadores <- build_cuadro_indicadores("data/cartera.csv")

# 1) Redondeo numérico a 2 decimales (mantiene números)
cuadro_indicadores_2d <- cuadro_indicadores %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# 2) Guardar CSV (con números ya redondeados)
out_csv <- file.path("output", paste0("cuadro_indicadores_", fecha, ".csv"))
write_csv(cuadro_indicadores_2d, out_csv)

cat("Archivo generado:", out_csv, "\n")
cat("Filas:", nrow(cuadro_indicadores_2d), " | Columnas:", ncol(cuadro_indicadores_2d), "\n\n")

# 3) Tabla bonita en el log (formatea a 2 decimales visualmente)
#    OJO: kable imprime bien en log y en Rmd; en consola también sirve.
tabla_print <- cuadro_indicadores_2d %>%
  mutate(across(where(is.numeric), ~ formatC(.x, format = "f", digits = 2)))

cat("\n--- Tabla (2 decimales) ---\n")
print(knitr::kable(tabla_print, align = "l"))

sink()