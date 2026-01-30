# install.packages(c("tidyverse", "lubridate"))
library(tidyverse)
library(lubridate)

# --- Parámetros de planificación (ajusta fechas a tu realidad) ---
inicio_proyecto <- as.Date("2026-01-01")  # Lunes recomendado

plan <- tribble(
  ~id, ~fase, ~tarea, ~inicio, ~fin, ~responsable,
  "A1", "Setup", "Estructura repo + carpetas",         inicio_proyecto,                 inicio_proyecto + days(1), "Eduardo",
  "A2", "Setup", "Config cartera (CSV/YAML) + umbrales", inicio_proyecto + days(2),       inicio_proyecto + days(3), "Eduardo",
  
  "B1", "Datos", "Descarga precios Yahoo Finance",     inicio_proyecto + days(4),       inicio_proyecto + days(6), "Eduardo",
  "B2", "Datos", "Limpieza/normalización series",      inicio_proyecto + days(7),       inicio_proyecto + days(8), "Eduardo",
  
  "C1", "Métricas", "Valor actual + P/L",              inicio_proyecto + days(9),       inicio_proyecto + days(10), "Eduardo",
  "C2", "Métricas", "Dividendos + TIR mensual",        inicio_proyecto + days(11),      inicio_proyecto + days(13), "Eduardo",
  "C3", "Métricas", "Indicadores (SMA/RSI/SlowD)",     inicio_proyecto + days(14),      inicio_proyecto + days(16), "Eduardo",
  
  "D1", "MVP", "Dataframe final (estructura objetivo)", inicio_proyecto + days(17),     inicio_proyecto + days(18), "Eduardo",
  "D2", "MVP", "Export outputs (CSV/RDS) + logs",      inicio_proyecto + days(19),      inicio_proyecto + days(20), "Eduardo",
  "D3", "MVP", "Automatización semanal (cron)",        inicio_proyecto + days(21),      inicio_proyecto + days(22), "Eduardo",
  
  "E1", "Docs", "README + supuestos + cómo ejecutar",  inicio_proyecto + days(23),      inicio_proyecto + days(24), "Eduardo",
  "E2", "Docs", "Diagrama pipeline (Mermaid/Visio)",   inicio_proyecto + days(25),      inicio_proyecto + days(26), "Eduardo",
  "E3", "Docs", "Model card (Vetiver-style) + QA",     inicio_proyecto + days(27),      inicio_proyecto + days(28), "Eduardo",
  
  "F1", "Alertas", "Telegram: envío básico con httr2", inicio_proyecto + days(29),      inicio_proyecto + days(30), "Eduardo",
  "F2", "Alertas", "Umbrales + reglas + formato msg",  inicio_proyecto + days(31),      inicio_proyecto + days(33), "Eduardo",
  "F3", "Alertas", "Pruebas end-to-end",               inicio_proyecto + days(34),      inicio_proyecto + days(35), "Eduardo"
) %>%
  mutate(
    tarea = paste0(id, " • ", tarea),
    fase = factor(fase, levels = c("Setup","Datos","Métricas","MVP","Docs","Alertas"))
  )

# --- Gantt con ggplot ---
gantt <- ggplot(plan, aes(y = fct_rev(tarea))) +
  geom_segment(aes(x = inicio, xend = fin, yend = fct_rev(tarea)), linewidth = 6) +
  facet_grid(fase ~ ., scales = "free_y", space = "free_y") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b") +
  labs(
    title = "Carta Gantt — Revisión Cartera de Inversiones",
    x = "Fecha",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(angle = 0),
    plot.title = element_text(face = "bold")
  )

print(gantt)

# Guardar imagen
ggsave("docs/gantt_cartera.png", gantt, width = 12, height = 8, dpi = 200)

# Guardar plan como CSV para versionarlo
write_csv(plan, "docs/planificacion_gantt.csv")