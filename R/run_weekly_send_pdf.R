# R/run_weekly_send_pdf.R
# ------------------------------------------------------------
# - Renderiza reporte PDF
# - Lo guarda en output/
# - Lo envía por Telegram (TELEGRAM_BOT_TOKEN + TELEGRAM_CHAT_ID)
# - Robusto ante cambios de working directory durante render()
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(rmarkdown)
  library(httr2)
})

# ---------- Helpers ----------
now_stamp <- function() format(Sys.time(), "%Y%m%d_%H%M%S")

get_env_or_stop <- function(name) {
  val <- Sys.getenv(name, unset = "")
  if (!nzchar(val)) stop("Falta variable de entorno: ", name)
  val
}

parse_chat_id_singular <- function(x) {
  ids <- trimws(unlist(strsplit(x, ",")))
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0) stop("TELEGRAM_CHAT_ID está vacío.")
  ids
}

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && nzchar(as.character(a))) a else b
}

telegram_send_document <- function(bot_token, chat_id, file_path, caption = NULL) {
  stopifnot(file.exists(file_path))
  
  url <- sprintf("https://api.telegram.org/bot%s/sendDocument", bot_token)
  
  req <- request(url) |>
    req_body_multipart(
      chat_id = chat_id,
      caption = caption %||% "",
      document = upload_file(file_path)
    )
  
  resp <- req_perform(req)
  body <- resp_body_json(resp, simplifyVector = TRUE)
  
  if (!isTRUE(body$ok)) {
    msg <- body$description %||% "Error desconocido Telegram"
    stop("Telegram sendDocument falló para chat_id=", chat_id, ": ", msg)
  }
  
  invisible(TRUE)
}

# ---------- Main ----------
main <- function() {
  # Raíz del proyecto = working dir desde donde ejecutas el script
  project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  
  output_dir <- file.path(project_root, "output")
  logs_dir   <- file.path(project_root, "logs")
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(logs_dir,   showWarnings = FALSE, recursive = TRUE)
  
  stamp <- now_stamp()
  log_file <- file.path(logs_dir, paste0("run_send_pdf_", stamp, ".log"))
  sink(log_file, split = TRUE)
  on.exit(sink(), add = TRUE)
  
  cat("=== RUN SEMANAL + ENVÍO PDF ===\n")
  cat("Timestamp:", as.character(Sys.time()), "\n")
  cat("Project root:", project_root, "\n\n")
  
  # 1) Secrets desde entorno
  bot_token <- get_env_or_stop("TELEGRAM_BOT_TOKEN")
  chat_ids  <- parse_chat_id_singular(get_env_or_stop("TELEGRAM_CHAT_ID"))
  
  # 2) Render PDF (rutas ABSOLUTAS)
  input_rmd <- file.path(project_root, "reports", "reporte_cliente_pdf.Rmd")
  if (!file.exists(input_rmd)) stop("No existe: ", input_rmd)
  
  out_pdf <- file.path(output_dir, paste0("informe_cartera_", stamp, ".pdf"))
  
  cat("Renderizando:\n - input:", input_rmd, "\n - output:", out_pdf, "\n\n")
  
  rmarkdown::render(
    input = input_rmd,
    output_file = out_pdf,          # <- ABSOLUTO (clave)
    knit_root_dir = project_root,   # <- raíz del proyecto
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
  
  # Verificación explícita
  if (!file.exists(out_pdf)) {
    stop("Render terminó pero no encuentro el PDF en: ", out_pdf)
  }
  
  cat("PDF generado OK:", out_pdf, "\n\n")
  
  # 3) Enviar por Telegram
  caption <- paste0("Informe de Cartera (", format(Sys.Date(), "%Y-%m-%d"), ")")
  
  for (cid in chat_ids) {
    cat("Enviando a chat_id:", cid, "...\n")
    tryCatch(
      telegram_send_document(bot_token, cid, out_pdf, caption = caption),
      error = function(e) cat("ERROR enviando a ", cid, ": ", conditionMessage(e), "\n", sep = "")
    )
  }
  
  cat("\n=== FIN ===\n")
  cat("Log:", log_file, "\n")
  cat("PDF:", out_pdf, "\n")
}

# Ejecutar si se llama vía Rscript (Terminal/Cron)
if (identical(environment(), globalenv())) {
  tryCatch(main(), error = function(e) {
    message("FATAL: ", conditionMessage(e))
    quit(status = 1)
  })
}