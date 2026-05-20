# ===========================================================================
# BUILD_BINS_DAILY_CHUNKS.R — lecture disque par chunk (low RAM)
# ===========================================================================

library(data.table)
library(lubridate)

BASE_THESIS <- "C:/Users/simon/Desktop/master_thesis"
BASE_METEO  <- file.path(BASE_THESIS, "meteo")
BASE_FINAL  <- file.path(BASE_THESIS, "final_data")

METEO_DAILY <- file.path(BASE_METEO, "communes_1980_2022_all_variables_final.csv")
OUT_BINS_13 <- file.path(BASE_FINAL, "meteo_bins_13_1980_2019.csv")
OUT_BINS_9  <- file.path(BASE_FINAL, "meteo_bins_9_1980_2019.csv")

dir.create(BASE_FINAL, recursive = TRUE, showWarnings = FALSE)

# ===========================================================================
# BINS — 13 bins (left-closed : [a, b))
# ===========================================================================
temp_breaks <- c(-Inf, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 28, 30, Inf)
temp_labels <- c(
  "tbin_lt_m20",
  "tbin_m20_m15",
  "tbin_m15_m10",
  "tbin_m10_m5",
  "tbin_m5_0",
  "tbin_0_5",
  "tbin_5_10",
  "tbin_10_15",
  "tbin_15_20",
  "tbin_20_25",
  "tbin_25_28",
  "tbin_28_30",
  "tbin_gt_30"
)

hu_breaks <- c(-Inf, 20, 40, 60, 80, Inf)
hu_labels <- c("hubin_0_20","hubin_20_40","hubin_40_60","hubin_60_80","hubin_80_100")
rr_breaks <- c(-Inf, 0, 3, 10, 100, Inf)
rr_labels <- c("rrbin_0mm","rrbin_0_3","rrbin_3_10","rrbin_10_100","rrbin_gt_100")
fg_breaks <- c(-Inf, 3, 10, 20, Inf)
fg_labels <- c("fgbin_0_3","fgbin_3_10","fgbin_10_20","fgbin_gt_20")

bin_cols <- c(paste0("tg_", temp_labels), paste0("tx_", temp_labels),
              hu_labels, rr_labels, fg_labels)

# ===========================================================================
# COUNT LINES
# ===========================================================================
cat("=== LECTURE EN-TÊTE ===\n")
header <- names(fread(METEO_DAILY, nrows = 0))
cat("Colonnes :", paste(header, collapse = ", "), "\n")

n_total <- as.integer(system(
  paste0('find /c /v "" "', normalizePath(METEO_DAILY, winslash = "\\"), '"'),
  intern = TRUE
))
if (length(n_total) == 0 || is.na(n_total)) {
  cat("Comptage ligne par ligne...\n")
  n_total <- nrow(fread(METEO_DAILY, select = 1L))
}
n_data <- n_total - 1L
cat(sprintf("Lignes de données : %d\n", n_data))

# ===========================================================================
# CHUNKS : ~2 millions LINES
# ===========================================================================
CHUNK_SIZE <- 2000000L
n_chunks   <- ceiling(n_data / CHUNK_SIZE)
cat(sprintf("Chunks prévus : %d (taille : %d lignes)\n", n_chunks, CHUNK_SIZE))

# ===========================================================================
# FUNCTION: process a daily data.table → aggregated bins (13 fine bins)
# right = FALSE → intervals [a, b) = same logic as Salesse (>= lower bound)
# ===========================================================================
process_dt <- function(dt) {
  
  dt[, COM := as.character(COM)]
  dt[, COM := trimws(COM)]
  dt[!grepl("^2[AB]", COM), COM := formatC(as.integer(COM), width = 5, flag = "0")]
  
  dt[, date  := as.IDate(date)]
  dt[, year  := year(date)]
  dt[, month := month(date)]
  
  dt <- dt[year >= 1980 & year <= 2019]
  dt <- dt[!grepl("^97|^98|^99|^2[AB]", COM)]  # exclusion DOM-TOM + Corse
  dt[hu < 0,   hu := 0]
  dt[hu > 100, hu := 100]
  
  if (nrow(dt) == 0) return(NULL)
  
  # Binning — right = FALSE → [a, b)
  dt[, tg_bin := cut(tg, breaks = temp_breaks, labels = temp_labels,
                     include.lowest = TRUE, right = FALSE)]
  dt[, tx_bin := cut(tx, breaks = temp_breaks, labels = temp_labels,
                     include.lowest = TRUE, right = FALSE)]
  dt[, hu_bin := cut(hu, breaks = hu_breaks, labels = hu_labels,
                     include.lowest = TRUE, right = FALSE)]
  dt[, rr_bin := cut(rr, breaks = rr_breaks, labels = rr_labels,
                     include.lowest = TRUE, right = FALSE)]
  dt[, fg_bin := cut(fg, breaks = fg_breaks, labels = fg_labels,
                     include.lowest = TRUE, right = FALSE)]
  
  # One-hot encoding
  for (lb in temp_labels) {
    dt[, paste0("tg_", lb) := as.integer(!is.na(tg_bin) & tg_bin == lb)]
    dt[, paste0("tx_", lb) := as.integer(!is.na(tx_bin) & tx_bin == lb)]
  }
  for (lb in hu_labels) dt[, (lb) := as.integer(!is.na(hu_bin) & hu_bin == lb)]
  for (lb in rr_labels) dt[, (lb) := as.integer(!is.na(rr_bin) & rr_bin == lb)]
  for (lb in fg_labels) dt[, (lb) := as.integer(!is.na(fg_bin) & fg_bin == lb)]
  
  # Agrégation mensuelle
  agg <- dt[, c(
    list(n_days = .N),
    lapply(.SD, sum, na.rm = TRUE)
  ), by = .(COM, year, month), .SDcols = bin_cols]
  
  return(agg)
}

# ===========================================================================
# MAIN LOOP: read chunk by chunk
# Remaining: avoid splitting a month between two chunks
# ===========================================================================
cat("\n=== TRAITEMENT PAR CHUNK ===\n")

residual    <- NULL
first_write <- TRUE
skip_rows   <- 0L

repeat {
  cat(sprintf("\n--- Lecture lignes %d à %d ---\n",
              skip_rows + 1L, skip_rows + CHUNK_SIZE))
  
  chunk <- tryCatch(
    fread(METEO_DAILY,
          skip      = skip_rows + 1L,
          nrows     = CHUNK_SIZE,
          col.names = header),
    error = function(e) NULL
  )
  
  if (is.null(chunk) || nrow(chunk) == 0L) {
    cat("Fin du fichier.\n")
    if (!is.null(residual) && nrow(residual) > 0L) {
      cat("Traitement résidu final...\n")
      agg <- process_dt(residual)
      if (!is.null(agg)) {
        for (col in bin_cols) agg[, (col) := get(col) / n_days * 30]
        fwrite(agg, OUT_BINS_13, append = !first_write)
        first_write <- FALSE
      }
    }
    break
  }
  
  if (!is.null(residual)) {
    chunk <- rbindlist(list(residual, chunk), use.names = TRUE, fill = TRUE)
    residual <- NULL
  }
  
  chunk[, date_tmp  := as.IDate(date)]
  chunk[, year_tmp  := year(date_tmp)]
  chunk[, month_tmp := month(date_tmp)]
  
  last_ym   <- chunk[.N, .(year_tmp, month_tmp)]
  last_year <- last_ym$year_tmp
  last_mon  <- last_ym$month_tmp
  
  to_process <- chunk[!(year_tmp == last_year & month_tmp == last_mon)]
  residual   <- chunk[  year_tmp == last_year & month_tmp == last_mon]
  
  chunk[,      c("date_tmp","year_tmp","month_tmp") := NULL]
  to_process[, c("date_tmp","year_tmp","month_tmp") := NULL]
  residual[,   c("date_tmp","year_tmp","month_tmp") := NULL]
  
  cat(sprintf("  À traiter : %d lignes | Résidu : %d lignes\n",
              nrow(to_process), nrow(residual)))
  
  if (nrow(to_process) == 0L) {
    skip_rows <- skip_rows + CHUNK_SIZE
    rm(chunk); gc()
    next
  }
  
  agg <- process_dt(to_process)
  if (!is.null(agg) && nrow(agg) > 0L) {
    for (col in bin_cols) agg[, (col) := get(col) / n_days * 30]
    fwrite(agg, OUT_BINS_13, append = !first_write)
    first_write <- FALSE
    cat(sprintf("  Lignes agrégées : %d | Communes : %d\n",
                nrow(agg), length(unique(agg$COM))))
  }
  
  skip_rows <- skip_rows + CHUNK_SIZE
  rm(chunk, to_process, agg); gc()
}

# ===========================================================================
# VERIFI 13 BINS
# ===========================================================================
cat("\n=== VÉRIFICATION FICHIER 13 BINS ===\n")
final <- fread(OUT_BINS_13)
cat(sprintf("Lignes totales   : %d\n",   nrow(final)))
cat(sprintf("Communes totales : %d\n",   length(unique(final$COM))))
cat(sprintf("Années           : %d-%d\n", min(final$year), max(final$year)))

tg_cols_13 <- paste0("tg_", temp_labels)
final[, tg_sum := rowSums(.SD, na.rm = TRUE), .SDcols = tg_cols_13]
cat(sprintf("Somme bins tg (13) — min: %.2f | médiane: %.2f | max: %.2f\n",
            min(final$tg_sum, na.rm = TRUE),
            median(final$tg_sum, na.rm = TRUE),
            max(final$tg_sum, na.rm = TRUE)))


# ===========================================================================
# AGRÉGATION : 13 bins fins → 9 bins larges
# Mapping :
#   lt_m5   = lt_m20 + m20_m15 + m15_m10 + m10_m5
#   m5_0    = m5_0       (identique)
#   0_5     = 0_5        (identique)
#   5_10    = 5_10       (identique)
#   10_15   = 10_15      (identique)
#   15_20   = 15_20      (identique)
#   20_25   = 20_25      (identique)
#   25_30   = 25_28 + 28_30
#   gt_30   = gt_30      (identique)
# ===========================================================================
cat("\n=== AGRÉGATION 13 → 9 BINS ===\n")

temp_labels_9 <- c(
  "tbin_lt_m5","tbin_m5_0","tbin_0_5","tbin_5_10",
  "tbin_10_15","tbin_15_20","tbin_20_25","tbin_25_30","tbin_gt_30"
)

# tg
final[, tg_tbin_lt_m5  := tg_tbin_lt_m20 + tg_tbin_m20_m15 + tg_tbin_m15_m10 + tg_tbin_m10_m5]
final[, tg_tbin_m5_0   := tg_tbin_m5_0]
final[, tg_tbin_0_5    := tg_tbin_0_5]
final[, tg_tbin_5_10   := tg_tbin_5_10]
final[, tg_tbin_10_15  := tg_tbin_10_15]
final[, tg_tbin_15_20  := tg_tbin_15_20]
final[, tg_tbin_20_25  := tg_tbin_20_25]
final[, tg_tbin_25_30  := tg_tbin_25_28 + tg_tbin_28_30]
final[, tg_tbin_gt_30  := tg_tbin_gt_30]

# tx
final[, tx_tbin_lt_m5  := tx_tbin_lt_m20 + tx_tbin_m20_m15 + tx_tbin_m15_m10 + tx_tbin_m10_m5]
final[, tx_tbin_m5_0   := tx_tbin_m5_0]
final[, tx_tbin_0_5    := tx_tbin_0_5]
final[, tx_tbin_5_10   := tx_tbin_5_10]
final[, tx_tbin_10_15  := tx_tbin_10_15]
final[, tx_tbin_15_20  := tx_tbin_15_20]
final[, tx_tbin_20_25  := tx_tbin_20_25]
final[, tx_tbin_25_30  := tx_tbin_25_28 + tx_tbin_28_30]
final[, tx_tbin_gt_30  := tx_tbin_gt_30]

bins_9_tg <- paste0("tg_", temp_labels_9)
bins_9_tx <- paste0("tx_", temp_labels_9)

cols_keep <- c("COM", "year", "month", "n_days",
               bins_9_tg, bins_9_tx,
               hu_labels, rr_labels, fg_labels)

final_9bins <- final[, ..cols_keep]

fwrite(final_9bins, OUT_BINS_9)
cat(sprintf("Fichier 9 bins écrit : %s\n", OUT_BINS_9))
