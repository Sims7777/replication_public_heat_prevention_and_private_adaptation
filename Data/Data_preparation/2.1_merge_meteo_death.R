# ===========================================================================
# MERGE_MORTALITY_BINS.R
# ===========================================================================

library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(readxl)
library(sf)

# ===========================================================================
# PATHS
# ===========================================================================
BASE_THESIS <- "C:/Users/simon/Desktop/master_thesis"
BASE_DECES  <- file.path(BASE_THESIS, "deces")
BASE_METEO  <- file.path(BASE_THESIS, "meteo")
BASE_FINAL  <- file.path(BASE_THESIS, "final_data")

BINS_FILE  <- file.path(BASE_FINAL, "meteo_bins_9_1980_2019.csv")
PROXY_FILE <- file.path(BASE_METEO, "proxy_communes_sans_meteo.csv")

SHAPEFILE_PATH <- "C:/Users/simon/Desktop/master_thesis/temp/shapefiles/ADMIN-EXPRESS-COG-CARTO_3-1__SHP_LAMB93_FXX_2022-04-15/ADMIN-EXPRESS-COG-CARTO/1_DONNEES_LIVRAISON_2022-04-15/ADECOGC_3-1_SHP_LAMB93_FXX/COMMUNE.shp"

dir.create(BASE_FINAL, recursive = TRUE, showWarnings = FALSE)

# ===========================================================================
# CROSSWALK TABLE
# ===========================================================================
table_passage <- read_excel(
  file.path(BASE_DECES, "passage-geo-2022.xlsx"),
  sheet = "PASSAGE_GEO_2022"
)
names(table_passage)[1] <- "COM_AV"
names(table_passage)[2] <- "COM_AP"
table_passage_bis <- table_passage[, c("COM_AV", "COM_AP")]
names(table_passage_bis)[1] <- "lieudeces"
table_passage_bis <- table_passage_bis %>%
  mutate(lieudeces = as.character(lieudeces))

# ===========================================================================
# DEATHS 1980-2019
# ===========================================================================
first_write <- TRUE

for (annee in 1980:2019) {
  fichier <- file.path(BASE_DECES, paste0("Deces_", annee, ".csv"))
  if (!file.exists(fichier)) next

  deces <- fread(fichier, sep = ";", encoding = "UTF-8")
  deces$lieudeces <- as.character(deces$lieudeces)
  deces <- left_join(deces, table_passage_bis, by = "lieudeces")
  deces$COM <- ifelse(!is.na(deces$COM_AP), deces$COM_AP, deces$lieudeces)

  deces$datedeces <- as.IDate(fast_strptime(as.character(deces$datedeces), "%Y%m%d"))
  deces$datenaiss <- as.IDate(fast_strptime(as.character(deces$datenaiss), "%Y%m%d"))
  deces$year      <- year(deces$datedeces)
  deces           <- deces[year == annee, ]

  deces$age_years   <- as.integer((deces$datedeces - deces$datenaiss) / 365.25)
  deces$month       <- month(deces$datedeces)
  deces$tranche_age <- cut(
    deces$age_years,
    c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf),
    labels = c("0-9","10-19","20-39","40-59","60-64","65-69","70-74","75-79","80+")
  )

  deces_agg <- deces %>%
    group_by(COM, month, tranche_age) %>%
    summarise(nbr_mort = n(), .groups = "drop")

  deces_spread <- deces_agg %>%
    pivot_wider(names_from = tranche_age, values_from = nbr_mort,
                values_fill = 0)
  deces_spread$year <- annee

  if (first_write) {
    fwrite(as.data.table(deces_spread),
           file.path(BASE_DECES, "deces_1980_2019_fusionne.csv"))
    first_write <- FALSE
  } else {
    fwrite(as.data.table(deces_spread),
           file.path(BASE_DECES, "deces_1980_2019_fusionne.csv"), append = TRUE)
  }

  rm(deces, deces_agg, deces_spread); gc()
}

# ===========================================================================
# DEATHS CLEANUP
# ===========================================================================
deces <- fread(file.path(BASE_DECES, "deces_1980_2019_fusionne.csv"))

deces[, COM := as.character(COM)]
deces[, COM := trimws(COM)]
deces[!grepl("^2[AB]", COM), COM := formatC(as.integer(COM), width = 5, flag = "0")]

deces[grepl("^751", COM), COM := "75056"]
deces[grepl("^6938", COM), COM := "69123"]
deces[grepl("^132",  COM), COM := "13055"]
deces <- deces[!grepl("^97|^98|^99|^2[AB]", COM)]  # exclude overseas + Corsica
deces <- deces[!is.na(COM)]

# ===========================================================================
# BINS
# ===========================================================================
bins <- fread(BINS_FILE)

bins[, COM := as.character(COM)]
bins[, COM := trimws(COM)]
bins[!grepl("^2[AB]", COM), COM := formatC(as.integer(COM), width = 5, flag = "0")]

# Corsica already excluded from bins — defensive filter
bins <- bins[!grepl("^2[AB]", COM)]

communes_bins  <- unique(bins$COM)
communes_deces <- unique(deces$COM)

# ===========================================================================
# PROXY: communes with deaths but no bins
# ===========================================================================
com_sans_bins <- setdiff(communes_deces, communes_bins)
com_sans_bins <- com_sans_bins[!is.na(com_sans_bins)]

if (length(com_sans_bins) > 0) {

  if (file.exists(PROXY_FILE)) {
    proxy <- fread(PROXY_FILE)

    proxy[, COM := as.character(COM)]
    proxy[, COM := trimws(COM)]
    proxy[!grepl("^2[AB]", COM), COM := formatC(as.integer(COM), width = 5, flag = "0")]

    proxy[, COM_proxy := as.character(COM_proxy)]
    proxy[, COM_proxy := trimws(COM_proxy)]
    proxy[!grepl("^2[AB]", COM_proxy), COM_proxy := formatC(as.integer(COM_proxy), width = 5, flag = "0")]

    proxy <- proxy[!grepl("^2[AB]", COM) & !grepl("^2[AB]", COM_proxy)]

    manquants <- setdiff(com_sans_bins, proxy$COM)

  } else {

    shapefile <- st_read(SHAPEFILE_PATH, quiet = TRUE)

    code_col_candidates <- c("INSEE_COM","CODE_INSEE","codgeo","DEPCOM","insee","code")
    CODE_COL <- code_col_candidates[code_col_candidates %in% names(shapefile)][1]

    # Exclude overseas territories and Corsica
    shapefile <- shapefile %>%
      filter(!startsWith(as.character(.data[[CODE_COL]]), "97"),
             !startsWith(as.character(.data[[CODE_COL]]), "98"),
             !grepl("^2[AB]", as.character(.data[[CODE_COL]])))

    shapefile  <- st_transform(shapefile, crs = 2154)
    centroides <- st_centroid(shapefile)

    com_avec_sf <- centroides[as.character(centroides[[CODE_COL]]) %in% communes_bins, ]
    com_sans_sf <- centroides[as.character(centroides[[CODE_COL]]) %in% com_sans_bins, ]

    # Assign each unmatched commune to its nearest commune with bins
    idx_proche <- st_nearest_feature(com_sans_sf, com_avec_sf)

    proxy <- data.table(
      COM       = as.character(com_sans_sf[[CODE_COL]]),
      COM_proxy = as.character(com_avec_sf[[CODE_COL]][idx_proche])
    )

    proxy[, COM := trimws(COM)]
    proxy[!grepl("^2[AB]", COM), COM := formatC(as.integer(COM), width = 5, flag = "0")]

    proxy[, COM_proxy := trimws(COM_proxy)]
    proxy[!grepl("^2[AB]", COM_proxy), COM_proxy := formatC(as.integer(COM_proxy), width = 5, flag = "0")]

    fwrite(proxy, PROXY_FILE)
  }

  # Duplicate bins for proxy communes
  bins_proxy <- merge(proxy, bins, by.x = "COM_proxy", by.y = "COM", all.x = TRUE)
  bins_proxy[, COM_proxy := NULL]

  bins_complet <- rbindlist(list(bins, bins_proxy), use.names = TRUE)
  bins_complet <- unique(bins_complet, by = c("COM", "year", "month"))

} else {
  bins_complet <- bins
}

# ===========================================================================
# FINAL MERGE: bins (left) x deaths (right)
# ===========================================================================

deces[, year  := as.integer(year)]
deces[, month := as.integer(month)]

df_final <- merge(
  bins_complet,
  deces,
  by    = c("COM", "year", "month"),
  all.x = TRUE
)

# Months with no recorded deaths → 0
age_cols <- c("0-9","10-19","20-39","40-59","60-64","65-69","70-74","75-79","80+")
for (col in age_cols) {
  if (col %in% names(df_final)) {
    df_final[is.na(get(col)), (col) := 0L]
  }
}

# Total deaths per row
df_final[, deces_total := rowSums(.SD, na.rm = TRUE), .SDcols = age_cols]

# ===========================================================================
# SAVE
# ===========================================================================
OUT_FINAL <- file.path(BASE_FINAL, "df_bins_mortality_1980_2019.csv")
fwrite(df_final, OUT_FINAL)
