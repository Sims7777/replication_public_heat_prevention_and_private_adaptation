# LIBRARIES ----
packages <- c("data.table", "dplyr", "readr", "lubridate",
              "ncdf4", "raster", "sf", "tidyr")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ===========================================================================
# PATHS — aligned with downstream scripts
# ===========================================================================
BASE_THESIS         <- "C:/Users/simon/Desktop/master_thesis/temp"
EOBS_DIR            <- file.path(BASE_THESIS, "eobs_netcdf")
SHAPEFILE_PATH      <- file.path(
  BASE_THESIS,
  "shapefiles/ADMIN-EXPRESS-COG-CARTO_3-1__SHP_LAMB93_FXX_2022-04-15/ADMIN-EXPRESS-COG-CARTO/1_DONNEES_LIVRAISON_2022-04-15/ADECOGC_3-1_SHP_LAMB93_FXX/COMMUNE.shp"
)
INSEE_FILE          <- file.path(BASE_THESIS, "insee/commune_2022.csv")
OUTPUT_DIR_TEMP     <- file.path(BASE_THESIS, "donnees_temperature")
OUTPUT_DIR_COMMUNES <- file.path(BASE_THESIS, "donnees_communes_annees")

dir.create(OUTPUT_DIR_TEMP,     recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_DIR_COMMUNES, recursive = TRUE, showWarnings = FALSE)

# ===========================================================================
# E-OBS EXTRACTION — RUN ONCE ONLY
# (uncomment the block to run)
# ===========================================================================

# extract_variable <- function(var_name, nc_file) {
#   cat(sprintf("\n=== EXTRACTING %s ===\n", toupper(var_name)))
#   shapefile <- st_read(SHAPEFILE_PATH)
#   pre1.brick <- brick(file.path(EOBS_DIR, nc_file))
#   shapefile  <- filter(shapefile, INSEE_COM < "95600")        # keep mainland France only
#   shp        <- st_transform(shapefile, crs(pre1.brick))      # reproject to match raster CRS
#   out_crop   <- crop(pre1.brick, extent(shp))                 # crop raster to shapefile extent
#   out_crop2  <- raster::mask(out_crop, shp)                   # mask to municipality boundaries
#   mean_value <- raster::extract(out_crop2, as(shp, "Spatial"),
#                                 fun = mean, na.rm = TRUE)     # extract mean value per municipality
#   write.csv(mean_value, file.path(OUTPUT_DIR_TEMP, var_name))
#   shp$nombre <- row.names(shp)
#   shp1 <- as.data.frame(shp[, c("INSEE_COM","nombre","NOM","POPULATION")])
#   shp1 <- shp1[, c("INSEE_COM","nombre","NOM","POPULATION")]
#   write.csv(shp1, file.path(OUTPUT_DIR_TEMP, paste0("shape_nom_communes_", var_name)))
#   cat(sprintf("%s: OK\n", var_name))
#   gc()
# }
#
# extract_variable("tg", "tg_ens_mean_0.1deg_reg_v26.0e.nc")
# extract_variable("hu", "hu_ens_mean_0.1deg_reg_v26.0e.nc")
# extract_variable("rr", "rr_ens_mean_0.1deg_reg_v26.0e.nc")
# extract_variable("fg", "fg_ens_mean_0.1deg_reg_v26.0e.nc")
# extract_variable("tx", "tx_ens_mean_0.1deg_reg_v26.0e.nc")
# extract_variable("tn", "tn_ens_mean_0.1deg_reg_v26.0e.nc")

# ===========================================================================
# LOAD SHAPEFILE AND MUNICIPALITY CROSSWALK
# ===========================================================================
shape <- fread(file.path(OUTPUT_DIR_TEMP, "shape_nom_communes"))

# Load INSEE commune list and restrict to mainland France
commune_2022 <- read_csv(INSEE_FILE)
commune_2022 <- filter(commune_2022, !is.na(DEP))
commune_2022 <- filter(commune_2022, !(DEP %in% c("976","974","973","972","971")))
commune_2022 <- commune_2022[, c("COM","REG","DEP","LIBELLE")]

# Identify date columns in the extracted TG file
tg_headers  <- names(fread(file.path(OUTPUT_DIR_TEMP, "tg"), nrows = 0))
date_cols_h <- grep("^X\\d{4}\\.\\d{2}\\.\\d{2}$", tg_headers, value = TRUE)
last_col_h  <- tail(date_cols_h, 1)

# Keep only communes with non-missing temperature data on the last available date
tg_check <- fread(file.path(OUTPUT_DIR_TEMP, "tg"), select = c("V1", last_col_h))
tg_check <- left_join(tg_check, shape, by = "V1")
tg_check <- filter(tg_check, !is.na(tg_check[[last_col_h]]))
communes  <- as.character(tg_check$INSEE_COM)
rm(tg_check); gc()

# Restrict commune list to those with valid weather data
commune_2022 <- filter(commune_2022, COM %in% communes)
communes     <- commune_2022$COM
cat(sprintf(">>> %d communes retained\n", length(communes)))

# ===========================================================================
# TIME PERIODS
# ===========================================================================
period_bounds <- list(
  c(as.Date("1980-01-01"), as.Date("1989-12-31")),
  c(as.Date("1990-01-01"), as.Date("1999-12-31")),
  c(as.Date("2000-01-01"), as.Date("2009-12-31")),
  c(as.Date("2010-01-01"), as.Date("2022-12-31"))
)
n_periods <- length(period_bounds)

# ===========================================================================
# PROCESSING FUNCTION — ONE VARIABLE AT A TIME, ONE PERIOD AT A TIME
# Processes data period by period to limit memory usage
# ===========================================================================
process_variable_efficient <- function(var_name, shape, communes,
                                       OUTPUT_DIR_TEMP, OUTPUT_DIR_COMMUNES) {
  cat(sprintf("\n========== VARIABLE: %s ==========\n", toupper(var_name)))

  # Read column names and parse date columns
  all_col_names <- names(fread(file.path(OUTPUT_DIR_TEMP, var_name), nrows = 0))
  all_date_cols <- all_col_names[grep("^X\\d{4}\\.\\d{2}\\.\\d{2}$", all_col_names)]
  col_dates     <- as.Date(gsub("\\.", "-", substring(all_date_cols, 2, 11)))

  cat(sprintf(">>> Coverage: %s to %s (%d days)\n",
              min(col_dates), max(col_dates), length(col_dates)))

  for (i in seq_along(period_bounds)) {
    start_d <- period_bounds[[i]][1]
    end_d   <- period_bounds[[i]][2]
    period_col_names <- all_date_cols[col_dates >= start_d & col_dates <= end_d]

    if (length(period_col_names) == 0) {
      cat(sprintf("  Period %d: no data\n", i)); next
    }

    cat(sprintf("  Period %d (%s to %s): %d columns...\n",
                i, start_d, end_d, length(period_col_names)))

    # Load only the relevant date columns for this period
    data_p <- fread(file.path(OUTPUT_DIR_TEMP, var_name),
                    select = c("V1", period_col_names))
    data_p <- left_join(data_p, shape, by = "V1")
    data_p <- filter(data_p, !is.na(INSEE_COM))
    data_p <- data_p[, c(period_col_names, "INSEE_COM"), with = FALSE]

    # Reshape from wide to long format
    data_p <- data_p %>% gather(variable, value, -INSEE_COM)
    data_p$variable <- gsub("\\.", "-", substring(data_p$variable, 2, 11))
    data_p$value    <- round(data_p$value, 1)
    names(data_p)[names(data_p) == "INSEE_COM"] <- "COM"
    names(data_p)[names(data_p) == "variable"]  <- "date"
    data_p$date <- as.Date(data_p$date)

    # Keep only valid communes with non-missing values
    data_p <- filter(data_p, COM %in% communes, !is.na(value))
    names(data_p)[names(data_p) == "value"] <- var_name

    cat(sprintf("  -> %d rows\n", nrow(data_p)))
    fwrite(data_p,
           file.path(OUTPUT_DIR_COMMUNES, sprintf("tmp_%s_p%d.csv", var_name, i)))
    rm(data_p); gc()
  }
  cat(sprintf(">>> %s: all periods saved\n", var_name))
}

# ===========================================================================
# PROCESS ALL 6 VARIABLES
# ===========================================================================
for (var in c("tg", "hu", "rr", "fg", "tx","tn")) {
  process_variable_efficient(var, shape, communes, OUTPUT_DIR_TEMP, OUTPUT_DIR_COMMUNES)
}

# ===========================================================================
# FINAL ASSEMBLY — JOIN ALL VARIABLES PERIOD BY PERIOD
# ===========================================================================
cat("\n=== FINAL ASSEMBLY ===\n")

out_final   <- file.path(OUTPUT_DIR_COMMUNES, "communes_1980_2022_all_variables.csv")
all_vars    <- c("tg", "hu", "rr", "fg", "tx","tn")
first_write <- TRUE

for (i in seq_len(n_periods)) {
  cat(sprintf(">>> Joining period %d...\n", i))
  tg_file <- file.path(OUTPUT_DIR_COMMUNES, sprintf("tmp_tg_p%d.csv", i))
  if (!file.exists(tg_file)) next

  # Start from TG and left-join all other variables on COM x date
  period_data <- fread(tg_file)
  for (var in c("hu", "rr", "fg", "tx","tn")) {
    var_file <- file.path(OUTPUT_DIR_COMMUNES, sprintf("tmp_%s_p%d.csv", var, i))
    if (file.exists(var_file)) {
      tmp <- fread(var_file)
      period_data <- left_join(period_data, tmp, by = c("COM", "date"))
      rm(tmp); gc()
    }
  }

  cat(sprintf("  -> %d rows\n", nrow(period_data)))
  # Write first period normally, append subsequent periods
  if (first_write) {
    fwrite(period_data, out_final); first_write <- FALSE
  } else {
    fwrite(period_data, out_final, append = TRUE)
  }
  rm(period_data); gc()
}

# Clean up temporary period files
cat(">>> Cleaning up temporary files...\n")
for (var in all_vars) {
  for (i in seq_len(n_periods)) {
    f <- file.path(OUTPUT_DIR_COMMUNES, sprintf("tmp_%s_p%d.csv", var, i))
    if (file.exists(f)) file.remove(f)
  }
}

cat(sprintf("\n=== DONE ===\nFinal file: %s\n", out_final))


# Copy final output to the main data folder
file.copy(
  from = file.path(OUTPUT_DIR_COMMUNES, "communes_1980_2022_all_variables.csv"),
  to   = "C:/Users/simon/Desktop/master_thesis/meteo/communes_1980_2022_all_variables_final.csv",
  overwrite = TRUE
)
