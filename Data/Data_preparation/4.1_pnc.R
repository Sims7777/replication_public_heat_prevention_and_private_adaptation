library(data.table)

# ==============================================================================
# CONFIGURATION
# ==============================================================================
BASE_FINAL  <- "C:/Users/simon/Desktop/master_thesis/final_data"
path_input  <- file.path(BASE_FINAL, "df_final_reg_logement_pnc.csv")
path_output <- file.path(BASE_FINAL, "df_final_reg_logement_pnc_bins.csv")

gc()

# ==============================================================================
# LOAD
# ==============================================================================
df <- fread(path_input)

# ==============================================================================
# BIN: PNC VARIABLES
# ==============================================================================

# Duration bins
df[, n_jours_bin := fcase(
  is.na(n_jours_pnc),  NA_character_,
  n_jours_pnc == 0,    "0. Pas de canicule",
  n_jours_pnc <= 3,    "1. Court (1-3j)",
  n_jours_pnc <= 6,    "2. Moyen (4-6j)",
  n_jours_pnc >= 7,    "3. Long (7j+)",
  default = NA_character_
)]

# Dose bins
df[, dose_bin := fcase(
  is.na(dose_pnc),  NA_character_,
  dose_pnc == 0,    "0. Pas de canicule",
  dose_pnc <= 3,    "1. Faible (1-3)",
  dose_pnc <= 6,    "2. Moyen (4-6)",
  dose_pnc >= 7,    "3. Fort (7+)",
  default = NA_character_
)]

# Share bins (10/20% thresholds)
df[, share_bin := fcase(
  is.na(share_pnc),    NA_character_,
  share_pnc == 0,      "0. Pas de canicule",
  share_pnc < 0.10,    "1. < 10% du mois",
  share_pnc < 0.20,    "2. 10-20% du mois",
  share_pnc >= 0.20,   "3. \u2265 20% du mois",
  default = NA_character_
)]

# Convert to ordered factors
df[, n_jours_bin := factor(n_jours_bin,
  levels = c("0. Pas de canicule", "1. Court (1-3j)", "2. Moyen (4-6j)", "3. Long (7j+)"))]

df[, dose_bin := factor(dose_bin,
  levels = c("0. Pas de canicule", "1. Faible (1-3)", "2. Moyen (4-6)", "3. Fort (7+)"))]

df[, share_bin := factor(share_bin,
  levels = c("0. Pas de canicule", "1. < 10% du mois", "2. 10-20% du mois", "3. \u2265 20% du mois"))]

# ==============================================================================
# BIN: AC RATES
# ==============================================================================

# Regional rate — quartiles
df[, taux_clim_RG_bin := fcase(
  is.na(taux_clim_RG),              NA_character_,
  taux_clim_RG <= 0.01443240,       "1. Q1 : Tr\u00e8s faible (\u2264 1.4%)",
  taux_clim_RG <= 0.03167915,       "2. Q2 : Faible (1.4-3.2%)",
  taux_clim_RG <= 0.06794577,       "3. Q3 : Moyen (3.2-6.8%)",
  default =                          "4. Q4 : \u00c9lev\u00e9 (> 6.8%)"
)]

# Departmental rate — tertiles
df[, taux_clim_DEP_bin := fcase(
  is.na(taux_clim_DEP),             NA_character_,
  taux_clim_DEP <= 0.004470624,     "1. Tr\u00e8s faible (\u2264 0.4%)",
  taux_clim_DEP <= 0.029883428,     "2. Faible (0.4-3.0%)",
  default =                          "3. Moyen/\u00c9lev\u00e9 (> 3.0%)"
)]

df[, taux_clim_RG_bin := factor(taux_clim_RG_bin,
  levels = c("1. Q1 : Tr\u00e8s faible (\u2264 1.4%)",
             "2. Q2 : Faible (1.4-3.2%)",
             "3. Q3 : Moyen (3.2-6.8%)",
             "4. Q4 : \u00c9lev\u00e9 (> 6.8%)"))]

df[, taux_clim_DEP_bin := factor(taux_clim_DEP_bin,
  levels = c("1. Tr\u00e8s faible (\u2264 0.4%)",
             "2. Faible (0.4-3.0%)",
             "3. Moyen/\u00c9lev\u00e9 (> 3.0%)"))]

# ==============================================================================
# EXPORT
# ==============================================================================
fwrite(df, path_output)
gc()
