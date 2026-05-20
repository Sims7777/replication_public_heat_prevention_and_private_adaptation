library(data.table)

# ==============================================================================
# 0. CONFIGURATION
# ==============================================================================

BASE_FINAL <- "C:/Users/simon/Desktop/master_thesis/final_data"
path_input  <- file.path(BASE_FINAL, "df_final_reg_logement_pnc.csv")
path_output <- file.path(BASE_FINAL, "df_final_reg_logement_pnc_bins.csv")

cat("=== CRÉATION VARIABLES BINNED ===\n\n")

gc()

# ==============================================================================
# 1. CHARGEMENT
# ==============================================================================

cat("Chargement df_final_reg_logement_pnc...\n")
df <- fread(path_input)
cat(sprintf("  -> %d lignes x %d colonnes\n\n", nrow(df), ncol(df)))

# ==============================================================================
# 2. BINNING PNC
# ==============================================================================

cat("--- Binning variables PNC ---\n")

# n_jours_bin : terciles ordonnés
df[, n_jours_bin := fcase(
  is.na(n_jours_pnc), NA_character_,
  n_jours_pnc == 0, "0. Pas de canicule",
  n_jours_pnc <= 3, "1. Court (1-3j)",
  n_jours_pnc <= 6, "2. Moyen (4-6j)",
  n_jours_pnc >= 7, "3. Long (7j+)",
  default = NA_character_
)]

# dose_bin : terciles ordonnés
df[, dose_bin := fcase(
  is.na(dose_pnc), NA_character_,
  dose_pnc == 0, "0. Pas de canicule",
  dose_pnc <= 3, "1. Faible (1-3)",
  dose_pnc <= 6, "2. Moyen (4-6)",
  dose_pnc >= 7, "3. Fort (7+)",
  default = NA_character_
)]

# share_bin : seuils 10/20%
df[, share_bin := fcase(
  is.na(share_pnc), NA_character_,
  share_pnc == 0, "0. Pas de canicule",
  share_pnc < 0.10, "1. < 10% du mois",
  share_pnc < 0.20, "2. 10-20% du mois",
  share_pnc >= 0.20, "3. ≥ 20% du mois",
  default = NA_character_
)]

# Convertir en factors ordonnés
df[, n_jours_bin := factor(n_jours_bin, 
                           levels = c("0. Pas de canicule", "1. Court (1-3j)", 
                                      "2. Moyen (4-6j)", "3. Long (7j+)"))]

df[, dose_bin := factor(dose_bin, 
                        levels = c("0. Pas de canicule", "1. Faible (1-3)", 
                                   "2. Moyen (4-6)", "3. Fort (7+)"))]

df[, share_bin := factor(share_bin, 
                         levels = c("0. Pas de canicule", "1. < 10% du mois", 
                                    "2. 10-20% du mois", "3. ≥ 20% du mois"))]

# Distribution
cat("\n--- Distribution n_jours_bin ---\n")
print(df[!is.na(n_jours_bin), .(.N, pct = round(.N / nrow(df[!is.na(n_jours_bin)]) * 100, 2)), 
         by = n_jours_bin])

cat("\n--- Distribution dose_bin ---\n")
print(df[!is.na(dose_bin), .(.N, pct = round(.N / nrow(df[!is.na(dose_bin)]) * 100, 2)), 
         by = dose_bin])

cat("\n--- Distribution share_bin ---\n")
print(df[!is.na(share_bin), .(.N, pct = round(.N / nrow(df[!is.na(share_bin)]) * 100, 2)), 
         by = share_bin])

# Statistiques par bin
cat("\n--- Stats par n_jours_bin ---\n")
print(df[!is.na(n_jours_bin), .(
  n = .N,
  moy_jours = round(mean(n_jours_pnc, na.rm = TRUE), 2),
  moy_dose = round(mean(dose_pnc, na.rm = TRUE), 2),
  moy_share = round(mean(share_pnc, na.rm = TRUE), 3)
), by = n_jours_bin])

cat("\n--- Stats par dose_bin ---\n")
print(df[!is.na(dose_bin), .(
  n = .N,
  moy_jours = round(mean(n_jours_pnc, na.rm = TRUE), 2),
  moy_dose = round(mean(dose_pnc, na.rm = TRUE), 2),
  moy_share = round(mean(share_pnc, na.rm = TRUE), 3)
), by = dose_bin])

cat("\n--- Stats par share_bin ---\n")
print(df[!is.na(share_bin), .(
  n = .N,
  moy_jours = round(mean(n_jours_pnc, na.rm = TRUE), 2),
  moy_dose = round(mean(dose_pnc, na.rm = TRUE), 2),
  moy_share = round(mean(share_pnc, na.rm = TRUE), 3)
), by = share_bin])

# ==============================================================================
# 3. BINNING CLIMATISATION
# ==============================================================================

cat("\n--- Binning taux climatisation ---\n")

# taux_clim_RG : quartiles
df[, taux_clim_RG_bin := fcase(
  is.na(taux_clim_RG), NA_character_,
  taux_clim_RG <= 0.01443240, "1. Q1 : Très faible (≤ 1.4%)",
  taux_clim_RG <= 0.03167915, "2. Q2 : Faible (1.4-3.2%)",
  taux_clim_RG <= 0.06794577, "3. Q3 : Moyen (3.2-6.8%)",
  default = "4. Q4 : Élevé (> 6.8%)"
)]

# taux_clim_DEP : terciles (Q1/Q3)
df[, taux_clim_DEP_bin := fcase(
  is.na(taux_clim_DEP), NA_character_,
  taux_clim_DEP <= 0.004470624, "1. Très faible (≤ 0.4%)",
  taux_clim_DEP <= 0.029883428, "2. Faible (0.4-3.0%)",
  default = "3. Moyen/Élevé (> 3.0%)"
)]

# Convertir en factors ordonnés
df[, taux_clim_RG_bin := factor(
  taux_clim_RG_bin, 
  levels = c("1. Q1 : Très faible (≤ 1.4%)", 
             "2. Q2 : Faible (1.4-3.2%)", 
             "3. Q3 : Moyen (3.2-6.8%)", 
             "4. Q4 : Élevé (> 6.8%)")
)]

df[, taux_clim_DEP_bin := factor(
  taux_clim_DEP_bin, 
  levels = c("1. Très faible (≤ 0.4%)", 
             "2. Faible (0.4-3.0%)", 
             "3. Moyen/Élevé (> 3.0%)")
)]

cat("\n--- Distribution taux_clim_RG_bin ---\n")
print(df[!is.na(taux_clim_RG_bin), .(.N, pct = round(.N / nrow(df[!is.na(taux_clim_RG_bin)]) * 100, 2)), 
         by = taux_clim_RG_bin])

cat("\n--- Distribution taux_clim_DEP_bin ---\n")
print(df[!is.na(taux_clim_DEP_bin), .(.N, pct = round(.N / nrow(df[!is.na(taux_clim_DEP_bin)]) * 100, 2)), 
         by = taux_clim_DEP_bin])

# ==============================================================================
# 4. EXPORT
# ==============================================================================

cat("\n--- Export ---\n")
fwrite(df, path_output)

cat(sprintf("✓ Fichier exporté : %s\n", path_output))
cat(sprintf("  %d lignes x %d colonnes\n", nrow(df), ncol(df)))
cat("  Nouvelles colonnes : n_jours_bin, dose_bin, share_bin, taux_clim_RG_bin, taux_clim_DEP_bin\n")
cat("\n✓ Script terminé avec succès !\n")
print(gc())
