library(data.table)

# ==============================================================================
# CHARGEMENT
# ==============================================================================

cat("Chargement des données interpolées...\n")
log <- fread("C:/Users/simon/Desktop/master_thesis/final_data/panel_logement_insee_2006_2020_interpolated.csv")

cat("Chargement df_final_reg_complet...\n")
reg <- fread("C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet.csv")

cat("Colonnes reg :", paste(names(reg)[1:10], collapse = ", "), "...\n")
cat("Colonnes log :", paste(names(log)[1:10], collapse = ", "), "...\n")

# ==============================================================================
# HARMONISATION CODGEO
# ==============================================================================

# Nettoie codgeo dans log : retire espaces et non-chiffres, garde codes à 5 chiffres
log[, codgeo := trimws(codgeo)]
log[, codgeo := gsub("[^0-9]", "", codgeo)]
log <- log[nchar(codgeo) == 5]

# Harmonise codgeo dans reg : renomme si besoin et formate en 5 chiffres avec zéros devant
if ("COM" %in% names(reg)) {
  setnames(reg, "COM", "codgeo")
} else if ("CODGEO" %in% names(reg)) {
  setnames(reg, "CODGEO", "codgeo")
}
reg[, codgeo := formatC(as.integer(codgeo), width = 5, flag = "0")]

# Drop colonnes logement existantes dans reg pour éviter les doublons .x/.y
cols_to_drop <- grep("^n_logements|^n_proprietaires|^n_hlm|^n_post_|^pct_proprietaires|^pct_hlm|^pct_post_|^interp_pct", 
                     names(reg), value = TRUE)
if (length(cols_to_drop) > 0) {
  cat("Suppression des colonnes existantes dans reg :", paste(cols_to_drop, collapse = ", "), "\n")
  reg[, (cols_to_drop) := NULL]
}

# ==============================================================================
# FUSION
# ==============================================================================

cat("Fusion par codgeo × year...\n")
df_merged <- merge(
  reg,
  log,
  by = c("codgeo", "year"),
  all.x = TRUE
)

cat(sprintf("Lignes avant : %d\n", nrow(reg)))
cat(sprintf("Lignes après : %d\n", nrow(df_merged)))

# ==============================================================================
# DIAGNOSTIC
# ==============================================================================

# Couverture temporelle : combien d'obs ont des données logement par année
coverage <- df_merged[, .(
  n_obs = .N,
  avec_log = sum(!is.na(n_logements)),
  pct_couvert = round(sum(!is.na(n_logements)) / .N * 100, 1)
), by = year][order(year)]
print(coverage)

# ==============================================================================
# EXPORT
# ==============================================================================

fwrite(df_merged, "C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_logement.csv")
cat("✓ Export réussi\n")
print(gc())
