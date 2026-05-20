library(data.table)
# ==============================================================================
# CHARGEMENT
# ==============================================================================
BASE_FINAL <- "C:/Users/simon/Desktop/master_thesis/final_data"
PATH_DENSITE <- "C:/Users/simon/Desktop/master_thesis/densité/communes-france-2025.csv"

df_reg <- fread(file.path(BASE_FINAL, "df_final_reg_complet1.csv"))
communes <- fread(PATH_DENSITE)

communes_sel <- communes[, .(code_insee, superficie_km2, niveau_equipements_services)]

# Harmoniser types + zero padding
communes_sel[, code_insee := str_pad(as.character(code_insee), width = 5, pad = "0", side = "left")]
df_reg[, COM := str_pad(as.character(COM), width = 5, pad = "0", side = "left")]

# ==============================================================================
# FUSION
# ==============================================================================
df_merged <- merge(df_reg, communes_sel, by.x = "COM", by.y = "code_insee", all.x = TRUE)
cat(sprintf("Lignes avant: %d | après: %d\n", nrow(df_reg), nrow(df_merged)))

# ==============================================================================
# CALCUL DENSITÉ
# ==============================================================================
df_merged[, densite := ifelse(superficie_km2 > 0, value_estimated_population / superficie_km2, NA_real_)]

# ==============================================================================
# EXPORT
# ==============================================================================
output_path <- file.path(BASE_FINAL, "df_final_reg_complet2.csv")
fwrite(df_merged, output_path)
cat(sprintf("✓ Exporté: %d × %d\n", nrow(df_merged), ncol(df_merged)))
