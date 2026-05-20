# ===========================================================================
# BUILD_DF_FINAL_REG.R
# Fusion : df_bins_mortality_clim × population → df_final_reg
# ===========================================================================

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)

# ===========================================================================
# CHEMINS
# ===========================================================================
BASE_THESIS  <- "C:/Users/simon/Desktop/master_thesis"
BASE_FINAL   <- file.path(BASE_THESIS, "final_data")
BASE_POP_OUT <- file.path(BASE_THESIS, "population_harmonized")

# ===========================================================================
# CHARGEMENT
# ===========================================================================
cat("=== CHARGEMENT ===\n")

base <- fread(file.path(BASE_FINAL, "df_bins_mortality_clim_1980_2019.csv"))
population_interpolee <- fread(
  file.path(BASE_POP_OUT, "population_interpolee_1980_2022.csv")
)

cat(sprintf("Lignes base          : %d\n", nrow(base)))
cat(sprintf("Lignes population    : %d\n", nrow(population_interpolee)))
cat(sprintf("Colonnes base        : %d\n", ncol(base)))

# ===========================================================================
# NORMALISATION COM
# ===========================================================================
base[, COM := as.character(COM)]
base[, COM := trimws(COM)]
base[!grepl("^2[AB]", COM), COM := formatC(as.integer(COM), width = 5, flag = "0")]

base <- base[!grepl("^2[AB]", COM)]
base[, year := as.integer(year)]

population_interpolee[, YEAR := as.integer(YEAR)]
population_interpolee[, COM  := as.character(COM)]
population_interpolee[, COM  := trimws(COM)]

population_interpolee <- population_interpolee[!grepl("^2[AB]|^20[0-9]{3}$", COM) |
                                                 grepl("^[0-9]{5}$", COM)]
population_interpolee <- population_interpolee[
  grepl("^[0-9]{5}$", COM) &
    !grepl("^(97|98|99|20)", COM) &
    !is.na(COM)
]

cat(sprintf("Communes valides dans base       : %d\n", length(unique(base$COM))))
cat(sprintf("Communes valides dans population : %d\n", length(unique(population_interpolee$COM))))

n_corse_base <- length(unique(base$COM[grepl("^2[AB]|^20", base$COM)]))
n_corse_pop  <- length(unique(population_interpolee$COM[grepl("^20", population_interpolee$COM)]))
if (n_corse_base == 0) cat("✅ Corse absente de base.\n") else cat(sprintf("❌ %d communes Corse dans base !\n", n_corse_base))
if (n_corse_pop  == 0) cat("✅ Corse absente de population.\n") else cat(sprintf("❌ %d communes Corse dans population !\n", n_corse_pop))

population_interpolee[, AGE_QUINQUENNAL := gsub("\u2013|\u2014|\u2212", "-",
                                                AGE_QUINQUENNAL)]

# ===========================================================================
# POPULATION TOTALE
# ===========================================================================
cat("\n=== POPULATION TOTALE ===\n")

population_total <- population_interpolee[
  SEXE == "TOTAL" & AGE_QUINQUENNAL == "TOTAL" & !is.na(COM),
  .(COM, year = YEAR, population_totale = POPULATION)
]

dup_check <- population_total[, .N, by = .(COM, year)][N > 1]
cat(sprintf("Doublons dans population_total : %d\n", nrow(dup_check)))
cat(sprintf("Lignes population_total        : %d\n", nrow(population_total)))

# ===========================================================================
# POPULATION PAR TRANCHE D'ÂGE
# ===========================================================================
cat("\n=== POPULATION PAR TRANCHE ===\n")

GROUPES_ATTENDUS <- c("pop_0_9","pop_10_19","pop_20_39","pop_40_59",
                      "pop_60_64","pop_65_69","pop_70_74","pop_75_79","pop_80_plus")

population_age <- population_interpolee[
  SEXE %in% c("Homme","Femme") & AGE_QUINQUENNAL != "TOTAL" & !is.na(COM)
][, groupe_age := fcase(
  AGE_QUINQUENNAL %in% c("0-4","5-9"),                        "pop_0_9",
  AGE_QUINQUENNAL %in% c("10-14","15-19"),                    "pop_10_19",
  AGE_QUINQUENNAL %in% c("20-24","25-29","30-34","35-39"),    "pop_20_39",
  AGE_QUINQUENNAL %in% c("40-44","45-49","50-54","55-59"),    "pop_40_59",
  AGE_QUINQUENNAL == "60-64",                                  "pop_60_64",
  AGE_QUINQUENNAL == "65-69",                                  "pop_65_69",
  AGE_QUINQUENNAL == "70-74",                                  "pop_70_74",
  AGE_QUINQUENNAL == "75-79",                                  "pop_75_79",
  AGE_QUINQUENNAL %in% c("80-84","85-89","90-94","95+"),      "pop_80_plus",
  default = NA_character_
)][!is.na(groupe_age)][,
                       .(POPULATION = sum(POPULATION, na.rm = TRUE)),
                       by = .(COM, year = YEAR, groupe_age)
] %>%
  pivot_wider(names_from = groupe_age, values_from = POPULATION, values_fill = 0L)

setDT(population_age)
population_age[, pop_75_plus := pop_75_79 + pop_80_plus]

cols_manquantes <- setdiff(GROUPES_ATTENDUS, names(population_age))
if (length(cols_manquantes) > 0) {
  stop(paste("Colonnes manquantes :", paste(cols_manquantes, collapse = ", ")))
}
cat(sprintf("Lignes population_age          : %d\n", nrow(population_age)))
cat("Toutes les tranches d'âge présentes.\n")

# ===========================================================================
# FUSION POPULATION
# ===========================================================================
cat("\n=== FUSION POPULATION ===\n")

df <- merge(base, population_total, by = c("COM", "year"), all.x = TRUE)
df <- merge(df,   population_age,   by = c("COM", "year"), all.x = TRUE)

cat(sprintf("Lignes après fusion           : %d\n", nrow(df)))
cat(sprintf("Avec population_totale        : %d\n", sum(!is.na(df$population_totale))))
cat(sprintf("Sans population_totale        : %d\n", sum( is.na(df$population_totale))))

n_corse_df <- length(unique(df$COM[grepl("^2[AB]|^20", df$COM)]))
if (n_corse_df == 0) cat("✅ Corse absente de df final.\n") else cat(sprintf("❌ %d communes Corse dans df final !\n", n_corse_df))

cat("\n--- Couverture population par année ---\n")
print(df[, .(
  total    = .N,
  avec_pop = sum(!is.na(population_totale)),
  taux     = round(sum(!is.na(population_totale)) / .N * 100, 1)
), by = year][order(year)])

# ===========================================================================
# TAUX DE MORTALITÉ (pour 10 000 habitants)
# ===========================================================================
cat("\n=== CALCUL TAUX DE MORTALITÉ ===\n")

age_cols <- c("0-9","10-19","20-39","40-59","60-64","65-69","70-74","75-79","80+")

manquants_deces <- setdiff(age_cols, names(df))
if (length(manquants_deces) > 0) {
  cat("ATTENTION colonnes décès manquantes :", paste(manquants_deces, collapse=", "), "\n")
} else {
  cat("Toutes les colonnes décès présentes.\n")
}

df[, deces_total := rowSums(.SD, na.rm = TRUE), .SDcols = age_cols]

df[, taux_mortalite_total   := ifelse(!is.na(population_totale) & population_totale > 0,
                                      deces_total / population_totale * 10000, NA_real_)]
df[, taux_mortalite_75_plus := ifelse(!is.na(pop_75_plus) & pop_75_plus > 0,
                                      (`75-79` + `80+`) / pop_75_plus * 10000, NA_real_)]
df[, taux_mortalite_80_plus := ifelse(!is.na(pop_80_plus) & pop_80_plus > 0,
                                      `80+` / pop_80_plus * 10000, NA_real_)]
df[, taux_mortalite_75_79   := ifelse(!is.na(pop_75_79) & pop_75_79 > 0,
                                      `75-79` / pop_75_79 * 10000, NA_real_)]
df[, taux_mortalite_70_74   := ifelse(!is.na(pop_70_74) & pop_70_74 > 0,
                                      `70-74` / pop_70_74 * 10000, NA_real_)]
df[, taux_mortalite_65_69   := ifelse(!is.na(pop_65_69) & pop_65_69 > 0,
                                      `65-69` / pop_65_69 * 10000, NA_real_)]
df[, taux_mortalite_60_64   := ifelse(!is.na(pop_60_64) & pop_60_64 > 0,
                                      `60-64` / pop_60_64 * 10000, NA_real_)]
df[, taux_mortalite_40_59   := ifelse(!is.na(pop_40_59) & pop_40_59 > 0,
                                      `40-59` / pop_40_59 * 10000, NA_real_)]
df[, taux_mortalite_20_39   := ifelse(!is.na(pop_20_39) & pop_20_39 > 0,
                                      `20-39` / pop_20_39 * 10000, NA_real_)]
df[, taux_mortalite_10_19   := ifelse(!is.na(pop_10_19) & pop_10_19 > 0,
                                      `10-19` / pop_10_19 * 10000, NA_real_)]
df[, taux_mortalite_0_9     := ifelse(!is.na(pop_0_9) & pop_0_9 > 0,
                                      `0-9` / pop_0_9 * 10000, NA_real_)]

# ===========================================================================
# NETTOYAGE : taux > 10 000 → NA
# ===========================================================================
cat("\n=== NETTOYAGE TAUX ABERRANTS (> 10 000) ===\n")

taux_cols <- grep("^taux_mortalite", names(df), value = TRUE)
for (col in taux_cols) {
  n_aberrants <- sum(df[[col]] > 10000, na.rm = TRUE)
  if (n_aberrants > 0) {
    df[get(col) > 10000, (col) := NA_real_]
    cat(sprintf("%-35s : %d valeurs → NA\n", col, n_aberrants))
  }
}

cat(sprintf("Lignes avec taux_mortalite_total : %d\n",
            sum(!is.na(df$taux_mortalite_total))))

# ===========================================================================
# DATASETS FINAUX
# ===========================================================================
df[, value_estimated_population := population_totale]

df_final     <- df
df_final_reg <- df[!is.na(value_estimated_population)]

# ===========================================================================
# CORRECTION DOUBLONS
# ===========================================================================
cat("\n=== CORRECTION DOUBLONS ===\n")

# Détecter le nom exact de la colonne mois
col_mois <- intersect(c("month", "mois"), names(df_final_reg))
cat(sprintf("Colonne mois détectée : %s\n", col_mois))

n_avant <- nrow(df_final_reg)
dup <- df_final_reg[, .N, by = c("COM", "year", col_mois)][N > 1]
cat(sprintf("Doublons détectés COM × year × %s : %d\n", col_mois, nrow(dup)))

df_final_reg <- unique(df_final_reg, by = c("COM", "year", col_mois))
df_final     <- unique(df_final,     by = c("COM", "year", col_mois))

cat(sprintf("Lignes supprimées               : %d\n", n_avant - nrow(df_final_reg)))
cat(sprintf("Lignes restantes df_final_reg   : %d\n", nrow(df_final_reg)))

dup_apres <- df_final_reg[, .N, by = c("COM", "year", col_mois)][N > 1]
cat(sprintf("Doublons restants               : %d\n", nrow(dup_apres)))

# ===========================================================================
# FILTRE POPULATION > 0
# ===========================================================================
cat("\n=== FILTRE POPULATION > 0 ===\n")

n_avant <- nrow(df_final_reg)

# Supprimer df_final pour libérer la RAM — on ne garde que df_final_reg
rm(df_final)
gc()

df_final_reg <- df_final_reg[population_totale > 0]

cat(sprintf("Lignes supprimées (pop = 0)     : %d\n", n_avant - nrow(df_final_reg)))
cat(sprintf("Lignes restantes df_final_reg   : %d\n", nrow(df_final_reg)))
cat(sprintf("Pop min dans df_final_reg       : %.0f\n",
            min(df_final_reg$population_totale, na.rm = TRUE)))

# df_final == df_final_reg ici (on a déjà filtré sur !is.na(population_totale) avant)
df_final <- df_final_reg

# ===========================================================================
# EXPORT
# ===========================================================================
cat("\n=== EXPORT ===\n")

fwrite(df_final,     file.path(BASE_FINAL, "df_final.csv"))
fwrite(df_final_reg, file.path(BASE_FINAL, "df_final_reg.csv"))

cat(sprintf("df_final     : %d lignes × %d colonnes\n",
            nrow(df_final), ncol(df_final)))
cat(sprintf("df_final_reg : %d lignes × %d colonnes\n",
            nrow(df_final_reg), ncol(df_final_reg)))
cat(sprintf("Années       : %d - %d\n", min(df_final$year), max(df_final$year)))
cat(sprintf("Communes     : %d\n", length(unique(df_final$COM))))
cat("Attendu      : ~16.2M lignes, ~34,310 communes (hors Corse), 1980-2019\n")

# ===========================================================================
# VÉRIFICATION FINALE
# ===========================================================================
cat("\n=== VÉRIFICATION FINALE ===\n")
cat("Colonnes df_final_reg :\n")
print(names(df_final_reg))

cat(sprintf("\nTaux mortalité total — médiane : %.2f | max : %.2f\n",
            median(df_final_reg$taux_mortalite_total, na.rm = TRUE),
            max(df_final_reg$taux_mortalite_total,    na.rm = TRUE)))

tg_cols <- grep("^tg_tbin", names(df_final_reg), value = TRUE)
if (length(tg_cols) > 0) {
  tg_sum <- rowSums(df_final_reg[, ..tg_cols], na.rm = TRUE)
  cat(sprintf("Somme bins tg — min: %.2f | médiane: %.2f | max: %.2f\n",
              min(tg_sum), median(tg_sum), max(tg_sum)))
  cat("Attendu : 30 partout\n")
}

if ("V12" %in% names(df_final_reg)) {
  df_final_reg[, V12 := NULL]
  df_final[, V12 := NULL]
  cat("Colonne V12 supprimée.\n")
}

cat("\n--- Distribution taux_mortalite_total ---\n")
cat(sprintf("= 0          : %d (%.1f%%)\n",
            sum(df_final_reg$taux_mortalite_total == 0, na.rm = TRUE),
            sum(df_final_reg$taux_mortalite_total == 0, na.rm = TRUE) / nrow(df_final_reg) * 100))
cat(sprintf("> 1000       : %d\n",
            sum(df_final_reg$taux_mortalite_total > 1000, na.rm = TRUE)))
cat(sprintf("Percentiles  : p25=%.1f | p50=%.1f | p75=%.1f | p99=%.1f\n",
            quantile(df_final_reg$taux_mortalite_total, 0.25, na.rm = TRUE),
            quantile(df_final_reg$taux_mortalite_total, 0.50, na.rm = TRUE),
            quantile(df_final_reg$taux_mortalite_total, 0.75, na.rm = TRUE),
            quantile(df_final_reg$taux_mortalite_total, 0.99, na.rm = TRUE)))

cat("\n--- Distribution post-nettoyage par taux ---\n")
for (col in taux_cols) {
  q <- quantile(df_final_reg[[col]], c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  cat(sprintf("%-35s min=%.1f | Q1=%.1f | median=%.1f | Q3=%.1f | max=%.1f\n",
              col, q[1], q[2], q[3], q[4], q[5]))
}

cat("\n--- Distribution population_totale ---\n")
cat(sprintf("< 100 hab    : %d obs\n",
            sum(df_final_reg$population_totale < 100, na.rm = TRUE)))
cat(sprintf("Médiane pop  : %.0f\n",
            median(df_final_reg$population_totale, na.rm = TRUE)))

cat(sprintf("\nDoublons restants COM × year × %s : %d\n",
            col_mois,
            nrow(df_final_reg[, .N, by = c("COM", "year", col_mois)][N > 1])))

fwrite(df_final_reg, file.path(BASE_FINAL, "df_final_reg.csv"))
fwrite(df_final,     file.path(BASE_FINAL, "df_final.csv"))
cat("\n>>> Fichiers mis à jour.\n")






