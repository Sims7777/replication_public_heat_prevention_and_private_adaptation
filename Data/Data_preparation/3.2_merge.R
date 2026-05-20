# ===========================================================================
# BUILD_DF_FINAL_REG.R
# Merge: df_bins_mortality_clim x population → df_final_reg
# ===========================================================================

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)

# ===========================================================================
# PATHS
# ===========================================================================
BASE_THESIS  <- "C:/Users/simon/Desktop/master_thesis"
BASE_FINAL   <- file.path(BASE_THESIS, "final_data")
BASE_POP_OUT <- file.path(BASE_THESIS, "population_harmonized")

# ===========================================================================
# LOAD
# ===========================================================================
base <- fread(file.path(BASE_FINAL, "df_bins_mortality_clim_1980_2019.csv"))
population_interpolee <- fread(
  file.path(BASE_POP_OUT, "population_interpolee_1980_2022.csv")
)

# ===========================================================================
# NORMALIZE COM
# ===========================================================================
base[, COM := as.character(COM)]
base[, COM := trimws(COM)]
base[!grepl("^2[AB]", COM), COM := formatC(as.integer(COM), width = 5, flag = "0")]

base <- base[!grepl("^2[AB]", COM)]
base[, year := as.integer(year)]

population_interpolee[, YEAR := as.integer(YEAR)]
population_interpolee[, COM  := as.character(COM)]
population_interpolee[, COM  := trimws(COM)]

# Keep only valid metropolitan 5-digit codes
population_interpolee <- population_interpolee[
  grepl("^[0-9]{5}$", COM) &
    !grepl("^(97|98|99|20)", COM) &
    !is.na(COM)
]

# Corsica checks
n_corse_base <- length(unique(base$COM[grepl("^2[AB]|^20", base$COM)]))
n_corse_pop  <- length(unique(population_interpolee$COM[grepl("^20", population_interpolee$COM)]))
if (n_corse_base > 0) warning(sprintf("%d Corsican communes in base.", n_corse_base))
if (n_corse_pop  > 0) warning(sprintf("%d Corsican communes in population.", n_corse_pop))

# Normalize unicode dashes in age labels
population_interpolee[, AGE_QUINQUENNAL := gsub("\u2013|\u2014|\u2212", "-",
                                                AGE_QUINQUENNAL)]

# ===========================================================================
# TOTAL POPULATION
# ===========================================================================
population_total <- population_interpolee[
  SEXE == "TOTAL" & AGE_QUINQUENNAL == "TOTAL" & !is.na(COM),
  .(COM, year = YEAR, population_totale = POPULATION)
]

dup_check <- population_total[, .N, by = .(COM, year)][N > 1]
if (nrow(dup_check) > 0) warning(sprintf("%d duplicates in population_total.", nrow(dup_check)))

# ===========================================================================
# POPULATION BY AGE GROUP
# ===========================================================================
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
  stop(paste("Missing age columns:", paste(cols_manquantes, collapse = ", ")))
}

# ===========================================================================
# MERGE POPULATION
# ===========================================================================
df <- merge(base, population_total, by = c("COM", "year"), all.x = TRUE)
df <- merge(df,   population_age,   by = c("COM", "year"), all.x = TRUE)

n_corse_df <- length(unique(df$COM[grepl("^2[AB]|^20", df$COM)]))
if (n_corse_df > 0) warning(sprintf("%d Corsican communes in merged df.", n_corse_df))

# ===========================================================================
# MORTALITY RATES (per 10,000 inhabitants)
# ===========================================================================
age_cols <- c("0-9","10-19","20-39","40-59","60-64","65-69","70-74","75-79","80+")

manquants_deces <- setdiff(age_cols, names(df))
if (length(manquants_deces) > 0) {
  stop(paste("Missing death columns:", paste(manquants_deces, collapse = ", ")))
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
# CLEAN ABERRANT RATES (> 10,000 → NA)
# ===========================================================================
taux_cols <- grep("^taux_mortalite", names(df), value = TRUE)
for (col in taux_cols) {
  n_aberrants <- sum(df[[col]] > 10000, na.rm = TRUE)
  if (n_aberrants > 0) df[get(col) > 10000, (col) := NA_real_]
}

# ===========================================================================
# BUILD FINAL DATASETS
# ===========================================================================
df[, value_estimated_population := population_totale]

df_final_reg <- df[!is.na(value_estimated_population)]

# ===========================================================================
# DEDUPLICATE
# ===========================================================================
col_mois <- intersect(c("month", "mois"), names(df_final_reg))

n_avant <- nrow(df_final_reg)
df_final_reg <- unique(df_final_reg, by = c("COM", "year", col_mois))
df           <- unique(df,           by = c("COM", "year", col_mois))

dup_apres <- df_final_reg[, .N, by = c("COM", "year", col_mois)][N > 1]
if (nrow(dup_apres) > 0) warning(sprintf("%d duplicates remain after deduplication.", nrow(dup_apres)))

# ===========================================================================
# FILTER POPULATION > 0
# ===========================================================================
n_avant <- nrow(df_final_reg)
df_final_reg <- df_final_reg[population_totale > 0]

rm(df); gc()

df_final <- df_final_reg

# Drop spurious column if present
if ("V12" %in% names(df_final_reg)) {
  df_final_reg[, V12 := NULL]
  df_final[, V12 := NULL]
}

# ===========================================================================
# EXPORT
# ===========================================================================
fwrite(df_final,     file.path(BASE_FINAL, "df_final.csv"))
fwrite(df_final_reg, file.path(BASE_FINAL, "df_final_reg.csv"))
