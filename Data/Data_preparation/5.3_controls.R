library(data.table)
library(dplyr)
library(readr)
library(zoo)

# ==============================================================================
# CONFIGURATION
# ==============================================================================
chemin_base       <- "C:/Users/simon/Desktop/master_thesis/soins"
chemin_final_data <- "C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet2.csv"
fichier_final     <- "C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet3.csv"

# ==============================================================================
# 1998: EXTRACT GPs
# ==============================================================================
liste_medecins <- list()

fichier_1998 <- file.path(chemin_base, "ic98.csv")
if (file.exists(fichier_1998)) {
  data_1998 <- fread(fichier_1998, encoding = "Latin-1")
  names(data_1998) <- toupper(names(data_1998))

  medecins_1998 <- data_1998 %>%
    select(DEPCOM, G2MG) %>%
    rename(nb_medecins = G2MG) %>%
    mutate(annee = 1998, DEPCOM = as.character(DEPCOM),
           nb_medecins = as.numeric(nb_medecins)) %>%
    filter(!is.na(nb_medecins) & nb_medecins > 0) %>%
    select(annee, DEPCOM, nb_medecins)

  liste_medecins[[1]] <- medecins_1998
}

# ==============================================================================
# 2007-2019: EXTRACT GPs FROM BPE
# ==============================================================================
for (annee in 2007:2019) {
  annee_court <- substr(as.character(annee), 3, 4)
  fichier     <- file.path(chemin_base, paste0("bpe", annee_court, "_ensemble.csv"))

  if (!file.exists(fichier)) next

  data_annee <- fread(fichier, encoding = "Latin-1")
  names(data_annee) <- toupper(names(data_annee))

  medecins_annee <- data_annee %>%
    filter(TYPEQU == "D201") %>%
    group_by(DEPCOM) %>%
    summarise(nb_medecins = sum(NB_EQUIP, na.rm = TRUE), .groups = "drop") %>%
    mutate(annee = annee, DEPCOM = as.character(DEPCOM)) %>%
    filter(nb_medecins > 0) %>%
    select(annee, DEPCOM, nb_medecins)

  liste_medecins[[length(liste_medecins) + 1]] <- medecins_annee
}

# ==============================================================================
# CONSOLIDATE
# ==============================================================================
medecins_final <- bind_rows(liste_medecins) %>% arrange(annee, DEPCOM)

fwrite(medecins_final,
       file.path(chemin_base, "medecins_generalistes_1998_2019.csv"),
       sep = ";", dec = ",")

# ==============================================================================
# LINEAR INTERPOLATION 1998-2019
# ==============================================================================
communes_uniques <- unique(medecins_final$DEPCOM)

panel_complet <- expand.grid(
  DEPCOM = communes_uniques,
  annee  = 1998:2019,
  stringsAsFactors = FALSE
) %>%
  left_join(medecins_final, by = c("DEPCOM", "annee")) %>%
  arrange(DEPCOM, annee)

medecins_interpole <- panel_complet %>%
  group_by(DEPCOM) %>%
  mutate(
    nb_medecins_interpole = zoo::na.approx(nb_medecins, na.rm = FALSE),
    donnee_observee       = ifelse(is.na(nb_medecins), 0L, 1L)
  ) %>%
  ungroup() %>%
  mutate(nb_medecins_interpole = round(nb_medecins_interpole, 0))

fwrite(medecins_interpole,
       file.path(chemin_base, "medecins_generalistes_1998_2019_interpole.csv"),
       sep = ";", dec = ",")

# ==============================================================================
# MERGE WITH REGRESSION BASE
# ==============================================================================
if (!file.exists(chemin_final_data)) stop("df_final_reg_complet2.csv not found.")

df_final_reg <- fread(chemin_final_data, encoding = "Latin-1")

# Drop any pre-existing GP columns
df_final_reg <- df_final_reg %>%
  select(-any_of(c("nb_medecins", "medecins_observed", "medecins_par_hab")))

# Convert non-categorical character columns to numeric (handles comma decimals)
cols_categoriel <- c("COM", "COM_char", "vig_phenomene", "vig_niveau",
                     "n_jours_bin", "dose_bin", "share_bin",
                     "taux_clim_RG_bin", "taux_clim_DEP_bin")

cols_to_numeric <- setdiff(
  names(df_final_reg)[sapply(df_final_reg, is.character)],
  cols_categoriel
)
df_final_reg <- df_final_reg %>%
  mutate(across(all_of(cols_to_numeric), ~ as.numeric(gsub(",", ".", .))))

df_final_reg <- df_final_reg %>% mutate(COM = as.character(COM))

medecins_pour_fusion <- medecins_interpole %>%
  select(DEPCOM, annee, nb_medecins_interpole, donnee_observee) %>%
  rename(COM = DEPCOM, year = annee,
         nb_medecins = nb_medecins_interpole,
         medecins_observed = donnee_observee) %>%
  mutate(COM = as.character(COM))

df_final_avec_medecins <- df_final_reg %>%
  left_join(medecins_pour_fusion, by = c("COM", "year")) %>%
  mutate(
    nb_medecins = ifelse(is.na(nb_medecins), 0, nb_medecins),
    medecins_par_hab = ifelse(
      is.na(value_estimated_population) | value_estimated_population == 0,
      NA_real_,
      as.numeric(nb_medecins) / as.numeric(value_estimated_population)
    )
  )

# ==============================================================================
# EXPORT
# ==============================================================================
fwrite(df_final_avec_medecins, fichier_final, sep = ";", dec = ",")
