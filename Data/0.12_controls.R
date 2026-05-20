################################################################################
# Extraction médecins généralistes par commune (1998-2019) + Fusion
################################################################################

library(data.table)
library(dplyr)
library(readr)
library(zoo)

chemin_base <- "C:/Users/simon/Desktop/master_thesis/soins"
annees <- 1998:2019
liste_medecins <- list()

################################################################################
# TRAITEMENT 1998
################################################################################

cat("Traitement de l'année 1998...\n")
fichier_1998 <- file.path(chemin_base, "ic98.csv")

if (file.exists(fichier_1998)) {
  data_1998 <- fread(fichier_1998, encoding = "Latin-1")
  names(data_1998) <- toupper(names(data_1998))
  
  medecins_1998 <- data_1998 %>%
    select(DEPCOM, G2MG) %>%
    rename(nb_medecins = G2MG) %>%
    mutate(
      annee = 1998,
      DEPCOM = as.character(DEPCOM),
      nb_medecins = as.numeric(nb_medecins)
    ) %>%
    filter(!is.na(nb_medecins) & nb_medecins > 0) %>%
    select(annee, DEPCOM, nb_medecins)
  
  liste_medecins[[1]] <- medecins_1998
  cat("  -> 1998: ", nrow(medecins_1998), " communes avec médecins\n")
} else {
  cat("  -> ATTENTION: Fichier 1998 introuvable\n")
}

################################################################################
# TRAITEMENT 2007-2019
################################################################################

for (annee in 2007:2019) {
  cat("Traitement de l'année ", annee, "...\n", sep = "")
  
  annee_court <- substr(as.character(annee), 3, 4)
  fichier <- file.path(chemin_base, paste0("bpe", annee_court, "_ensemble.csv"))
  
  if (file.exists(fichier)) {
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
    cat("  -> ", annee, ": ", nrow(medecins_annee), " communes avec médecins\n", sep = "")
  } else {
    cat("  -> ATTENTION: Fichier ", annee, " introuvable\n", sep = "")
  }
}

################################################################################
# CONSOLIDATION
################################################################################

cat("\nConsolidation des données...\n")
medecins_final <- bind_rows(liste_medecins) %>% arrange(annee, DEPCOM)

cat("\n========================================\n")
cat("RÉSUMÉ DES DONNÉES EXTRAITES\n")
cat("========================================\n")
cat("Nombre total de lignes: ", nrow(medecins_final), "\n")
cat("Période couverte: ", min(medecins_final$annee), "-", max(medecins_final$annee), "\n")
cat("Nombre de communes uniques: ", n_distinct(medecins_final$DEPCOM), "\n\n")

cat("Nombre de communes par année:\n")
print(
  medecins_final %>%
    group_by(annee) %>%
    summarise(
      nb_communes = n(),
      nb_medecins_total = sum(nb_medecins, na.rm = TRUE),
      nb_medecins_moyen = round(mean(nb_medecins, na.rm = TRUE), 2),
      .groups = "drop"
    )
)

fichier_sortie <- file.path(chemin_base, "medecins_generalistes_1998_2019.csv")
fwrite(medecins_final, fichier_sortie, sep = ";", dec = ",")
cat("\n✓ Fichier exporté: ", fichier_sortie, "\n")

################################################################################
# INTERPOLATION LINÉAIRE (1998-2019)
################################################################################

cat("\n========================================\n")
cat("INTERPOLATION LINÉAIRE\n")
cat("========================================\n")

communes_uniques <- unique(medecins_final$DEPCOM)

panel_complet <- expand.grid(
  DEPCOM = communes_uniques,
  annee = 1998:2019,
  stringsAsFactors = FALSE
) %>%
  left_join(medecins_final, by = c("DEPCOM", "annee")) %>%
  arrange(DEPCOM, annee)

cat("Panel complet créé: ", nrow(panel_complet), " observations\n")
cat("Avant interpolation - lignes avec données: ", sum(!is.na(panel_complet$nb_medecins)), "\n")
cat("Avant interpolation - lignes manquantes: ", sum(is.na(panel_complet$nb_medecins)), "\n\n")

medecins_interpole <- panel_complet %>%
  group_by(DEPCOM) %>%
  mutate(
    nb_medecins_interpole = zoo::na.approx(nb_medecins, na.rm = FALSE),
    donnee_observee = ifelse(is.na(nb_medecins), 0, 1)
  ) %>%
  ungroup() %>%
  mutate(nb_medecins_interpole = round(nb_medecins_interpole, 0))

cat("Après interpolation - lignes avec valeurs: ", sum(!is.na(medecins_interpole$nb_medecins_interpole)), "\n")
cat("Après interpolation - lignes encore manquantes: ", sum(is.na(medecins_interpole$nb_medecins_interpole)), "\n\n")

stats_interpol <- medecins_interpole %>%
  group_by(annee) %>%
  summarise(
    nb_observees = sum(donnee_observee == 1, na.rm = TRUE),
    nb_interpolees = sum(donnee_observee == 0 & !is.na(nb_medecins_interpole), na.rm = TRUE),
    nb_manquantes = sum(is.na(nb_medecins_interpole)),
    .groups = "drop"
  )

cat("Statistiques d'interpolation par année:\n")
print(stats_interpol)

fichier_interpole <- file.path(chemin_base, "medecins_generalistes_1998_2019_interpole.csv")
fwrite(medecins_interpole, fichier_interpole, sep = ";", dec = ",")
cat("\n✓ Fichier interpolé exporté: ", fichier_interpole, "\n")

################################################################################
# FUSION AVEC df_final_reg_complet.csv
################################################################################

cat("\n========================================\n")
cat("FUSION AVEC df_final_reg_complet.csv\n")
cat("========================================\n")

chemin_final_data <- "C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet2.csv"

if (file.exists(chemin_final_data)) {
  
  df_final_reg <- fread(chemin_final_data, encoding = "Latin-1")
  
  cat("Fichier de régression chargé:\n")
  cat("  - Lignes: ", nrow(df_final_reg), "\n")
  cat("  - Colonnes: ", ncol(df_final_reg), "\n\n")
  
  # Supprimer les colonnes médecins si elles existent déjà
  df_final_reg <- df_final_reg %>%
    select(-any_of(c("nb_medecins", "medecins_observed", "medecins_par_hab")))
  
  medecins_pour_fusion <- medecins_interpole %>%
    select(DEPCOM, annee, nb_medecins_interpole, donnee_observee) %>%
    rename(
      COM = DEPCOM,
      year = annee,
      nb_medecins = nb_medecins_interpole,
      medecins_observed = donnee_observee
    )
  
  # Convertir COM en character AVANT tout
  df_final_reg <- df_final_reg %>% mutate(COM = as.character(COM))
  medecins_pour_fusion <- medecins_pour_fusion %>% mutate(COM = as.character(COM))
  
  # Convertir toutes les colonnes character en numeric (sauf COM, COM_char, et variables catégorielles)
  cols_to_numeric <- names(df_final_reg)[sapply(df_final_reg, is.character)]
  cols_to_numeric <- setdiff(cols_to_numeric, c("COM", "COM_char", "vig_phenomene", "vig_niveau","n_jours_bin","dose_bin",
                                                "share_bin","taux_clim_RG_bin","taux_clim_DEP_bin"))
  
  df_final_reg <- df_final_reg %>%
    mutate(across(all_of(cols_to_numeric), ~ as.numeric(gsub(",", ".", .))))
  
  df_final_avec_medecins <- df_final_reg %>%
    left_join(medecins_pour_fusion, by = c("COM", "year"))
  
  cat("Fusion effectuée:\n")
  cat("  - Lignes dans df_final: ", nrow(df_final_reg), "\n")
  cat("  - Lignes après fusion: ", nrow(df_final_avec_medecins), "\n")
  cat("  - Communes avec médecins (>0): ", sum(df_final_avec_medecins$nb_medecins > 0, na.rm = TRUE), "\n")
  cat("  - Communes avec NA: ", sum(is.na(df_final_avec_medecins$nb_medecins)), "\n\n")
  
  # Remplacer NA par 0 et créer médecins par habitant
  df_final_avec_medecins <- df_final_avec_medecins %>%
    mutate(
      nb_medecins = ifelse(is.na(nb_medecins), 0, nb_medecins),
      medecins_par_hab = ifelse(
        is.na(value_estimated_population) | value_estimated_population == 0,
        NA_real_,
        as.numeric(nb_medecins) / as.numeric(value_estimated_population)
      )
    )
  
  fichier_final <- "C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet3.csv"
  fwrite(df_final_avec_medecins, fichier_final, sep = ";", dec = ",")
  
} else {
  cat("⚠ ATTENTION: Fichier df_final_reg_complet.csv introuvable!\n")
  cat("Chemin recherché: ", chemin_final_data, "\n")
}

cat("\n========================================\n")
cat("EXTRACTION ET FUSION TERMINÉES!\n")
cat("========================================\n")
