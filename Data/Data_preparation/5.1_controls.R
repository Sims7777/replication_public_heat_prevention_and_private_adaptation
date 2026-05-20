library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(zoo)

# ==============================================================================
# CONFIGURATION
# ==============================================================================
BASE_THESIS <- "C:/Users/simon/Desktop/master_thesis"
BASE_FINAL  <- file.path(BASE_THESIS, "final_data")

gc()

# ==============================================================================
# 1. LOAD CSP DATA FROM INSEE EXCEL
# ==============================================================================
base_path <- file.path(BASE_THESIS, "socio")
file_path <- file.path(base_path, "pop-act2554-csp-cd-6822.xlsx")

load_csp_year <- function(file_path, sheet_name, year_label) {

  df <- read_excel(file_path, sheet = sheet_name, skip = 15)

  names(df) <- c(
    "region", "dep", "com", "stable", "dep_2024", "libelle",
    "agri_emploi", "agri_chomeur",
    "artisan_emploi", "artisan_chomeur",
    "cadre_emploi", "cadre_chomeur",
    "prof_inter_emploi", "prof_inter_chomeur",
    "employe_emploi", "employe_chomeur",
    "ouvrier_emploi", "ouvrier_chomeur"
  )

  setDT(df)
  df[, COM := sprintf("%02d%03d", as.integer(dep), as.integer(com))]

  df[, `:=`(
    total_actifs_occupe = agri_emploi + artisan_emploi + cadre_emploi +
      prof_inter_emploi + employe_emploi + ouvrier_emploi,
    total_chomeurs = agri_chomeur + artisan_chomeur + cadre_chomeur +
      prof_inter_chomeur + employe_chomeur + ouvrier_chomeur
  )]
  df[, total_actifs := total_actifs_occupe + total_chomeurs]

  df[, `:=`(
    agriculteur       = agri_emploi      / total_actifs_occupe * 100,
    artisan_comm_chef = artisan_emploi   / total_actifs_occupe * 100,
    cadre             = cadre_emploi     / total_actifs_occupe * 100,
    prof_inter        = prof_inter_emploi / total_actifs_occupe * 100,
    employe           = employe_emploi   / total_actifs_occupe * 100,
    ouvrier           = ouvrier_emploi   / total_actifs_occupe * 100,
    taux_chomage      = total_chomeurs   / total_actifs * 100,
    annee             = year_label,
    csp_ensemble      = total_actifs_occupe
  )]

  df_final <- df[, .(COM, annee, taux_chomage, agriculteur, artisan_comm_chef,
                     cadre, prof_inter, employe, ouvrier, csp_ensemble)]
  df_final <- df_final[!is.na(COM) & !is.na(csp_ensemble) & csp_ensemble > 0]

  return(df_final)
}

csp_1990 <- load_csp_year(file_path, "COM_1990", 1990)
csp_1999 <- load_csp_year(file_path, "COM_1999", 1999)
csp_2006 <- load_csp_year(file_path, "COM_2006", 2006)
csp_2011 <- load_csp_year(file_path, "COM_2011", 2011)
csp_2016 <- load_csp_year(file_path, "COM_2016", 2016)

socio_panel <- rbindlist(list(csp_1990, csp_1999, csp_2006, csp_2011, csp_2016))
setorder(socio_panel, COM, annee)

# ==============================================================================
# 2. LINEAR INTERPOLATION CSP 1990-2019 (WITH EXTRAPOLATION)
# ==============================================================================
vars_csp <- c("taux_chomage", "agriculteur", "artisan_comm_chef", "cadre",
              "prof_inter", "employe", "ouvrier", "csp_ensemble")

interpoler_commune <- function(data_commune) {
  annees_completes <- data.frame(annee = 1990:2019)
  data_complete    <- merge(annees_completes, data_commune, by = "annee", all.x = TRUE)
  setDT(data_complete)
  setorder(data_complete, annee)

  for (var in vars_csp) {
    if (var %in% names(data_complete)) {
      data_complete[, (var) := zoo::na.approx(get(var), rule = 2, na.rm = FALSE)]
    }
  }
  return(data_complete)
}

socio_panel_interpolated <- socio_panel %>%
  group_by(COM) %>%
  group_modify(~ interpoler_commune(.x)) %>%
  ungroup()

setDT(socio_panel_interpolated)

# ==============================================================================
# 3. EXTRACT TFPB
# ==============================================================================
extraire_tfpb <- function(annee) {
  chemin_base <- file.path(BASE_THESIS, "tax")

  fichiers_possibles <- c(
    file.path(chemin_base, paste0("rei_",  annee, ".xlsx")),
    file.path(chemin_base, paste0("rei-",  annee, ".xlsx")),
    file.path(chemin_base, paste0("REI_",  annee, ".xlsx"))
  )

  fichier <- NULL
  for (f in fichiers_possibles) {
    if (file.exists(f)) { fichier <- f; break }
  }
  if (is.null(fichier)) return(NULL)

  sheets <- excel_sheets(fichier)
  feuilles_possibles <- c(paste0("REI_", annee), paste0("REI ", annee),
                           paste0("REI-", annee), "REI", sheets[1])

  sheet_name <- NULL
  for (s in feuilles_possibles) {
    if (s %in% sheets) { sheet_name <- s; break }
  }
  if (is.null(sheet_name)) return(NULL)

  data <- read_excel(fichier, sheet = sheet_name)

  # Build COM code
  if (annee < 2000) {
    col_commune <- NULL
    for (nom in c("DEPCOM","depcom","CODE_COMMUNE","code_commune","CODGEO","codgeo")) {
      if (nom %in% names(data)) { col_commune <- nom; break }
    }
    if (is.null(col_commune)) return(NULL)
    data$COM <- data[[col_commune]]
  } else {
    col_dep <- col_com <- NULL
    for (nom in c("DEPARTEMENT","DEP","dep","DEPT","dept")) {
      if (nom %in% names(data)) { col_dep <- nom; break }
    }
    for (nom in c("COMMUNE","COM","com","commune")) {
      if (nom %in% names(data)) { col_com <- nom; break }
    }
    if (is.null(col_dep) || is.null(col_com)) return(NULL)
    data$COM <- sprintf("%02d%03d", as.integer(data[[col_dep]]), as.integer(data[[col_com]]))
  }

  # Locate TFPB base column
  col_tfpb <- NULL
  for (nom in c("FNB - COMMUNE / BASE NETTE","B00","b00","BASE","base","TFPB","tfpb")) {
    if (nom %in% names(data)) { col_tfpb <- nom; break }
  }
  if (is.null(col_tfpb)) return(NULL)

  result <- data.frame(
    COM       = data$COM,
    base_tfpb = data[[col_tfpb]],
    annee     = annee
  ) %>%
    filter(!is.na(COM) & !is.na(base_tfpb))

  return(result)
}

tfpb_list <- list()
for (a in 1990:2019) {
  result <- tryCatch(extraire_tfpb(a), error = function(e) NULL)
  if (!is.null(result)) tfpb_list[[as.character(a)]] <- result
}

tfpb_final <- bind_rows(tfpb_list)
setDT(tfpb_final)

# ==============================================================================
# 4. EXTRACT EMERGENCY ACCESS TIMES
# ==============================================================================
urgences_2015 <- read_excel(
  file.path(BASE_THESIS, "acces_urgence/acces_urgence_2015.xls"),
  sheet = "BASECOM_URGENCES_2015", skip = 5
) %>%
  select(COM = `Code commune Insee`, acces_urgence = tps_SU_SMUR_MCS) %>%
  mutate(COM = as.character(COM), annee = 2015L)

urgences_2019 <- read_excel(
  file.path(BASE_THESIS, "acces_urgence/acces_urgence_2019.xlsx"),
  sheet = "BASECOM_URGENCES_2019", skip = 5
) %>%
  select(COM = `Code commune Insee`, acces_urgence = tps_SU_SMUR_MCS) %>%
  mutate(COM = as.character(COM), annee = 2019L)

urgences_panel <- rbind(urgences_2015, urgences_2019)
setDT(urgences_panel)

# ==============================================================================
# 5. LOAD REGRESSION BASE
# ==============================================================================
df_reg <- fread(file.path(BASE_FINAL, "df_final_reg_logement_pnc_bins.csv"))

if ("year" %in% names(df_reg) && !("annee" %in% names(df_reg))) {
  setnames(df_reg, "year", "annee")
}

# ==============================================================================
# 6. NORMALIZE TYPES
# ==============================================================================
df_reg[,                    COM := as.character(COM)]; df_reg[,                    annee := as.integer(annee)]
socio_panel_interpolated[,  COM := as.character(COM)]; socio_panel_interpolated[,  annee := as.integer(annee)]
tfpb_final[,                COM := as.character(COM)]; tfpb_final[,                annee := as.integer(annee)]
urgences_panel[,            COM := as.character(COM)]; urgences_panel[,            annee := as.integer(annee)]

# ==============================================================================
# 7. MERGE
# ==============================================================================
df_merged <- merge(df_reg,    socio_panel_interpolated, by = c("COM", "annee"), all.x = TRUE)
df_merged <- merge(df_merged, tfpb_final,               by = c("COM", "annee"), all.x = TRUE)
df_merged <- merge(df_merged, urgences_panel,           by = c("COM", "annee"), all.x = TRUE)

# ==============================================================================
# 8. RESOLVE .x/.y CONFLICTS
# ==============================================================================
vars_base <- c("taux_chomage", "agriculteur", "artisan_comm_chef", "cadre",
               "prof_inter", "employe", "ouvrier", "csp_ensemble")

for (v in vars_base) {
  col_x <- paste0(v, ".x")
  col_y <- paste0(v, ".y")
  if (col_x %in% names(df_merged) && col_y %in% names(df_merged)) {
    df_merged[, (v) := fifelse(is.na(get(col_y)), get(col_x), get(col_y))]
    df_merged[, c(col_x, col_y) := NULL]
  }
}

# ==============================================================================
# 9. DERIVED VARIABLES
# ==============================================================================
df_merged[, `:=`(
  pct_csp_haute = cadre + prof_inter,
  pct_csp_basse = ouvrier + employe,
  tfpb          = base_tfpb / value_estimated_population
)]

# ==============================================================================
# 10. RENAME AND CLEAN
# ==============================================================================
if ("RG_final" %in% names(df_merged)) setnames(df_merged, "RG_final", "RG")
setnames(df_merged, "annee", "year")

cols_supprimer <- c("is_corse", "n_chomeurs", "pct_chomeurs", "interp_pct_chomeurs")
cols_supprimer <- cols_supprimer[cols_supprimer %in% names(df_merged)]
if (length(cols_supprimer) > 0) df_merged[, (cols_supprimer) := NULL]

# ==============================================================================
# EXPORT
# ==============================================================================
fwrite(df_merged, file.path(BASE_FINAL, "df_final_reg_complet1.csv"))library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(zoo)

# ==============================================================================
# 0. CONFIGURATION
# ==============================================================================

BASE_THESIS <- "C:/Users/simon/Desktop/master_thesis"
BASE_FINAL  <- file.path(BASE_THESIS, "final_data")

cat("=== FUSION SOCIO + TAX + URGENCES ===\n\n")
gc()

# ==============================================================================
# 1. CHARGER LES DONNÉES CSP DEPUIS LE FICHIER EXCEL INSEE
# ==============================================================================

base_path <- file.path(BASE_THESIS, "socio")
file_path <- file.path(base_path, "pop-act2554-csp-cd-6822.xlsx")

load_csp_year <- function(file_path, sheet_name, year_label) {
  cat("Chargement de", sheet_name, "...\n")
  
  df <- read_excel(file_path, sheet = sheet_name, skip = 15)
  
  names(df) <- c(
    'region', 'dep', 'com', 'stable', 'dep_2024', 'libelle',
    'agri_emploi', 'agri_chomeur',
    'artisan_emploi', 'artisan_chomeur',
    'cadre_emploi', 'cadre_chomeur',
    'prof_inter_emploi', 'prof_inter_chomeur',
    'employe_emploi', 'employe_chomeur',
    'ouvrier_emploi', 'ouvrier_chomeur'
  )
  
  setDT(df)
  df[, COM := sprintf("%02d%03d", as.integer(dep), as.integer(com))]
  
  df[, `:=`(
    total_actifs_occupe = agri_emploi + artisan_emploi + cadre_emploi + 
      prof_inter_emploi + employe_emploi + ouvrier_emploi,
    total_chomeurs = agri_chomeur + artisan_chomeur + cadre_chomeur + 
      prof_inter_chomeur + employe_chomeur + ouvrier_chomeur
  )]
  
  df[, total_actifs := total_actifs_occupe + total_chomeurs]
  
  df[, `:=`(
    agriculteur = agri_emploi / total_actifs_occupe * 100,
    artisan_comm_chef = artisan_emploi / total_actifs_occupe * 100,
    cadre = cadre_emploi / total_actifs_occupe * 100,
    prof_inter = prof_inter_emploi / total_actifs_occupe * 100,
    employe = employe_emploi / total_actifs_occupe * 100,
    ouvrier = ouvrier_emploi / total_actifs_occupe * 100,
    taux_chomage = total_chomeurs / total_actifs * 100,
    annee = year_label,
    csp_ensemble = total_actifs_occupe
  )]
  
  df_final <- df[, .(
    COM, annee, taux_chomage, agriculteur, artisan_comm_chef, cadre, 
    prof_inter, employe, ouvrier, csp_ensemble
  )]
  
  df_final <- df_final[!is.na(COM) & !is.na(csp_ensemble) & csp_ensemble > 0]
  cat("✓", nrow(df_final), "communes\n")
  
  return(df_final)
}

# Charger les années
csp_1990 <- load_csp_year(file_path, "COM_1990", 1990)
csp_1999 <- load_csp_year(file_path, "COM_1999", 1999)
csp_2006 <- load_csp_year(file_path, "COM_2006", 2006)
csp_2011 <- load_csp_year(file_path, "COM_2011", 2011)
csp_2016 <- load_csp_year(file_path, "COM_2016", 2016)

socio_panel <- rbindlist(list(csp_1990, csp_1999, csp_2006, csp_2011, csp_2016))
setorder(socio_panel, COM, annee)

cat("\nPanel CSP créé avec", nrow(socio_panel), "observations\n")

# ==============================================================================
# 2. INTERPOLATION LINÉAIRE CSP 1990-2019 (AVEC EXTRAPOLATION)
# ==============================================================================

cat("\n=== INTERPOLATION CSP 1990-2019 ===\n")

vars_csp <- c("taux_chomage", "agriculteur", "artisan_comm_chef", "cadre",
              "prof_inter", "employe", "ouvrier", "csp_ensemble")

interpoler_commune <- function(data_commune) {
  annees_completes <- data.frame(annee = 1990:2019)
  data_complete <- merge(annees_completes, data_commune, by = "annee", all.x = TRUE)
  setDT(data_complete)
  setorder(data_complete, annee)
  
  for (var in vars_csp) {
    if (var %in% names(data_complete)) {
      data_complete[, (var) := zoo::na.approx(get(var), rule = 2, na.rm = FALSE)]
    }
  }
  
  return(data_complete)
}

socio_panel_interpolated <- socio_panel %>%
  group_by(COM) %>%
  group_modify(~ interpoler_commune(.x)) %>%
  ungroup()

setDT(socio_panel_interpolated)
cat("✓ Interpolation terminée:", nrow(socio_panel_interpolated), "observations\n")

# ==============================================================================
# 3. EXTRACTION TFPB
# ==============================================================================

cat("\n=== EXTRACTION TFPB ===\n")

extraire_tfpb <- function(annee) {
  chemin_base <- file.path(BASE_THESIS, "tax")
  
  fichiers_possibles <- c(
    file.path(chemin_base, paste0("rei_", annee, ".xlsx")),
    file.path(chemin_base, paste0("rei-", annee, ".xlsx")),
    file.path(chemin_base, paste0("REI_", annee, ".xlsx"))
  )
  
  fichier <- NULL
  for (f in fichiers_possibles) {
    if (file.exists(f)) {
      fichier <- f
      break
    }
  }
  
  if (is.null(fichier)) {
    cat("ERREUR: Aucun fichier trouvé pour", annee, "\n")
    return(NULL)
  }
  
  cat("Lecture:", basename(fichier), "... ")
  
  sheets <- excel_sheets(fichier)
  feuilles_possibles <- c(
    paste0("REI_", annee), paste0("REI ", annee), paste0("REI-", annee),
    "REI", sheets[1]
  )
  
  sheet_name <- NULL
  for (s in feuilles_possibles) {
    if (s %in% sheets) {
      sheet_name <- s
      break
    }
  }
  
  if (is.null(sheet_name)) {
    cat("ERREUR: Aucune feuille valide trouvée\n")
    return(NULL)
  }
  
  data <- read_excel(fichier, sheet = sheet_name)
  
  # Gestion code commune
  if (annee < 2000) {
    col_commune <- NULL
    noms_possibles_commune <- c("DEPCOM", "depcom", "CODE_COMMUNE", "code_commune", "CODGEO", "codgeo")
    for (nom in noms_possibles_commune) {
      if (nom %in% names(data)) {
        col_commune <- nom
        break
      }
    }
    
    if (is.null(col_commune)) {
      cat("ERREUR: Colonne code commune non trouvée\n")
      return(NULL)
    }
    
    data$COM <- data[[col_commune]]
    
  } else {
    col_dep <- NULL
    col_com <- NULL
    
    noms_possibles_dep <- c("DEPARTEMENT", "DEP", "dep", "DEPT", "dept")
    noms_possibles_com <- c("COMMUNE", "COM", "com", "commune")
    
    for (nom in noms_possibles_dep) {
      if (nom %in% names(data)) {
        col_dep <- nom
        break
      }
    }
    
    for (nom in noms_possibles_com) {
      if (nom %in% names(data)) {
        col_com <- nom
        break
      }
    }
    
    if (is.null(col_dep) || is.null(col_com)) {
      cat("ERREUR: Colonnes DEP/COM non trouvées\n")
      return(NULL)
    }
    
    data$COM <- sprintf("%02d%03d", as.integer(data[[col_dep]]), as.integer(data[[col_com]]))
  }
  
  # Gestion base TFPB
  col_tfpb <- NULL
  noms_possibles_tfpb <- c(
    "FNB - COMMUNE / BASE NETTE", "B00", "b00", "BASE", "base", "TFPB", "tfpb"
  )
  
  for (nom in noms_possibles_tfpb) {
    if (nom %in% names(data)) {
      col_tfpb <- nom
      break
    }
  }
  
  if (is.null(col_tfpb)) {
    cat("ERREUR: Colonne base TFPB non trouvée\n")
    return(NULL)
  }
  
  result <- data.frame(
    COM = data$COM,
    base_tfpb = data[[col_tfpb]],
    annee = annee
  ) %>%
    filter(!is.na(COM) & !is.na(base_tfpb))
  
  cat("✓", nrow(result), "communes\n")
  
  return(result)
}

annees <- 1990:2019
tfpb_list <- list()

for (a in annees) {
  result <- tryCatch(
    extraire_tfpb(a),
    error = function(e) {
      cat("ERREUR année", a, ":", e$message, "\n")
      return(NULL)
    }
  )
  if (!is.null(result)) {
    tfpb_list[[as.character(a)]] <- result
  }
}

tfpb_final <- bind_rows(tfpb_list)
setDT(tfpb_final)

cat("\n✓ Total TFPB:", nrow(tfpb_final), "observations extraites\n")

# ==============================================================================
# 4. EXTRACTION TEMPS D'ACCÈS AUX URGENCES
# ==============================================================================

cat("\n=== EXTRACTION URGENCES ===\n")

urgences_2015 <- read_excel(
  file.path(BASE_THESIS, "acces_urgence/acces_urgence_2015.xls"),
  sheet = "BASECOM_URGENCES_2015", skip = 5
)

urgences_2015 <- urgences_2015 %>%
  select(COM = `Code commune Insee`, acces_urgence = tps_SU_SMUR_MCS) %>%
  mutate(COM = as.character(COM), annee = 2015L)

cat("✓ Urgences 2015:", nrow(urgences_2015), "communes\n")

urgences_2019 <- read_excel(
  file.path(BASE_THESIS, "acces_urgence/acces_urgence_2019.xlsx"),
  sheet = "BASECOM_URGENCES_2019", skip = 5
)

urgences_2019 <- urgences_2019 %>%
  select(COM = `Code commune Insee`, acces_urgence = tps_SU_SMUR_MCS) %>%
  mutate(COM = as.character(COM), annee = 2019L)

cat("✓ Urgences 2019:", nrow(urgences_2019), "communes\n")

urgences_panel <- rbind(urgences_2015, urgences_2019)
setDT(urgences_panel)

# ==============================================================================
# 5. CHARGEMENT DF RÉGRESSION
# ==============================================================================

cat("\n=== CHARGEMENT DF RÉGRESSION ===\n")

df_reg <- fread(file.path(BASE_FINAL, "df_final_reg_logement_pnc_bins.csv"))
cat("✓", nrow(df_reg), "observations\n")

if ("year" %in% names(df_reg) && !("annee" %in% names(df_reg))) {
  setnames(df_reg, "year", "annee")
}

# ==============================================================================
# 6. HARMONISATION TYPES AVANT FUSION
# ==============================================================================

cat("\n=== HARMONISATION TYPES ===\n")

df_reg[, COM := as.character(COM)]
df_reg[, annee := as.integer(annee)]

socio_panel_interpolated[, COM := as.character(COM)]
socio_panel_interpolated[, annee := as.integer(annee)]

tfpb_final[, COM := as.character(COM)]
tfpb_final[, annee := as.integer(annee)]

urgences_panel[, COM := as.character(COM)]
urgences_panel[, annee := as.integer(annee)]

# ==============================================================================
# 7. FUSION DES DONNÉES
# ==============================================================================

cat("\n=== FUSION ===\n")

df_merged <- merge(df_reg, socio_panel_interpolated, by = c("COM", "annee"), all.x = TRUE)
cat("Après fusion CSP:", nrow(df_merged), "observations\n")

df_merged <- merge(df_merged, tfpb_final, by = c("COM", "annee"), all.x = TRUE)
cat("Après fusion TFPB:", nrow(df_merged), "observations\n")

df_merged <- merge(df_merged, urgences_panel, by = c("COM", "annee"), all.x = TRUE)
cat("Après fusion urgences:", nrow(df_merged), "observations\n")
# ==============================================================================
# 8. NETTOYAGE DOUBLONS .x/.y
# ==============================================================================

vars_base <- c("taux_chomage", "agriculteur", "artisan_comm_chef", "cadre", 
               "prof_inter", "employe", "ouvrier", "csp_ensemble")

for (v in vars_base) {
  col_x <- paste0(v, ".x")
  col_y <- paste0(v, ".y")
  
  if (col_x %in% names(df_merged) && col_y %in% names(df_merged)) {
    df_merged[, (v) := fifelse(is.na(get(col_y)), get(col_x), get(col_y))]
    df_merged[, c(col_x, col_y) := NULL]
  }
}

# ==============================================================================
# 9. CALCUL VARIABLES DÉRIVÉES
# ==============================================================================

cat("\n=== CALCUL VARIABLES DÉRIVÉES ===\n")

df_merged[, `:=`(
  pct_csp_haute = cadre + prof_inter,
  pct_csp_basse = ouvrier + employe,
  tfpb = base_tfpb / value_estimated_population
)]

cat("✓ Variables créées: pct_csp_haute, pct_csp_basse, tfpb\n")

# ==============================================================================
# 10. RENOMMAGE ET NETTOYAGE FINAL
# ==============================================================================

cat("\n=== RENOMMAGE ET NETTOYAGE ===\n")

if ("RG_final" %in% names(df_merged)) {
  setnames(df_merged, "RG_final", "RG")
}
setnames(df_merged, "annee", "year")

cols_supprimer <- c("is_corse", "n_chomeurs", "pct_chomeurs", "interp_pct_chomeurs")
cols_supprimer_existantes <- cols_supprimer[cols_supprimer %in% names(df_merged)]
if (length(cols_supprimer_existantes) > 0) {
  df_merged[, (cols_supprimer_existantes) := NULL]
  cat("✓ Colonnes supprimées:", paste(cols_supprimer_existantes, collapse = ", "), "\n")
}

# ==============================================================================
# 11. SAUVEGARDE FINALE
# ==============================================================================

output_path <- file.path(BASE_FINAL, "df_final_reg_complet1.csv")
fwrite(df_merged, output_path)

cat("\n✓ Fichier final sauvegardé:", output_path, "\n")
cat("  Dimensions:", nrow(df_merged), "×", ncol(df_merged), "\n")
cat("\n✓ Script terminé avec succès !\n")
