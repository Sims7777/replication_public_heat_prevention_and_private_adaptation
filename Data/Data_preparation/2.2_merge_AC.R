# ===========================================================================
# MERGE_CLIM.R
# Merge : df_bins_mortality_1980_2019 × taux de climatisation (RG + DEP)
# ===========================================================================

library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(readxl)
library(sf)
library(ggplot2)

# ===========================================================================
# CHEMINS
# ===========================================================================
BASE_THESIS   <- "C:/Users/simon/Desktop/master_thesis"
BASE_LOGEMENT <- file.path(BASE_THESIS, "logement")
BASE_FINAL    <- file.path(BASE_THESIS, "final_data")

dir.create(BASE_LOGEMENT, recursive = TRUE, showWarnings = FALSE)

# ===========================================================================
# MAPPING ANCIENNES RÉGIONS → NOUVELLES RÉGIONS (format post-2016)
# ===========================================================================
REGION_MAPPING <- c(
  "11" = "11", "21" = "44", "22" = "32", "23" = "28", "24" = "24",
  "25" = "28", "26" = "27", "31" = "32", "41" = "44", "42" = "44",
  "43" = "27", "52" = "52", "53" = "53", "54" = "75", "72" = "75",
  "73" = "76", "74" = "75", "82" = "84", "83" = "84", "91" = "76",
  "93" = "93", "94" = "94"
)

remap_rg <- function(df) {
  df %>% mutate(
    RG = as.character(RG),
    RG = ifelse(RG %in% names(REGION_MAPPING), REGION_MAPPING[RG], RG)
  )
}

# ===========================================================================
# CHARGEMENT DES DONNÉES LOGEMENT
# ===========================================================================
cat("=== CHARGEMENT DONNÉES LOGEMENT ===\n")

logement2001 <- read.csv(
  file.path(BASE_LOGEMENT, "2001/menage.csv"), sep = ";"
) %>%
  select(DEP, RG, KCLIM1, QEX) %>%
  rename(CLIM = KCLIM1, POIDS = QEX) %>%
  mutate(ANNEE = 2001L, DEP = as.character(DEP))

logement2006 <- read.csv(
  file.path(BASE_LOGEMENT, "2006/logement.csv"), sep = ";"
) %>%
  select(DEP, RG, KCLIM1, QEX) %>%
  rename(CLIM = KCLIM1, POIDS = QEX) %>%
  mutate(ANNEE = 2006L, DEP = as.character(DEP))

logement2013 <- read.csv(
  file.path(BASE_LOGEMENT, "2013/menlog_diff.csv"), sep = ";"
) %>%
  select(RG, KCLIM1, qex, dep1) %>%
  rename(CLIM = KCLIM1, POIDS = qex, DEP = dep1) %>%
  mutate(ANNEE = 2013L, DEP = as.character(DEP))

logement2020 <- read.csv(
  file.path(BASE_LOGEMENT, "2020/menlog.csv"), sep = ","
) %>%
  select(RG, KCLIM1, QEX, DEP_IDF) %>%
  rename(CLIM = KCLIM1, POIDS = QEX, DEP = DEP_IDF) %>%
  mutate(ANNEE = 2020L, DEP = as.character(DEP), RG = as.character(RG))

# ===========================================================================
# REMAPPAGE RÉGIONS
# ===========================================================================
logement2001 <- remap_rg(logement2001)
logement2006 <- remap_rg(logement2006)
logement2013 <- remap_rg(logement2013)

df <- bind_rows(logement2001, logement2006, logement2013, logement2020) %>%
  mutate(CLIM = as.numeric(CLIM), POIDS = as.numeric(POIDS))

# ===========================================================================
# CALCUL DU TAUX DE CLIMATISATION
# ===========================================================================
calc_taux <- function(data, var_geo) {
  data %>%
    filter(!is.na(!!sym(var_geo))) %>%
    mutate(!!var_geo := as.character(!!sym(var_geo))) %>%
    group_by(ANNEE, !!sym(var_geo)) %>%
    summarise(
      taux_clim = sum(POIDS * (CLIM %in% c(1, 2)), na.rm = TRUE) /
        sum(POIDS * (CLIM %in% c(1, 2, 3)), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(geo = !!sym(var_geo)) %>%
    mutate(type = var_geo)
}

clim_all <- bind_rows(
  calc_taux(df, "RG"),
  calc_taux(df, "DEP")
)

cat("Taux observés (avant interpolation) :\n")
print(clim_all %>% count(type, ANNEE))

fwrite(as.data.table(clim_all),
       file.path(BASE_LOGEMENT, "clim_all_brut.csv"))

# ===========================================================================
# INTERPOLATION LINÉAIRE 2001–2020
# ===========================================================================
annees_cibles <- 2001:2020

interpoler_geo <- function(data) {
  data %>%
    group_by(geo, type) %>%
    complete(ANNEE = annees_cibles) %>%
    arrange(ANNEE) %>%
    mutate(
      taux_clim = {
        idx_obs <- !is.na(taux_clim)
        if (sum(idx_obs) >= 2) {
          approx(x = ANNEE[idx_obs], y = taux_clim[idx_obs],
                 xout = ANNEE, rule = 2)$y
        } else if (sum(idx_obs) == 1) {
          rep(taux_clim[idx_obs][1], n())
        } else {
          taux_clim
        }
      }
    ) %>%
    ungroup()
}

clim_interpolated         <- interpoler_geo(clim_all)
clim_regional_interp      <- clim_interpolated %>% filter(type == "RG")
clim_departemental_interp <- clim_interpolated %>% filter(type == "DEP")

cat("\nContrôle interpolation régionale :\n")
print(clim_regional_interp %>% count(ANNEE))

fwrite(as.data.table(clim_interpolated),
       file.path(BASE_LOGEMENT, "clim_interpolated_2001_2020.csv"))
fwrite(as.data.table(clim_regional_interp),
       file.path(BASE_LOGEMENT, "clim_regional_post2016_2001_2020.csv"))

# ===========================================================================
# CHARGEMENT BASE PRINCIPALE
# ===========================================================================
cat("\n=== CHARGEMENT BASE BINS + MORTALITÉ ===\n")
base <- fread(file.path(BASE_FINAL, "df_bins_mortality_1980_2019.csv"))

base[, COM := as.character(COM)]
base[, COM := trimws(COM)]
base[!grepl("^2[AB]", COM), COM := formatC(as.integer(COM), width = 5, flag = "0")]

# Exclusion Corse
base <- base[!grepl("^2[AB]", COM)]

base[, year := as.integer(year)]
base[, DEP  := substr(COM, 1, 2)]

cat(sprintf("Lignes    : %d\n", nrow(base)))
cat(sprintf("Communes  : %d\n", length(unique(base$COM))))
cat(sprintf("Années    : %d-%d\n", min(base$year), max(base$year)))

# ===========================================================================
# CRÉATION DE RG_final (format post-2016)
# ===========================================================================
cat("\n=== CRÉATION RG_final ===\n")
base[, RG_final := fcase(
  DEP %in% c("75","77","78","91","92","93","94","95"), "11",
  DEP %in% c("18","28","36","37","41","45"),           "24",
  DEP %in% c("21","25","39","58","70","71","89","90"), "27",
  DEP %in% c("14","27","50","61","76"),                "28",
  DEP %in% c("02","59","60","62","80"),                "32",
  DEP %in% c("08","10","51","52","54","55","57","67","68","88"), "44",
  DEP %in% c("44","49","53","72","85"),                "52",
  DEP %in% c("22","29","35","56"),                     "53",
  DEP %in% c("16","17","19","23","24","33","40","47","64","79","86","87"), "75",
  DEP %in% c("09","11","12","30","31","32","34","46","48","65","66","81","82"), "76",
  DEP %in% c("01","03","07","15","26","38","42","43","63","69","73","74"), "84",
  DEP %in% c("04","05","06","13","83","84"),           "93",
  default = NA_character_   # Corse supprimée ici
)]
cat(sprintf("Lignes avec RG_final : %d\n", sum(!is.na(base$RG_final))))

# ===========================================================================
# PRÉPARER TABLES CLIM
# ===========================================================================
clim_reg_prep <- as.data.table(clim_regional_interp)[
  , .(year = as.integer(ANNEE), RG = as.character(geo), taux_clim_RG = taux_clim)
]
clim_dep_prep <- as.data.table(clim_departemental_interp)[
  , .(year = as.integer(ANNEE), DEP = as.character(geo), taux_clim_DEP = taux_clim)
]

cat(sprintf("Lignes clim régionales     : %d\n", nrow(clim_reg_prep)))
cat(sprintf("Lignes clim départementales: %d\n", nrow(clim_dep_prep)))

# ===========================================================================
# FUSION 1 : clim régionale
# ===========================================================================
base[, RG_final := as.character(RG_final)]
clim_reg_prep[, RG := as.character(RG)]

final_etape1 <- merge(
  base,
  clim_reg_prep,
  by.x  = c("year", "RG_final"),
  by.y  = c("year", "RG"),
  all.x = TRUE
)

# ===========================================================================
# FUSION 2 : clim départementale
# ===========================================================================
final_etape1[,  DEP := as.character(DEP)]
clim_dep_prep[, DEP := as.character(DEP)]

final_avec_clim <- merge(
  final_etape1,
  clim_dep_prep,
  by    = c("year", "DEP"),
  all.x = TRUE
)

# ===========================================================================
# VÉRIFICATION ABSENCE CORSE
# ===========================================================================
n_corse <- length(unique(final_avec_clim[grepl("^2[AB]", COM), COM]))
if (n_corse == 0) {
  cat("\n✅ Corse correctement exclue du merge clim.\n")
} else {
  cat(sprintf("\n❌ ATTENTION : %d communes Corse encore présentes !\n", n_corse))
}

# ===========================================================================
# QUALITÉ
# ===========================================================================
cat("\n=== RÉSULTATS FINAUX ===\n")
cat(sprintf("Lignes totales                   : %d\n",   nrow(final_avec_clim)))
cat(sprintf("Communes                         : %d\n",   length(unique(final_avec_clim$COM))))
cat(sprintf("Lignes avec taux_clim_RG         : %d\n",   sum(!is.na(final_avec_clim$taux_clim_RG))))
cat(sprintf("Lignes sans taux_clim_RG         : %d\n",   sum( is.na(final_avec_clim$taux_clim_RG))))
cat(sprintf("Taux de couverture clim régional : %.2f%%\n",
            sum(!is.na(final_avec_clim$taux_clim_RG)) / nrow(final_avec_clim) * 100))
cat("Attendu couverture : ~47.5%% (années 2001-2019 sur 1980-2019)\n")

cat("\n--- Couverture taux_clim_RG par année ---\n")
print(
  final_avec_clim[, .(
    total     = .N,
    avec_clim = sum(!is.na(taux_clim_RG)),
    taux      = round(sum(!is.na(taux_clim_RG)) / .N * 100, 2)
  ), by = "year"][order(year)]
)

# ===========================================================================
# EXPORT
# ===========================================================================
OUT <- file.path(BASE_FINAL, "df_bins_mortality_clim_1980_2019.csv")
fwrite(final_avec_clim, OUT)
cat(sprintf("\n>>> Sauvegardé : %s\n", OUT))
cat(sprintf("    %d lignes | %d communes | %d colonnes\n",
            nrow(final_avec_clim),
            length(unique(final_avec_clim$COM)),
            ncol(final_avec_clim)))

# ===========================================================================
# PLOT : taux de climatisation par région 2001-2020
# ===========================================================================
ANNEES_OBS <- c(2001, 2006, 2013, 2020)

labels_regions <- c(
  "11" = "11 – Île-de-France",       "24" = "24 – Centre-Val de Loire",
  "27" = "27 – Bourgogne-FC",        "28" = "28 – Normandie",
  "32" = "32 – Hauts-de-France",     "44" = "44 – Grand Est",
  "52" = "52 – Pays de la Loire",    "53" = "53 – Bretagne",
  "75" = "75 – Nouvelle-Aquitaine",  "76" = "76 – Occitanie",
  "84" = "84 – Auvergne-RA",         "93" = "93 – PACA"
  # "94" supprimé : Corse exclue
)

plot_data  <- clim_regional_interp %>%
  mutate(geo = as.character(geo)) %>%
  filter(geo %in% names(labels_regions))

points_obs <- plot_data %>% filter(ANNEE %in% ANNEES_OBS)
labels_fin <- plot_data %>% filter(ANNEE == 2020) %>% mutate(label = geo)

ggplot(plot_data, aes(x = ANNEE, y = taux_clim, color = geo, group = geo)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = points_obs, size = 2.5, shape = 21,
             fill = "white", stroke = 1.2) +
  geom_text(data = labels_fin, aes(label = label),
            hjust = -0.2, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_x_continuous(
    breaks = c(ANNEES_OBS, seq(2002, 2019, by = 2)),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA)
  ) +
  scale_color_manual(
    values = scales::hue_pal()(length(labels_regions)),
    labels = labels_regions,
    name   = "Région"
  ) +
  labs(
    title    = "Taux de climatisation par région (2001–2020)",
    subtitle = "Interpolation linéaire entre années d'enquête · Points = années observées",
    x        = "Année",
    y        = "Taux de climatisation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    legend.key.width = unit(1.5, "cm"),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

ggsave(
  file.path(BASE_LOGEMENT, "taux_clim_regions_2001_2020.png"),
  width = 12, height = 7, dpi = 300
)
cat(">>> Plot sauvegardé.\n")
