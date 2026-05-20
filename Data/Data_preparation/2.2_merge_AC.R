# ===========================================================================
# MERGE_CLIM.R
# Merge: df_bins_mortality_1980_2019 x air conditioning rates (regional + departmental)
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
# PATHS
# ===========================================================================
BASE_THESIS   <- "C:/Users/simon/Desktop/master_thesis"
BASE_LOGEMENT <- file.path(BASE_THESIS, "logement")
BASE_FINAL    <- file.path(BASE_THESIS, "final_data")

dir.create(BASE_LOGEMENT, recursive = TRUE, showWarnings = FALSE)

# ===========================================================================
# OLD REGIONS → NEW REGIONS MAPPING (post-2016 format)
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
# LOAD HOUSING SURVEY DATA
# ===========================================================================
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
# REMAP REGIONS
# ===========================================================================
logement2001 <- remap_rg(logement2001)
logement2006 <- remap_rg(logement2006)
logement2013 <- remap_rg(logement2013)

df <- bind_rows(logement2001, logement2006, logement2013, logement2020) %>%
  mutate(CLIM = as.numeric(CLIM), POIDS = as.numeric(POIDS))

# ===========================================================================
# COMPUTE AC RATES
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

fwrite(as.data.table(clim_all),
       file.path(BASE_LOGEMENT, "clim_all_brut.csv"))

# ===========================================================================
# LINEAR INTERPOLATION 2001-2020
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

fwrite(as.data.table(clim_interpolated),
       file.path(BASE_LOGEMENT, "clim_interpolated_2001_2020.csv"))
fwrite(as.data.table(clim_regional_interp),
       file.path(BASE_LOGEMENT, "clim_regional_post2016_2001_2020.csv"))

# ===========================================================================
# LOAD MAIN BASE
# ===========================================================================
base <- fread(file.path(BASE_FINAL, "df_bins_mortality_1980_2019.csv"))

base[, COM := as.character(COM)]
base[, COM := trimws(COM)]
base[!grepl("^2[AB]", COM), COM := formatC(as.integer(COM), width = 5, flag = "0")]

# Exclude Corsica
base <- base[!grepl("^2[AB]", COM)]

base[, year := as.integer(year)]
base[, DEP  := substr(COM, 1, 2)]

# ===========================================================================
# BUILD RG_final (post-2016 format)
# ===========================================================================
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
  default = NA_character_
)]

# ===========================================================================
# PREPARE CLIM TABLES
# ===========================================================================
clim_reg_prep <- as.data.table(clim_regional_interp)[
  , .(year = as.integer(ANNEE), RG = as.character(geo), taux_clim_RG = taux_clim)
]
clim_dep_prep <- as.data.table(clim_departemental_interp)[
  , .(year = as.integer(ANNEE), DEP = as.character(geo), taux_clim_DEP = taux_clim)
]

# ===========================================================================
# MERGE 1: regional AC rate
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
# MERGE 2: departmental AC rate
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
# CORSICA CHECK
# ===========================================================================
n_corse <- length(unique(final_avec_clim[grepl("^2[AB]", COM), COM]))
if (n_corse > 0) {
  warning(sprintf("%d Corsican communes still present after merge.", n_corse))
}

# ===========================================================================
# EXPORT
# ===========================================================================
OUT <- file.path(BASE_FINAL, "df_bins_mortality_clim_1980_2019.csv")
fwrite(final_avec_clim, OUT)

# ===========================================================================
# PLOT: AC rate by region 2001-2020
# ===========================================================================
ANNEES_OBS <- c(2001, 2006, 2013, 2020)

labels_regions <- c(
  "11" = "11 – Île-de-France",       "24" = "24 – Centre-Val de Loire",
  "27" = "27 – Bourgogne-FC",        "28" = "28 – Normandie",
  "32" = "32 – Hauts-de-France",     "44" = "44 – Grand Est",
  "52" = "52 – Pays de la Loire",    "53" = "53 – Bretagne",
  "75" = "75 – Nouvelle-Aquitaine",  "76" = "76 – Occitanie",
  "84" = "84 – Auvergne-RA",         "93" = "93 – PACA"
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
    name   = "Region"
  ) +
  labs(
    title    = "Air conditioning rate by region (2001–2020)",
    subtitle = "Linear interpolation between survey years · Points = observed years",
    x        = "Year",
    y        = "AC rate"
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
