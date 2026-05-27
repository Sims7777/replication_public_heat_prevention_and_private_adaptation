library(readxl)
library(dplyr)
library(data.table)
library(skimr)
library(fixest)
library(ggplot2)
library(arrow)
library(tidyverse)
library(car)
library(sf)

# ===========================================================================
# BUILDING STOCK VINTAGE
# ===========================================================================

df_log <- read_excel(
  "C:/Users/simon/Desktop/master_thesis/logement/TD_LOG1_2022.xlsx",
  sheet = "COM",
  skip  = 10
)

cols_total  <- grep("ACHL24",              names(df_log), value = TRUE)
cols_av1945 <- grep("ACHL24A11|ACHL24A12", names(df_log), value = TRUE)
cols_av1970 <- grep("ACHL24A11|ACHL24A12|ACHL24B11", names(df_log), value = TRUE)

parts_log <- df_log %>%
  select(CODGEO, all_of(cols_total)) %>%
  rowwise() %>%
  mutate(
    total_log   = sum(c_across(all_of(cols_total)),   na.rm = TRUE),
    part_av1945 = sum(c_across(all_of(cols_av1945)), na.rm = TRUE) / total_log,
    part_av1970 = sum(c_across(all_of(cols_av1970)), na.rm = TRUE) / total_log
  ) %>%
  ungroup() %>%
  select(CODGEO, part_av1945, part_av1970) %>%
  mutate(CODGEO = as.integer(CODGEO))

df <- fread("C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet6.csv")

df <- df %>%
  left_join(parts_log, by = c("COM" = "CODGEO"))

# Coverage check
df %>%
  group_by(year) %>%
  summarise(
    n_communes      = n_distinct(COM),
    n_matched       = sum(!is.na(part_av1945)),
    coverage_rate   = round(n_matched / n() * 100, 1)
  )

# ===========================================================================
# HISTORICAL HEAT EXPOSURE
# ===========================================================================

meteo_annuel <- df %>%
  group_by(COM, year) %>%
  summarise(
    jours_gt28 = sum(tg_new_tbin_gt_28, na.rm = TRUE),
    jours_gt30 = sum(tg_tbin_gt_30,     na.rm = TRUE),
    .groups    = "drop"
  )

exposition_hist <- meteo_annuel %>%
  filter(year <= 2000) %>%
  group_by(COM) %>%
  summarise(
    exposition_hist_28 = mean(jours_gt28, na.rm = TRUE),
    exposition_hist_30 = mean(jours_gt30, na.rm = TRUE),
    .groups = "drop"
  )

df <- df %>%
  left_join(meteo_annuel,    by = c("COM", "year")) %>%
  left_join(exposition_hist, by = "COM") %>%
  mutate(
    ecart_norm_28 = jours_gt28 - exposition_hist_28,
    ecart_norm_30 = jours_gt30 - exposition_hist_30
  )

df %>%
  select(COM, year, jours_gt28, jours_gt30,
         exposition_hist_28, exposition_hist_30,
         ecart_norm_28, ecart_norm_30) %>%
  skimr::skim()

# ===========================================================================
# EXPORT
# ===========================================================================

fwrite(df, "C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet7.csv")

# ===========================================================================
# LOAD FOR ROBUSTNESS REGRESSIONS
# ===========================================================================

cols_needed <- c(
  "COM", "DEP", "RG", "year", "month",
  "taux_mortalite_75_plus",
  "dose_pnc",
  "taux_clim_RG",
  "value_estimated_population",
  "tg_new_tbin_lt_0", "tg_new_tbin_0_15", "tg_new_tbin_20_28", "tg_new_tbin_gt_28",
  "hubin_0_20", "hubin_40_60", "hubin_60_80", "hubin_80_100",
  "rrbin_0_3", "rrbin_10_100", "rrbin_gt_100",
  "fgbin_0_3", "fgbin_10_20", "fgbin_gt_20",
  "exposition_hist_28", "ecart_norm_28",
  "part_av1945", "part_av1970"
)

df <- fread(
  "C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet7.csv",
  select = cols_needed
)

# ===========================================================================
# AC SUBSAMPLES — MEDIAN SPLIT
# ===========================================================================

med_clim <- df %>%
  filter(year == 2001) %>%
  summarise(q = median(taux_clim_RG, na.rm = TRUE)) %>%
  pull(q)

reg_clim_groupe <- df %>%
  filter(year == 2001) %>%
  select(RG, taux_clim_RG) %>%
  distinct() %>%
  mutate(clim_groupe = ifelse(taux_clim_RG >= med_clim, "high", "low"))

df_clim_high <- df %>%
  filter(year >= 2001) %>%
  inner_join(reg_clim_groupe %>% filter(clim_groupe == "high") %>% select(RG), by = "RG")

df_clim_low <- df %>%
  filter(year >= 2001) %>%
  inner_join(reg_clim_groupe %>% filter(clim_groupe == "low") %>% select(RG), by = "RG")

# ===========================================================================
# AC SUBSAMPLES — QUARTILES
# ===========================================================================

quartiles <- df %>%
  filter(year == 2001) %>%
  summarise(
    q1 = quantile(taux_clim_RG, 0.25, na.rm = TRUE),
    q2 = quantile(taux_clim_RG, 0.50, na.rm = TRUE),
    q3 = quantile(taux_clim_RG, 0.75, na.rm = TRUE)
  )

dep_clim_2001 <- df %>%
  filter(year == 2001) %>%
  mutate(clim_quartile = case_when(
    taux_clim_RG <  quartiles$q1 ~ 1L,
    taux_clim_RG <  quartiles$q2 ~ 2L,
    taux_clim_RG <  quartiles$q3 ~ 3L,
    TRUE                         ~ 4L
  )) %>%
  select(DEP, clim_quartile) %>%
  distinct()

df_clim <- df %>%
  filter(year >= 2001) %>%
  left_join(dep_clim_2001, by = "DEP")

df_clim_q1_2 <- df_clim %>% filter(clim_quartile %in% c(1, 2))
df_clim_q1   <- df_clim %>% filter(clim_quartile == 1)
df_clim_q2   <- df_clim %>% filter(clim_quartile == 2)
df_clim_q3   <- df_clim %>% filter(clim_quartile == 3)
df_clim_q4   <- df_clim %>% filter(clim_quartile == 4)

# ===========================================================================
# ROBUSTNESS — HISTORICAL HEAT EXPOSURE
# ===========================================================================

feols_low_hist <- feols(
  taux_mortalite_75_plus ~
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15 +
       tg_new_tbin_20_28 + tg_new_tbin_gt_28) * dose_pnc * exposition_hist_28 +
    dose_pnc + exposition_hist_28 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = df_clim_low,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_low_hist)

feols_high_hist <- feols(
  taux_mortalite_75_plus ~
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15 +
       tg_new_tbin_20_28 + tg_new_tbin_gt_28) * dose_pnc * exposition_hist_28 +
    dose_pnc + exposition_hist_28 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = df_clim_high,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_high_hist)

# Horse race: AC vs. historical exposure in a single specification
feols_horserace <- feols(
  taux_mortalite_75_plus ~
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15 +
       tg_new_tbin_20_28 + tg_new_tbin_gt_28) * dose_pnc +
    dose_pnc * taux_clim_RG +
    dose_pnc * exposition_hist_28 +
    taux_clim_RG + exposition_hist_28 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = df,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_horserace)

# ===========================================================================
# ROBUSTNESS — DEVIATION FROM CLIMATIC NORM
# ===========================================================================

feols_low_ecart <- feols(
  taux_mortalite_75_plus ~
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15 +
       tg_new_tbin_20_28 + tg_new_tbin_gt_28) * dose_pnc * ecart_norm_28 +
    dose_pnc + ecart_norm_28 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = df_clim_low,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_low_ecart)

feols_high_ecart <- feols(
  taux_mortalite_75_plus ~
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15 +
       tg_new_tbin_20_28 + tg_new_tbin_gt_28) * dose_pnc * ecart_norm_28 +
    dose_pnc + ecart_norm_28 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = df_clim_high,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_high_ecart)

# ===========================================================================
# ROBUSTNESS — BUILDING STOCK VINTAGE (pre-1945)
# ===========================================================================

feols_low_av1945 <- feols(
  taux_mortalite_75_plus ~
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15 +
       tg_new_tbin_20_28 + tg_new_tbin_gt_28) * dose_pnc * part_av1945 +
    dose_pnc + part_av1945 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = df_clim_low,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_low_av1945)

feols_high_av1945 <- feols(
  taux_mortalite_75_plus ~
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15 +
       tg_new_tbin_20_28 + tg_new_tbin_gt_28) * dose_pnc * part_av1945 +
    dose_pnc + part_av1945 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = df_clim_high,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_high_av1945)

# ===========================================================================
# ROBUSTNESS — BUILDING STOCK VINTAGE (pre-1970)
# ===========================================================================

feols_low_av1970 <- feols(
  taux_mortalite_75_plus ~
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15 +
       tg_new_tbin_20_28 + tg_new_tbin_gt_28) * dose_pnc * part_av1970 +
    dose_pnc + part_av1970 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = df_clim_low,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_low_av1970)

feols_high_av1970 <- feols(
  taux_mortalite_75_plus ~
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15 +
       tg_new_tbin_20_28 + tg_new_tbin_gt_28) * dose_pnc * part_av1970 +
    dose_pnc + part_av1970 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = df_clim_high,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_high_av1970)

# ===========================================================================
# FIGURE — AC PREVALENCE MAP (2001)
# ===========================================================================

ac_data <- data.frame(
  code_region = c("11", "24", "27", "28", "32", "44", "52", "53", "75", "76", "84", "93"),
  ac_2001     = c(0.8, 1.0, 0.6, 0.5, 1.0, 0.6, 0.8, 0.3, 1.5, 3.8, 0.8, 4.6)
)

regions_sf <- st_read(
  "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions-version-simplifiee.geojson",
  quiet = TRUE
) %>%
  left_join(ac_data, by = c("code" = "code_region"))

ggplot(regions_sf) +
  geom_sf(aes(fill = ac_2001), color = "white", linewidth = 0.4) +
  scale_fill_gradientn(
    colours = c("#EFF3FF", "#BDD7E7", "#6BAED6", "#2171B5", "#08306B"),
    name    = "AC prevalence\n(% households, 2001)",
    limits  = c(0, 5),
    breaks  = c(0.3, 1.5, 3.8, 4.6),
    labels  = c("0.3% (Brittany)", "1.5% (Nouv.-Aquitaine)",
                "3.8% (Occitanie)", "4.6% (PACA)")
  ) +
  theme_void(base_size = 11) +
  theme(
    legend.position   = c(0.14, 0.48),
    legend.title      = element_text(size = 7.5),
    legend.text       = element_text(size = 7),
    legend.key.height = unit(0.55, "cm"),
    legend.key.width  = unit(0.35, "cm"),
    plot.margin       = margin(4, 4, 4, 4)
  )

ggsave("C:/Users/simon/Desktop/master_thesis/results/figure_ac_map_2001.pdf",
       width = 5, height = 5.5, device = "pdf")
ggsave("C:/Users/simon/Desktop/master_thesis/results/figure_ac_map_2001.png",
       width = 5, height = 5.5, dpi = 300)

# ===========================================================================
# FIGURE — HISTORICAL HEAT EXPOSURE MAP (1980–2000)
# ===========================================================================

exposition_regionale <- df %>%
  filter(year <= 2000) %>%
  group_by(RG, year) %>%
  summarise(
    jours_gt28_region = weighted.mean(jours_gt28, w = value_estimated_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(RG) %>%
  summarise(exposition_hist = mean(jours_gt28_region, na.rm = TRUE))

print(exposition_regionale)

exposition_data <- data.frame(
  RG              = c(11, 24, 27, 28, 32, 44, 52, 53, 75, 76, 84, 93),
  exposition_hist = c(0.166, 0.0747, 0.0996, 0.00667, 0.0194, 0.0485,
                      0.109, 0.0247, 0.256, 0.800, 0.236, 0.493)
)

regions_sf <- st_read(
  "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions-version-simplifiee.geojson",
  quiet = TRUE
) %>%
  mutate(RG = as.integer(code)) %>%
  left_join(exposition_data, by = "RG")

ggplot(regions_sf) +
  geom_sf(aes(fill = exposition_hist), color = "white", linewidth = 0.4) +
  scale_fill_gradientn(
    colours = c("#FFF5EB", "#FDD0A2", "#FDAE6B", "#E6550D", "#8C2D04"),
    name    = "Historical heat exposure\n(avg. days > 28°C per year,\n1980–2000, pop.-weighted)",
    breaks  = c(0.007, 0.10, 0.25, 0.49, 0.80),
    labels  = c("0.01", "0.10", "0.25", "0.49", "0.80")
  ) +
  theme_void(base_size = 11) +
  theme(
    legend.position   = c(0.14, 0.48),
    legend.title      = element_text(size = 7.5),
    legend.text       = element_text(size = 7),
    legend.key.height = unit(0.55, "cm"),
    legend.key.width  = unit(0.35, "cm"),
    plot.margin       = margin(4, 4, 4, 4)
  )

ggsave("C:/Users/simon/Desktop/master_thesis/results/figure_exposition_hist_map.pdf",
       width = 5, height = 5.5, device = "pdf")
ggsave("C:/Users/simon/Desktop/master_thesis/results/figure_exposition_hist_map.png",
       width = 5, height = 5.5, dpi = 300)
