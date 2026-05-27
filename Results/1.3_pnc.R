library(data.table)
library(dplyr)
library(fixest)
library(ggplot2)
library(car)
library(sf)

# ===========================================================================
# LOAD
# ===========================================================================
df <- fread("C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet6.csv")

# ===========================================================================
# AC MEDIAN SPLIT (2001)
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
# AC QUARTILE SPLIT (2001)
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

df_clim   <- df %>% filter(year >= 2001) %>% left_join(dep_clim_2001, by = "DEP")
df_clim_q1    <- df_clim %>% filter(clim_quartile == 1)
df_clim_q2    <- df_clim %>% filter(clim_quartile == 2)
df_clim_q3    <- df_clim %>% filter(clim_quartile == 3)
df_clim_q4    <- df_clim %>% filter(clim_quartile == 4)
df_clim_q1_2  <- df_clim %>% filter(clim_quartile %in% c(1, 2))

# ===========================================================================
# SHARED FORMULA COMPONENTS
# ===========================================================================
weather_controls <- c(
  "hubin_0_20", "hubin_40_60", "hubin_60_80", "hubin_80_100",
  "rrbin_0_3",  "rrbin_10_100", "rrbin_gt_100",
  "fgbin_0_3",  "fgbin_10_20", "fgbin_gt_20"
)

# ===========================================================================
# TABLE 1: BASELINE PNC — total and 75+ (dose variable)
# ===========================================================================
feols_dose <- feols(
  taux_mortalite_total ~
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 + tg_tbin_5_10 + tg_tbin_10_15 +
       tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30) * dose_pnc + dose_pnc +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = df[year >= 2001],
  cluster = ~COM,
  weights = ~value_estimated_population
)

feols_dose_75 <- feols(
  taux_mortalite_75_plus ~
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 + tg_tbin_5_10 + tg_tbin_10_15 +
       tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30) * dose_pnc + dose_pnc +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = df[year >= 2001],
  cluster = ~COM,
  weights = ~value_estimated_population
)

etable(feols_dose, feols_dose_75, tex = TRUE)

# ===========================================================================
# TABLE 2: DOSE × AC GROUP — total mortality
# ===========================================================================
run_dose_total <- function(data) {
  feols(
    taux_mortalite_total ~
      (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 + tg_tbin_5_10 + tg_tbin_10_15 +
         tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30) * dose_pnc + dose_pnc +
      hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
      rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
      fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
      COM^month + month^year + DEP^year,
    data    = data,
    cluster = ~COM,
    weights = ~value_estimated_population
  )
}

feols_low_dose  <- run_dose_total(df_clim_low)
feols_high_dose <- run_dose_total(df_clim_high)

etable(feols_low_dose, feols_high_dose, tex = TRUE)

# ===========================================================================
# TABLE 3: DOSE × AC QUARTILE — 75+ mortality (broader bins)
# ===========================================================================
run_dose_75_quartile <- function(data) {
  feols(
    taux_mortalite_75_plus ~
      (tg_new_tbin_lt_0 + tg_new_tbin_0_15 +
         tg_new_tbin_20_28 + tg_new_tbin_gt_28) * dose_pnc + dose_pnc +
      hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
      rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
      fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
      COM^month + month^year + DEP^year,
    data    = data,
    cluster = ~COM,
    weights = ~value_estimated_population
  )
}

feols_q1   <- run_dose_75_quartile(df_clim_q1)
feols_q2   <- run_dose_75_quartile(df_clim_q2)
feols_q3   <- run_dose_75_quartile(df_clim_q3)
feols_q4   <- run_dose_75_quartile(df_clim_q4)
feols_q1_2 <- run_dose_75_quartile(df_clim_q1_2)

etable(feols_q1, feols_q2, feols_q3, feols_q4, tex = TRUE)
etable(feols_q1_2, feols_q3, feols_q4, tex = TRUE)

# ===========================================================================
# TABLE G: DOSE × AC GROUP × INCOME — 75+
# ===========================================================================
run_dose_income_75 <- function(data) {
  feols(
    taux_mortalite_75_plus ~
      (tg_new_tbin_lt_0 + tg_new_tbin_0_15 +
         tg_new_tbin_20_28 + tg_new_tbin_gt_28) * dose_pnc * rich +
      dose_pnc + rich +
      hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
      rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
      fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
      COM^month + month^year + DEP^year,
    data    = data,
    cluster = ~COM,
    weights = ~value_estimated_population
  )
}

feols_low_income  <- run_dose_income_75(df_clim_low)
feols_high_income <- run_dose_income_75(df_clim_high)

etable(feols_low_income, feols_high_income, tex = TRUE)

# Joint tests for Table G
linearHypothesis(feols_low_income,
                 "tg_new_tbin_gt_28:dose_pnc + tg_new_tbin_gt_28:dose_pnc:rich = 0")
linearHypothesis(feols_high_income,
                 "tg_new_tbin_gt_28:dose_pnc = 0")
linearHypothesis(feols_high_income,
                 "tg_new_tbin_gt_28:dose_pnc + tg_new_tbin_gt_28:dose_pnc:rich = 0")

# ===========================================================================
# TABLE H: RESTRICTED TO 2001-2010
# ===========================================================================
feols_low_2010  <- run_dose_income_75(df_clim_low[year  <= 2010])
feols_high_2010 <- run_dose_income_75(df_clim_high[year <= 2010])

etable(feols_low_2010, feols_high_2010, tex = TRUE)
# ==============================================================================================
# FIGURE 2: Effect of PNC activation on over-75 mortality by air conditioning group
# ==============================================================================================
df <- data.frame(
  quartile = factor(
    c("Q1–Q2\n(lowest AC)", "Q3", "Q4\n(highest AC)"),
    levels = c("Q1–Q2\n(lowest AC)", "Q3", "Q4\n(highest AC)")
  ),
  coef = c(-0.4566, -0.1360,  0.0037),
  se   = c( 0.0524,  0.0298,  0.0122),
  sig  = c(TRUE, TRUE, FALSE)
)

df <- df %>%
  mutate(
    ci_lo = coef - 1.96 * se,
    ci_hi = coef + 1.96 * se
  )

ggplot(df, aes(x = quartile, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_errorbar(
    aes(ymin = ci_lo, ymax = ci_hi),
    width     = 0.10,
    linewidth = 0.7,
    color     = "grey40"
  ) +
  geom_point(
    aes(fill = sig),
    shape     = 21,
    size      = 3.5,
    color     = "grey30",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#1D6FA4", "FALSE" = "white"),
    guide  = "none"
  ) +
  scale_y_continuous(
    limits = c(-0.65, 0.15),
    breaks = seq(-0.6, 0.1, by = 0.1),
    labels = function(x) sprintf("%.1f", x)
  ) +
  labs(
    x       = "Air conditioning penetration group (2001)",
    y       = "Effect on mortality rate (75+)\ndeaths per 10,000 inhabitants"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.y       = element_text(size = 10, margin = margin(r = 10)),
    axis.title.x       = element_text(size = 10, margin = margin(t = 10)),
    axis.text          = element_text(size = 10, color = "grey20"),
    plot.caption       = element_text(size = 8, color = "grey40",
                                      hjust = 0, margin = margin(t = 12)),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.4),
    plot.margin        = margin(12, 16, 8, 8)
  )

ggsave("C:/Users/simon/Desktop/master_thesis/results/figure_pnc_ac_pooled.pdf", width = 6, height = 4.5, device = "pdf")
ggsave("C:/Users/simon/Desktop/master_thesis/results/figure_pnc_ac_pooled.png", width = 6, height = 4.5, dpi = 300)


# ===========================================================================
# FIGURE 3: TEMPERATURE × 75+ MORTALITY BY AC GROUP AND INCOME
# ===========================================================================
order_levels <- c("< 0", "0 to 15", "15 to 20 (REF)", "20 to 28", "> 28")

palette_4g <- c(
  "Low AC \u2014 Poor"  = "#104E8B",
  "Low AC \u2014 Rich"  = "#A0CBE8",
  "High AC \u2014 Poor" = "#8B1A1A",
  "High AC \u2014 Rich" = "#FF6A6A"
)

fml_4g <- taux_mortalite_75_plus ~
  (tg_new_tbin_lt_0 + tg_new_tbin_0_15 +
     tg_new_tbin_20_28 + tg_new_tbin_gt_28) * dose_pnc + dose_pnc +
  hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
  rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
  fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
  COM^month + month^year + DEP^year

run_4g <- function(data) {
  feols(fml_4g, data = data, cluster = ~COM, weights = ~value_estimated_population)
}

feols_highAC_rich <- run_4g(df_clim_high[rich == 1])
feols_highAC_poor <- run_4g(df_clim_high[rich == 0])
feols_lowAC_rich  <- run_4g(df_clim_low[rich  == 1])
feols_lowAC_poor  <- run_4g(df_clim_low[rich  == 0])

make_coef_df <- function(model, groupe) {
  cf <- coef(model)[1:4]
  ci <- confint(model)[1:4, ]
  data.frame(
    Variable    = names(cf),
    Coefficient = as.numeric(cf),
    Lower_CI    = ci[, 1],
    Upper_CI    = ci[, 2],
    Groupe      = groupe
  ) %>%
    mutate(Variable = case_match(
      Variable,
      "tg_new_tbin_lt_0"  ~ "< 0",
      "tg_new_tbin_0_15"  ~ "0 to 15",
      "tg_new_tbin_20_28" ~ "20 to 28",
      "tg_new_tbin_gt_28" ~ "> 28",
      .default = Variable
    ))
}

ref_rows <- function(groupe) {
  data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,
             Lower_CI = 0, Upper_CI = 0, Groupe = groupe)
}

df_plot4g <- bind_rows(
  make_coef_df(feols_highAC_rich, "High AC \u2014 Rich"),
  make_coef_df(feols_highAC_poor, "High AC \u2014 Poor"),
  make_coef_df(feols_lowAC_rich,  "Low AC \u2014 Rich"),
  make_coef_df(feols_lowAC_poor,  "Low AC \u2014 Poor"),
  ref_rows("High AC \u2014 Rich"), ref_rows("High AC \u2014 Poor"),
  ref_rows("Low AC \u2014 Rich"),  ref_rows("Low AC \u2014 Poor")
) %>%
  mutate(
    Variable = factor(Variable, levels = order_levels),
    Groupe   = factor(Groupe,   levels = names(palette_4g))
  )

fig_4g <- ggplot(df_plot4g,
                 aes(x = Variable, y = Coefficient,
                     color = Groupe, fill = Groupe, group = Groupe)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              alpha = 0.15, color = NA,
              position = position_dodge(width = 0.3)) +
  geom_line(linetype = "dashed", linewidth = 0.9,
            position = position_dodge(width = 0.3)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  scale_color_manual(values = palette_4g) +
  scale_fill_manual(values  = palette_4g) +
  labs(x = "Temperature bins (\u00b0C)", y = "Mortality rate per 10,000 (75+)",
       color = NULL, fill = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text     = element_text(size = 11))

ggsave("C:/Users/simon/Desktop/master_thesis/results/fig_pnc_4group.png",
       plot = fig_4g, width = 8, height = 7, dpi = 300)


# ===========================================================================
# DESCRIPTIVE STATS
# ===========================================================================

# Table B.1 — AC rates at survey waves by region
vagues <- c(2001, 2006, 2013, 2020)
table_vagues <- df %>%
  filter(year %in% vagues) %>%
  distinct(RG, year, taux_clim_RG) %>%
  tidyr::pivot_wider(names_from = year, values_from = taux_clim_RG) %>%
  arrange(RG)

# Table B.2 — Mean annual days above 28°C per commune by AC quartile
table_jours <- df_clim %>%
  group_by(clim_quartile, COM, year) %>%
  summarise(jours_28_com = sum(tg_new_tbin_gt_28, na.rm = TRUE), .groups = "drop") %>%
  group_by(clim_quartile, year) %>%
  summarise(jours_28_moy = sum(jours_28_com) / n_distinct(COM), .groups = "drop") %>%
  group_by(clim_quartile) %>%
  summarise(jours_28_moy = mean(jours_28_moy, na.rm = TRUE), .groups = "drop") %>%
  arrange(clim_quartile)

# AC persistence: correlation between 2001 and 2019 regional rates
wide <- df %>%
  filter(year %in% c(2001, 2019)) %>%
  group_by(COM, year) %>%
  summarise(clim = mean(taux_clim_RG, na.rm = TRUE), .groups = "drop") %>%
  as.data.table() %>%
  dcast(COM ~ year, value.var = "clim")

cor(wide$`2001`, wide$`2019`, use = "complete.obs")

# ===========================================================================
# MAPS: PNC ACTIVATION DAYS BY DEPARTMENT
# ===========================================================================
dept_sf <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson")

prep_jours <- function(data, yr) {
  data %>%
    filter(year == yr) %>%
    group_by(DEP) %>%
    summarise(n_jours_pnc = max(n_jours_pnc, na.rm = TRUE), .groups = "drop") %>%
    mutate(DEP = stringr::str_pad(as.character(DEP), width = 2, pad = "0"))
}

breaks <- c(0, 1, 4, 7, 11, Inf)
labels <- c("0", "1-3", "4-6", "7-10", "11+")
colors <- c("0" = "white", "1-3" = "#fcc5c0", "4-6" = "#df65b0",
            "7-10" = "#dd1c77", "11+" = "#67001f")

add_cat <- function(data) {
  data %>% mutate(cat = factor(
    cut(n_jours_pnc, breaks = breaks, labels = labels,
        include.lowest = TRUE, right = FALSE),
    levels = labels
  ))
}

plot_map <- function(data, titre) {
  ggplot(data) +
    geom_sf(aes(fill = cat), color = "grey40", linewidth = 0.2) +
    scale_fill_manual(values = colors, labels = labels, na.value = "grey90",
                      name = "Number of days\nPNC triggered", drop = FALSE) +
    labs(title = titre, caption = "Source: M\u00e9t\u00e9o-France") +
    theme_void() +
    theme(plot.title   = element_text(color = "#1f4e79", face = "bold", size = 13),
          plot.caption = element_text(hjust = 0, color = "#1f4e79", size = 9),
          legend.position = "right")
}

for (yr in c(2006, 2010, 2014, 2019)) {
  fig <- plot_map(
    add_cat(dept_sf %>% left_join(prep_jours(df, yr), by = c("code" = "DEP"))),
    sprintf("Number of PNC days triggered by department \u2014 %d", yr)
  )
  ggsave(
    sprintf("C:/Users/simon/Desktop/master_thesis/results/fig_pnc_%d.png", yr),
    plot = fig, width = 8, height = 7, dpi = 300
  )
}library(data.table)
library(dplyr)
library(fixest)
library(ggplot2)
library(arrow)
library(tidyverse)
library(car)
library(sf)

#gc()
# ===========================================================================
# CHARGEMENT
# ===========================================================================

df <- fread("C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet6.csv")

# ===========================================================================
# RÉGRESSION DiD : Effet PNC sur mortalité liée à la chaleur
# ===========================================================================

#pnc
feols_pnc <- feols(
  taux_mortalite_total ~ 
    #temperatures
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
       tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + 
       tg_tbin_25_30 + tg_tbin_gt_30)*pnc + pnc +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects 
    COM^month + month^year + DEP^year,
  
  data = df[year >= 2001],
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_pnc)



#dose_pnc = compteur d'intensité pnc intra month^year
feols_dose <- feols(
  taux_mortalite_total ~ 
    # Température 
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
       tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + 
       tg_tbin_25_30 + tg_tbin_gt_30)*dose_pnc +
    
    #jours PNC activé
    dose_pnc +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df[year >= 2001],
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_dose)


# ===========================================================================
# Effet PNC sur mortalité des 75+
# ===========================================================================
#pnc 
feols_pnc_75 <- feols(
  taux_mortalite_75_plus ~ 
    # Température baseline
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
       tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + 
       tg_tbin_25_30 + tg_tbin_gt_30)*pnc + pnc +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df[year >= 2001],
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_pnc_75)

#dose_pnc 75+
feols_dose_response_75 <- feols(
  taux_mortalite_75_plus ~ 
    # Température baseline
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
       tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + 
       tg_tbin_25_30 + tg_tbin_gt_30)*dose_pnc +
    
    # Effet dose-réponse : jours PNC activé
    dose_pnc +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df[year >= 2001],
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_dose_response_75)


# ===========================================================================
# CRÉATION SUBSAMPLE PAR NIVEAU DE CLIM
# ===========================================================================

# ===========================================================================
# MEDIANE
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
  # REGRESSION FAIBLE / FORTE CLIM
# ===========================================================================
#groupe faible clim 2001
feols_faible_clim <- feols(
  taux_mortalite_total ~ 
    # Température 
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
       tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + 
       tg_tbin_25_30 + tg_tbin_gt_30)*dose_pnc +
    
    #jours PNC activé
    dose_pnc +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_low,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_faible_clim)

#groupe forte clim 2001
feols_forte_clim <- feols(
  taux_mortalite_total ~ 
    # Température 
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
       tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + 
       tg_tbin_25_30 + tg_tbin_gt_30)*dose_pnc +
    
    #jours PNC activé
    dose_pnc +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_high,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_forte_clim)



#correlation entre les niveaux de clim de 2001 et de 2019 (classement)
wide <- df %>%
  filter(year %in% c(2001, 2019)) %>%
  group_by(COM, year) %>%
  summarise(clim = mean(taux_clim_RG, na.rm = TRUE), .groups = "drop") %>%
  as.data.table() %>%
  dcast(COM ~ year, value.var = "clim")

cor(wide$`2001`, wide$`2019`, use = "complete.obs")



# ===========================================================================
# QUARTILE
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

df_clim_q1_2<- df_clim %>% filter(clim_quartile == 1 | clim_quartile == 2)

df_clim_q1 <- df_clim %>% filter(clim_quartile == 1)
df_clim_q2 <- df_clim %>% filter(clim_quartile == 2)
df_clim_q3 <- df_clim %>% filter(clim_quartile == 3)
df_clim_q4 <- df_clim %>% filter(clim_quartile == 4)

#Q1
feols_dose_Q1 <- feols(
  taux_mortalite_75_plus ~ 
    # Température 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
    tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28)*dose_pnc +
    
    #jours PNC activé
    dose_pnc +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_q1,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_dose_Q1)

#Q2
feols_dose_Q2 <- feols(
  taux_mortalite_75_plus ~ 
    # Température 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
       tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28)*dose_pnc +
    
    #jours PNC activé
    dose_pnc +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_q2,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_dose_Q2)



#Q3
feols_dose_Q3 <- feols(
  taux_mortalite_75_plus ~ 
    # Température 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
    tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28)*dose_pnc +
    
    #jours PNC activé
    dose_pnc +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_q3,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_dose_Q3)


#Q4
feols_dose_Q4 <- feols(
  taux_mortalite_75_plus ~ 
    # Température 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
    tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28)*dose_pnc +
    
    #jours PNC activé
    dose_pnc +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_q4,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_dose_Q4)

#Q1_2 pour gagner en puissance
feols_dose_Q1_2 <- feols(
  taux_mortalite_75_plus ~ 
    # Température 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
       tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28)*dose_pnc +
    
    #jours PNC activé
    dose_pnc +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_q1_2,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_dose_Q1_2)


# ===========================================================================
# PNC * INCOME BY GROUP OF AC
# ===========================================================================

#groupe faible clim 2001
feols_faible_clim_rich <- feols(
  taux_mortalite_total ~ 
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
       tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + 
       tg_tbin_25_30 + tg_tbin_gt_30) * dose_pnc * rich +
    dose_pnc + rich +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  
  data    = df_clim_low,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_faible_clim_rich)


#pour gagner de la puissance
feols_faible_clim_rich_28 <- feols(
  taux_mortalite_75_plus ~ 
    # Température 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
       tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28) * dose_pnc * rich +
    
    #jours PNC activé
    dose_pnc + rich +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_low,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_faible_clim_rich_28)





#groupe bcp clim 2001
feols_forte_clim_rich <- feols(
  taux_mortalite_total ~ 
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
       tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + 
       tg_tbin_25_30 + tg_tbin_gt_30) * dose_pnc * rich +
    dose_pnc + rich +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  
  data    = df_clim_high,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_forte_clim_rich)


#pour gagner de la puissance
feols_forte_clim_rich_28 <- feols(
  taux_mortalite_75_plus ~ 
    # Température 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
       tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28) * dose_pnc * rich +
    
    #jours PNC activé
    dose_pnc + rich +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_high,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_forte_clim_rich_28)

#test de significativité
#
# ── Clim LOW : effet PNC pour rich=1 ──────────────────────────────────────────
# H0 : tg_new_tbin_gt_28:dose_pnc + tg_new_tbin_gt_28:dose_pnc:rich = 0
linearHypothesis(feols_faible_clim_rich_28,
                 "tg_new_tbin_gt_28:dose_pnc + tg_new_tbin_gt_28:dose_pnc:rich = 0")

# ── Clim HIGH : effet PNC pour rich=0 ─────────────────────────────────────────
linearHypothesis(feols_forte_clim_rich_28,
                 "tg_new_tbin_gt_28:dose_pnc = 0")

# ── Clim HIGH : effet PNC pour rich=1 ─────────────────────────────────────────
linearHypothesis(feols_forte_clim_rich_28,
                 "tg_new_tbin_gt_28:dose_pnc + tg_new_tbin_gt_28:dose_pnc:rich = 0")






#income comme variable continue pour low AC
feols_income_continue_lowAC <- feols(
  taux_mortalite_total ~ 
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
       tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + 
       tg_tbin_25_30 + tg_tbin_gt_30) * dose_pnc * median_uc +
    dose_pnc + median_uc +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  
  data    = df_clim_low,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_income_continue_lowAC)

#income comme variable continue pour high AC
feols_income_continue_highAC <- feols(
  taux_mortalite_total ~ 
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
       tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + 
       tg_tbin_25_30 + tg_tbin_gt_30) * dose_pnc * median_uc +
    dose_pnc + median_uc +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  
  data    = df_clim_high,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_income_continue_highAC)


#pour les 75+
#income comme variable continue pour low AC
feols_income_continue_lowAC_75 <- feols(
  taux_mortalite_75_plus ~ 
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
       tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + 
       tg_tbin_25_30 + tg_tbin_gt_30) * dose_pnc * median_uc +
    dose_pnc + median_uc +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  
  data    = df_clim_low,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_income_continue_lowAC_75)

#income comme variable continue pour high AC
feols_income_continue_highAC_75 <- feols(
  taux_mortalite_75_plus ~ 
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
       tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + 
       tg_tbin_25_30 + tg_tbin_gt_30) * dose_pnc * median_uc +
    dose_pnc + median_uc +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  
  data    = df_clim_high,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_income_continue_highAC_75)



#####pour gagner en power

#income comme variable continue pour low AC sup28
feols_income_continue_lowAC28 <- feols(
  taux_mortalite_75_plus ~ 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
       tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28) * dose_pnc * median_uc +
    dose_pnc + median_uc +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  
  data    = df_clim_low,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_income_continue_lowAC28)

#income comme variable continue pour high AC sup 28
feols_income_continue_highAC28 <- feols(
  taux_mortalite_75_plus ~ 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
       tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28) * dose_pnc * median_uc +
    dose_pnc + median_uc +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  
  data    = df_clim_high,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_income_continue_highAC28)
        




# ===========================================================================
# Figure avec les 4 groupes 
# ===========================================================================

# ===========================================================================
# RECODE & ORDER
# ===========================================================================
order_levels <- c("< 0", "0 to 15", "15 to 20 (REF)", "20 to 28", "> 28")

# ===========================================================================
# SOUS-ÉCHANTILLONS
# ===========================================================================
df_highAC_rich <- df_clim_high[rich == 1]
df_highAC_poor <- df_clim_high[rich == 0]
df_lowAC_rich  <- df_clim_low[rich == 1]
df_lowAC_poor  <- df_clim_low[rich == 0]

# ===========================================================================
# FORMULE COMMUNE
# ===========================================================================
fml <- taux_mortalite_75_plus ~
  (tg_new_tbin_lt_0 + tg_new_tbin_0_15 +
     tg_new_tbin_20_28 + tg_new_tbin_gt_28) * dose_pnc +
  dose_pnc +
  hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
  rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
  fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
  COM^month + month^year + DEP^year

# ===========================================================================
# ESTIMATIONS
# ===========================================================================
feols_highAC_rich <- feols(fml, data = df_highAC_rich,
                           cluster = ~COM,
                           weights = ~value_estimated_population)

feols_highAC_poor <- feols(fml, data = df_highAC_poor,
                           cluster = ~COM,
                           weights = ~value_estimated_population)

feols_lowAC_rich  <- feols(fml, data = df_lowAC_rich,
                           cluster = ~COM,
                           weights = ~value_estimated_population)

feols_lowAC_poor  <- feols(fml, data = df_lowAC_poor,
                           cluster = ~COM,
                           weights = ~value_estimated_population)

# ===========================================================================
# HELPER
# ===========================================================================
make_coef_df <- function(model, groupe) {
  cf  <- coef(model)[1:4]
  ci  <- confint(model)[1:4, ]
  data.frame(
    Variable    = names(cf),
    Coefficient = as.numeric(cf),
    Lower_CI    = ci[, 1],
    Upper_CI    = ci[, 2],
    Groupe      = groupe
  ) %>%
    mutate(Variable = case_match(
      Variable,
      "tg_new_tbin_lt_0"  ~ "< 0",
      "tg_new_tbin_0_15"  ~ "0 to 15",
      "tg_new_tbin_20_28" ~ "20 to 28",
      "tg_new_tbin_gt_28" ~ "> 28",
      .default = Variable
    ))
}

ref_rows <- function(groupe) {
  data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,
             Lower_CI = 0, Upper_CI = 0, Groupe = groupe)
}

build_plot_df_4g <- function(m_hAr, m_hAp, m_lAr, m_lAp) {
  bind_rows(
    make_coef_df(m_hAr, "High AC — Rich"),
    make_coef_df(m_hAp, "High AC — Poor"),
    make_coef_df(m_lAr, "Low AC — Rich"),
    make_coef_df(m_lAp, "Low AC — Poor"),
    ref_rows("High AC — Rich"),
    ref_rows("High AC — Poor"),
    ref_rows("Low AC — Rich"),
    ref_rows("Low AC — Poor")
  ) %>%
    mutate(
      Variable = factor(Variable, levels = order_levels),
      Groupe   = factor(Groupe, levels = c("Low AC — Poor", "Low AC — Rich",
                                           "High AC — Poor", "High AC — Rich"))
    )
}

# ===========================================================================
# PALETTE
# ===========================================================================
palette_4g <- c(
  "Low AC — Poor"  = "#104E8B",
  "Low AC — Rich"  = "#A0CBE8",
  "High AC — Poor" = "#8B1A1A",
  "High AC — Rich" = "#FF6A6A"
)

# ===========================================================================
# FIGURE
# ===========================================================================
df_plot4g <- build_plot_df_4g(feols_highAC_rich, feols_highAC_poor,
                              feols_lowAC_rich,  feols_lowAC_poor)

fig_4g <- ggplot(df_plot4g,
                 aes(x = Variable, y = Coefficient,
                     color = Groupe, fill = Groupe, group = Groupe)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_line(linetype = "dashed", linewidth = 0.9,
            position = position_dodge(width = 0.3)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI),
              alpha = 0.15, color = NA,
              position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "solid",
             color = "black", linewidth = 0.5) +
  scale_color_manual(values = palette_4g) +
  scale_fill_manual(values  = palette_4g) +
  labs(
    x     = "Temperature bins (°C)",
    y     = "Mortality rate per 10,000 (75+)",
    color = NULL, fill = NULL,
    title = "Temperature–mortality relationship by AC penetration and income"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x    = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.text     = element_text(size = 11)
  )

fig_4g
ggsave("C:/Users/simon/Desktop/master_thesis/results/fig_pnc_4group.png", plot = fig_4g, width = 8, height = 7, dpi = 300)



# ===========================================================================
# ROBUSTESSE 1 ACCES SOIN 
# ===========================================================================

#acces soin * pnc * bins
feols_acces_soins <- feols(
  taux_mortalite_total ~ 
    (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
       tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + 
       tg_tbin_25_30 + tg_tbin_gt_30) * dose_pnc * soin +
    dose_pnc + densite + soin +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  
  data    = df,
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_acces_soins)

linearHypothesis(feols_acces_soins, "tg_tbin_gt_30:dose_pnc + tg_tbin_gt_30:dose_pnc:soin = 0")


# ===========================================================================
# ROBUSTESSE 2 : CHANGER PERIODE (2001-2010) 
# ===========================================================================
#income comme variable continue pour low AC sup28
feols_income_continue_lowAC28_2010 <- feols(
  taux_mortalite_75_plus ~ 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
       tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28) * dose_pnc * rich +
    dose_pnc + rich +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  
  data    = df_clim_low[year<=2010],
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_income_continue_lowAC28_2010)

#income comme variable continue pour high AC sup 28
feols_income_continue_highAC28_2010 <- feols(
  taux_mortalite_75_plus ~ 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
       tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28) * dose_pnc * rich +
    dose_pnc + rich +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  
  data    = df_clim_high[year<=2010],
  cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feols_income_continue_highAC28_2010)

# ===========================================================================
# STATS DESCRIPTIVE
# ===========================================================================
# Table A : taux AC aux années de vague
vagues <- c(2001, 2006, 2013, 2020)

table_vagues <- df %>%
  filter(year %in% vagues) %>%
  distinct(RG, year, taux_clim_RG) %>%
  pivot_wider(names_from = year, values_from = taux_clim_RG) %>%
  arrange(RG)


# table B 
table_jours <- df_clim %>%
  group_by(clim_quartile, COM, year) %>%
  summarise(jours_28_com = sum(tg_new_tbin_gt_28, na.rm = TRUE), .groups = "drop") %>%
  group_by(clim_quartile, year) %>%
  summarise(
    jours_28_moy = sum(jours_28_com) / n_distinct(COM),
    .groups = "drop"
  ) %>%
  group_by(clim_quartile) %>%
  summarise(jours_28_moy = mean(jours_28_moy, na.rm = TRUE), .groups = "drop") %>%
  arrange(clim_quartile)

# ===========================================================================
# 4.4 heterogeneity by density healthcare acces and level of services 
# ===========================================================================

# ===========================================================================
# density  
# ===========================================================================

#low ac  
feols_low_clim_dens_28 <- feols(
  taux_mortalite_75_plus ~ 
    # Température 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
       tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28) * dose_pnc * dens +
    
    #jours PNC activé
    dose_pnc + dens +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_low,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_low_clim_dens_28)

#high ac  
feols_high_clim_dens_28 <- feols(
  taux_mortalite_75_plus ~ 
    # Température 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
       tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28) * dose_pnc * dens +
    
    #jours PNC activé
    dose_pnc + dens +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_high,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_high_clim_dens_28)


# ===========================================================================
# soin  
# ===========================================================================

#low ac  
feols_low_clim_soin_28 <- feols(
  taux_mortalite_75_plus ~ 
    # Température 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
       tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28) * dose_pnc * soin +
    
    #jours PNC activé
    dose_pnc + soin +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_low,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_low_clim_soin_28)

#high ac  
feols_high_clim_soin_28 <- feols(
  taux_mortalite_75_plus ~ 
    # Température 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
       tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28) * dose_pnc * soin +
    
    #jours PNC activé
    dose_pnc + soin +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_high,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_high_clim_soin_28)



# ===========================================================================
# services  
# ===========================================================================

#low ac  
feols_low_clim_services_28 <- feols(
  taux_mortalite_75_plus ~ 
    # Température 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
       tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28) * dose_pnc * equip +
    
    #jours PNC activé
    dose_pnc + equip +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_low,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_low_clim_services_28)

#high ac  
feols_high_clim_services_28 <- feols(
  taux_mortalite_75_plus ~ 
    # Température 
    (tg_new_tbin_lt_0 + tg_new_tbin_0_15  +
       tg_new_tbin_20_28 +tg_new_tbin_gt_28 + tg_new_tbin_gt_28) * dose_pnc * equip +
    
    #jours PNC activé
    dose_pnc + equip +
    
    # Contrôles météo
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    
    # Fixed effects
    COM^month + month^year + DEP^year,
  
  data = df_clim_high,
  cluster = ~COM,
  weights = ~value_estimated_population
)

summary(feols_high_clim_services_28)




# Corrélation entre dens, soin et equip
cor_matrix <- df_clim %>%
  filter(year == 2001) %>%
  distinct(COM, dens, soin, equip) %>%
  select(dens, soin, equip) %>%
  cor(use = "complete.obs")

print(cor_matrix)

# Tableau croisé dens x soin
table(df_clim %>% filter(year == 2001) %>% distinct(COM, dens, soin) %>% pull(dens),
      df_clim %>% filter(year == 2001) %>% distinct(COM, dens, soin) %>% pull(soin))

# ===========================================================================
# FIGURE
# ===========================================================================

# ===========================================================================
# PNC BY DEP*YEAR
# ===========================================================================


dept_sf <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson")

# ===========================================================================
# AGRÉGATION
# ===========================================================================
prep_jours <- function(data, yr) {
  data %>%
    filter(year == yr) %>%
    group_by(DEP) %>%
    summarise(n_jours_pnc = max(n_jours_pnc, na.rm = TRUE), .groups = "drop") %>%
    mutate(DEP = stringr::str_pad(as.character(DEP), width = 2, pad = "0"))
}

jours_2006 <- prep_jours(df, 2006)
jours_2010 <- prep_jours(df, 2010)
jours_2014 <- prep_jours(df, 2014)
jours_2019 <- prep_jours(df, 2019)

# ===========================================================================
# JOIN
# ===========================================================================
map_2006 <- dept_sf %>% left_join(jours_2006, by = c("code" = "DEP"))
map_2010 <- dept_sf %>% left_join(jours_2010, by = c("code" = "DEP"))
map_2014 <- dept_sf %>% left_join(jours_2014, by = c("code" = "DEP"))
map_2019 <- dept_sf %>% left_join(jours_2019, by = c("code" = "DEP"))

# ===========================================================================
# PALETTE
# ===========================================================================
breaks <- c(0, 1, 4, 7, 11, Inf)
labels <- c("0", "1-3", "4-6", "7-10", "11+")
colors <- c("0"    = "white",
            "1-3"  = "#fcc5c0",
            "4-6"  = "#df65b0",
            "7-10" = "#dd1c77",
            "11+"  = "#67001f")

add_cat <- function(data) {
  data %>% mutate(cat = factor(
    cut(n_jours_pnc, breaks = breaks, labels = labels,
        include.lowest = TRUE, right = FALSE),
    levels = labels
  ))
}

map_2006 <- add_cat(map_2006)
map_2010 <- add_cat(map_2010)
map_2014 <- add_cat(map_2014)
map_2019 <- add_cat(map_2019)

# ===========================================================================
# PLOT
# ===========================================================================
plot_map <- function(data, titre) {
  ggplot(data) +
    geom_sf(aes(fill = cat), color = "grey40", linewidth = 0.2) +
    scale_fill_manual(values = colors, labels = labels, na.value = "grey90",
                      name = "Number of days\nPNC triggered",
                      drop = FALSE) +
    labs(title = titre, caption = "Source: Météo-France") +
    theme_void() +
    theme(
      plot.title      = element_text(color = "#1f4e79", face = "bold", size = 13),
      plot.caption    = element_text(hjust = 0, color = "#1f4e79", size = 9),
      legend.position = "right"
    )
}

fig_2006 <- plot_map(map_2006, "Number of PNC days triggered by department — 2006")
fig_2010 <- plot_map(map_2010, "Number of PNC days triggered by department — 2010")
fig_2014 <- plot_map(map_2014, "Number of PNC days triggered by department — 2014")
fig_2019 <- plot_map(map_2019, "Number of PNC days triggered by department — 2019")

print(fig_2006)
print(fig_2010)
print(fig_2014)
print(fig_2019)

# ===========================================================================
# EXPORT
# ===========================================================================
ggsave("C:/Users/simon/Desktop/master_thesis/results/fig_pnc_2006.png", plot = fig_2006, width = 8, height = 7, dpi = 300)
ggsave("C:/Users/simon/Desktop/master_thesis/results/fig_pnc_2010.png", plot = fig_2010, width = 8, height = 7, dpi = 300)
ggsave("C:/Users/simon/Desktop/master_thesis/results/fig_pnc_2014.png", plot = fig_2014, width = 8, height = 7, dpi = 300)
ggsave("C:/Users/simon/Desktop/master_thesis/results/fig_pnc_2019.png", plot = fig_2019, width = 8, height = 7, dpi = 300)

