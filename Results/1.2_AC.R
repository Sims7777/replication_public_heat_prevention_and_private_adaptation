library(data.table)
library(dplyr)
library(fixest)
library(ggplot2)
library(car)

# ===========================================================================
# LOAD
# ===========================================================================
df <- fread("C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet6.csv")

# ===========================================================================
# MEDIAN SPLIT ON 2001 REGIONAL AC RATE
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
# QUARTILE SPLIT ON 2001 REGIONAL AC RATE
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

df_clim    <- df %>% filter(year >= 2001) %>% left_join(dep_clim_2001, by = "DEP")
df_clim_q1_2 <- df_clim %>% filter(clim_quartile %in% c(1, 2))
df_clim_q3   <- df_clim %>% filter(clim_quartile == 3)
df_clim_q4   <- df_clim %>% filter(clim_quartile == 4)

# ===========================================================================
# SHARED FORMULA COMPONENTS
# ===========================================================================
temp_bins <- c("tg_tbin_lt_m5", "tg_tbin_m5_0", "tg_tbin_0_5",
               "tg_tbin_5_10",  "tg_tbin_10_15", "tg_tbin_20_25",
               "tg_tbin_25_30", "tg_tbin_gt_30")

weather_controls <- c("hubin_0_20", "hubin_40_60", "hubin_60_80", "hubin_80_100",
                      "rrbin_0mm",  "rrbin_0_3",   "rrbin_10_100", "rrbin_gt_100",
                      "fgbin_0_3",  "fgbin_10_20", "fgbin_gt_20")

fe_str  <- "COM^month + month^year + DEP^year"
df_2001 <- df[year >= 2001]

# ===========================================================================
# TABLE 2: TOTAL MORTALITY BY AC GROUP (low vs high)
# ===========================================================================
feols_low_total <- feols(
  taux_mortalite_total ~
    tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 + tg_tbin_5_10 + tg_tbin_10_15 +
    tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = df_clim_low,
  cluster = ~COM,
  weights = ~value_estimated_population
)

feols_high_total <- feols(
  taux_mortalite_total ~
    tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 + tg_tbin_5_10 + tg_tbin_10_15 +
    tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = df_clim_high,
  cluster = ~COM,
  weights = ~value_estimated_population
)

etable(feols_low_total, feols_high_total, tex = TRUE)

# ===========================================================================
# TABLE 3: 75+ MORTALITY BY AC QUARTILE
# ===========================================================================
run_75_quartile <- function(data) {
  feols(
    taux_mortalite_75_plus ~
      tg_new_tbin_lt_0 + tg_new_tbin_0_15 + tg_new_tbin_20_28 + tg_new_tbin_gt_28 +
      hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
      rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
      fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
      COM^month + month^year + DEP^year,
    data    = data,
    cluster = ~COM,
    weights = ~value_estimated_population
  )
}

feols_q1 <- run_75_quartile(df_clim %>% filter(clim_quartile == 1))
feols_q2 <- run_75_quartile(df_clim %>% filter(clim_quartile == 2))
feols_q3 <- run_75_quartile(df_clim_q3)
feols_q4 <- run_75_quartile(df_clim_q4)

etable(feols_q1, feols_q2, feols_q3, feols_q4, tex = TRUE)

# ===========================================================================
# TABLE F: Q1+Q2 POOLED
# ===========================================================================
feols_q12 <- run_75_quartile(df_clim_q1_2)

etable(feols_q12, feols_q3, feols_q4, tex = TRUE)

# ===========================================================================
# TABLE G: INCOME HETEROGENEITY WITHIN AC GROUPS (75+)
# ===========================================================================
run_income_75 <- function(data) {
  feols(
    taux_mortalite_75_plus ~
      (tg_new_tbin_lt_0 + tg_new_tbin_0_15 + tg_new_tbin_20_28 + tg_new_tbin_gt_28) * rich +
      rich +
      hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
      rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
      fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
      COM^month + month^year + DEP^year,
    data    = data,
    cluster = ~COM,
    weights = ~value_estimated_population
  )
}

feols_low_income  <- run_income_75(df_clim_low)
feols_high_income <- run_income_75(df_clim_high)

etable(feols_low_income, feols_high_income, tex = TRUE)

# ===========================================================================
# TABLE H: RESTRICTED TO 2001-2010
# ===========================================================================
feols_low_2010  <- run_income_75(df_clim_low  %>% filter(year <= 2010))
feols_high_2010 <- run_income_75(df_clim_high %>% filter(year <= 2010))

etable(feols_low_2010, feols_high_2010, tex = TRUE)library(data.table)
library(dplyr)
library(fixest)
library(ggplot2)
library(arrow)
library(car)
#gc()
# ===========================================================================
# CHARGEMENT
# ===========================================================================
df <- fread("C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet6.csv")

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
df_clim_q3 <- df_clim %>% filter(clim_quartile == 3)
df_clim_q4 <- df_clim %>% filter(clim_quartile == 4)


# ===========================================================================
# RÉGRESSIONS PRINCIPALES — toutes tranches d'âge
# ===========================================================================
#baseline
feolss <- feols(
  taux_mortalite_total ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
                            tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 + taux_clim_RG |
    COM^month + month^year + DEP^year,
  data = df[year >= 2001], 
  se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feolss)

#baseline*AC
feolss <- feols(
  taux_mortalite_total ~ (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30)*taux_clim_RG +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 + taux_clim_RG |
    COM^month + month^year + DEP^year,
  data = df[year >= 2001], 
  se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
)
summary(feolss)


# ===========================================================================
# RÉGRESSIONS taux mortalité - AC - rich 
# ===========================================================================

#toute tranche d'âge
summary(feolss1 <- feols(
  taux_mortalite_total ~ (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30)*taux_clim_RG*rich +
    rich + taux_clim_RG +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df[year >= 2001], se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))


#seulement sur les 75+
summary(feolss1 <- feols(
  taux_mortalite_75_plus ~ (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
                            tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30)*taux_clim_RG*rich +
    rich + taux_clim_RG +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df[year >= 2001], se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))




df_equip <- df %>% filter(equip == 1)
df_non_equip <- df %>% filter(equip == 0)
#toute tranche dage communes bien equipees + haut services
summary(feolss1 <- feols(
  taux_mortalite_total ~ (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
                            tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30)*taux_clim_RG*rich +
    rich + taux_clim_RG +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df_equip[year >= 2001], se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))

#toute tranche dage communes mal equipees + bas services
summary(feolss1 <- feols(
  taux_mortalite_total ~ (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
                            tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30)*taux_clim_RG*rich +
    rich + taux_clim_RG +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df_non_equip[year >= 2001], se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))


df_soin <- df %>% filter(soin == 0)
#toute tranche dage communes avec peu daccces soins
summary(feolss1 <- feols(
  taux_mortalite_total ~ (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
                            tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30)*taux_clim_RG*rich +
    rich + taux_clim_RG +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df_soin[year >= 2001], se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))


df_dens <- df %>% filter(dens == 1)
#toute tranche dage communes denses
summary(feolss1 <- feols(
  taux_mortalite_total ~ (tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
                            tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30)*taux_clim_RG*rich +
    rich + taux_clim_RG +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df_dens[year >= 2001], se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))

# ==============================================================================
# EXTRACTION DES COEFFICIENTS
# ==============================================================================

extract_coefs <- function(model, var_control) {
  coefs <- coef(model)
  se <- se(model)
  pval <- pvalue(model)
  
  # Fonction pour formater significativité
  sig <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("***")
    if (p < 0.01) return("**")
    if (p < 0.05) return("*")
    if (p < 0.1) return(".")
    return("")
  }
  
  data.table(
    Controle = var_control,
    Effet_canicule = round(coefs["tg_tbin_gt_30"], 3),
    SE_canicule = round(se["tg_tbin_gt_30"], 3),
    Sig_canicule = sig(pval["tg_tbin_gt_30"]),
    Coef_controle = round(coefs[var_control], 3),
    SE_controle = round(se[var_control], 3),
    Sig_controle = sig(pval[var_control]),
    Interaction_clim = round(coefs["tg_tbin_gt_30:taux_clim_RG"], 3),
    SE_interaction = round(se["tg_tbin_gt_30:taux_clim_RG"], 3),
    Sig_interaction = sig(pval["tg_tbin_gt_30:taux_clim_RG"])
  )
}

# Construire le tableau
results75 <- rbindlist(list(
  extract_coefs(feols_tfpb75, "tfpb"),
  extract_coefs(feols_post200075, "pct_post_2000"),
  extract_coefs(feols_proprio75, "pct_proprietaires"),
  extract_coefs(feols_chomage75, "taux_chomage"),
  extract_coefs(feols_csp_haute75, "pct_csp_haute"),
  extract_coefs(feols_csp_basse75, "pct_csp_basse"),
  extract_coefs(feols_densite75, "densite")
))

# ==============================================================================
# AFFICHAGE
# ==============================================================================

cat("\n=== TABLEAU DE ROBUSTESSE : CONTRÔLES SOCIO-ÉCONOMIQUES ===\n\n")
print(results75)
