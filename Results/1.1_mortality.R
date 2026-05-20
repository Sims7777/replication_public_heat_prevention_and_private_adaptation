library(data.table)
library(dplyr)
library(fixest)
library(ggplot2)
library(arrow)
library(MetBrewer)

# ===========================================================================
# CHARGEMENT
# ===========================================================================
df <- fread("C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet6.csv")

df_pre  <- df %>% filter(year < 2004)
df_post <- df %>% filter(year >= 2004)

# ===========================================================================
# FIGURE 1
# ===========================================================================
# ===========================================================================
# RÉGRESSIONS
# ===========================================================================
summary(feolss1 <- feols(taux_mortalite_total ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
                           tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
                           hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
                           rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
                           fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
                           COM^month + month^year + DEP^year,
                         data = df_pre, se = "cluster", cluster = ~COM, weights = ~value_estimated_population))

summary(feolss2 <- feols(taux_mortalite_total ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
                           tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
                           hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
                           rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
                           fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
                           COM^month + month^year + DEP^year,
                         data = df_post, se = "cluster", cluster = ~COM, weights = ~value_estimated_population))

summary(feolss3 <- feols(taux_mortalite_75_plus ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
                           tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
                           hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
                           rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
                           fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
                           COM^month + month^year + DEP^year,
                         data = df_pre, se = "cluster", cluster = ~COM, weights = ~value_estimated_population))

summary(feolss4 <- feols(taux_mortalite_75_plus ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
                           tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
                           hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
                           rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
                           fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
                           COM^month + month^year + DEP^year,
                         data = df_post, se = "cluster", cluster = ~COM, weights = ~value_estimated_population))

etable(feolss1, feolss2, feolss3, feolss4, tex = TRUE)

# ===========================================================================
# HELPER — construction coef_df pour un modèle
# ===========================================================================
make_coef_df <- function(model, periode) {
  data.frame(
    Variable    = names(coef(model)),
    Coefficient = coef(model),
    Lower_CI    = confint(model)[, 1],
    Upper_CI    = confint(model)[, 2],
    Periode     = periode
  )[1:8, ]
}

build_plot_df <- function(m_pre, m_post) {
  bind_rows(
    make_coef_df(m_pre,  "1980–2003"),
    make_coef_df(m_post, "2004–2019")
  ) %>%
    mutate(Variable = recode(Variable, !!!temp_recode)) %>%
    bind_rows(
      data.frame(Variable = "15 to 20 (REF)", Coefficient = 0, Lower_CI = 0, Upper_CI = 0, Periode = "1980–2003"),
      data.frame(Variable = "15 to 20 (REF)", Coefficient = 0, Lower_CI = 0, Upper_CI = 0, Periode = "2004–2019")
    )
}

palette_teal <- c("1980–2003" = "#4E79A7", "2004–2019" = "#F28E2B")

# ===========================================================================
# FIGURE 1 — mortalité totale
# ===========================================================================
df_plot1 <- build_plot_df(feolss1, feolss2) %>%
  mutate(Variable = factor(Variable, levels = order_levels))

fig1 <- ggplot(df_plot1, aes(x = Variable, y = Coefficient, fill = Periode, color = Periode)) +
  geom_point(size = 3) +
  geom_line(aes(group = Periode), linetype = "dashed", linewidth = 1) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Periode), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  scale_color_manual(values = palette_teal) +
  scale_fill_manual(values = palette_teal) +
  labs(x = "Temperature bins (°C)", y = "Mortality rate per 10,000",
       color = "Period", fill = "Period",
       title = "Total mortality") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"))

# ===========================================================================
# FIGURE 2 — mortalité 75+
# ===========================================================================
df_plot2 <- build_plot_df(feolss3, feolss4) %>%
  mutate(Variable = factor(Variable, levels = order_levels))

fig2 <- ggplot(df_plot2, aes(x = Variable, y = Coefficient, fill = Periode, color = Periode)) +
  geom_point(size = 3) +
  geom_line(aes(group = Periode), linetype = "dashed", linewidth = 1) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Periode), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  scale_color_manual(values = palette_teal) +
  scale_fill_manual(values = palette_teal) +
  labs(x = "Temperature bins (°C)", y = "Mortality rate per 10,000",
       color = "Period", fill = "Period",
       title = "Mortality 75+") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"))
# ===========================================================================
# EXPORT
# ===========================================================================
ggsave("C:/Users/simon/Desktop/master_thesis/results/fig_mortalite_totale_pre_post.png",  plot = fig1, width = 10, height = 6, dpi = 300)
ggsave("C:/Users/simon/Desktop/master_thesis/results/fig_mortalite_75plus_pre_post.png",  plot = fig2, width = 10, height = 6, dpi = 300)

# ===========================================================================
# Tableau 1-A
# ===========================================================================
etable(feolss1, feolss2, feolss3, feolss4,
       keep_raw = c("tg_tbin_lt_m5", "tg_tbin_m5_0", "tg_tbin_0_5", "tg_tbin_5_10",
                    "tg_tbin_10_15", "tg_tbin_20_25", "tg_tbin_25_30", "tg_tbin_gt_30"),
       dict = c(tg_tbin_lt_m5 = "$<$ -5 \\textdegree C",
                tg_tbin_m5_0  = "-5 to 0 \\textdegree C",
                tg_tbin_0_5   = "0 to 5 \\textdegree C",
                tg_tbin_5_10  = "5 to 10 \\textdegree C",
                tg_tbin_10_15 = "10 to 15 \\textdegree C",
                tg_tbin_20_25 = "20 to 25 \\textdegree C",
                tg_tbin_25_30 = "25 to 30 \\textdegree C",
                tg_tbin_gt_30 = "$>$ 30 \\textdegree C"),
       depvar       = FALSE,
       drop.section = "fixef",
       extralines   = list(
         "_Period"               = c("1980--2003", "2004--2019", "1980--2003", "2004--2019"),
         "Controls"              = "---",
         "Wind"                  = c("Yes", "Yes", "Yes", "Yes"),
         "Rain"                  = c("Yes", "Yes", "Yes", "Yes"),
         "Humidity"              = c("Yes", "Yes", "Yes", "Yes"),
         "Fixed-effects"         = "---",
         "Municipality by Month" = c("Yes", "Yes", "Yes", "Yes"),
         "Month by Year"         = c("Yes", "Yes", "Yes", "Yes"),
         "Region by Year"        = c("Yes", "Yes", "Yes", "Yes")
       ),
       notes = "OLS estimates. Clustered (Municipality) standard-errors in parentheses. Regressions weighted by population. *p$<$0.1; **p$<$0.05; ***p$<$0.01.",
       tpt   = FALSE,
       tex   = TRUE,
       file  = "C:/Users/simon/Desktop/master_thesis/results/table_pre_post.tex")

# ===========================================================================
# RÉGRESSIONS PRINCIPALES — toutes tranches d'âge
# ===========================================================================

summary(feolss1 <- feols(
  taux_mortalite_total ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df, se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))


summary(feolss2 <- feols(
  taux_mortalite_75_plus ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df, se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))


summary(feolss3 <- feols(
  taux_mortalite_70_74 ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df, se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))


summary(feolss4 <- feols(
  taux_mortalite_65_69 ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df, se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))


summary(feolss5 <- feols(
  taux_mortalite_60_64 ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df, se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))


summary(feolss6 <- feols(
  taux_mortalite_40_59 ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df, se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))


summary(feolss7 <- feols(
  taux_mortalite_20_39 ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df, se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))


summary(feolss8 <- feols(
  taux_mortalite_10_19 ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df, se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))


summary(feolss9 <- feols(
  taux_mortalite_0_9 ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df, se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))



# ===========================================================================
# TABLE
# ===========================================================================
etable(feolss1, feolss2, feolss3, feolss4, feolss5,
       feolss6, feolss7, feolss8, feolss9, tex = TRUE)

# ===========================================================================
# Courbe température × mortalité totale
# ===========================================================================
BIN_ORDER <- c("tg_tbin_lt_m5", "tg_tbin_m5_0", "tg_tbin_0_5",
               "tg_tbin_5_10",  "tg_tbin_10_15", "tg_tbin_15_20",
               "tg_tbin_20_25", "tg_tbin_25_30", "tg_tbin_gt_30")

BIN_LABELS <- c("<-20 to -5", "-5 to 0", "0 to 5", "5 to 10", "10 to 15",
                "15 to 20\n(REF)", "20 to 25", "25 to 30", ">30")

coefficients <- coef(feolss1)
conf_int     <- confint(feolss1)

coef_df <- data.frame(
  Variable    = names(coefficients),
  Coefficient = as.numeric(coefficients),
  Lower_CI    = conf_int[, 1],
  Upper_CI    = conf_int[, 2]
)
coef_df <- coef_df[grep("^tg_tbin", coef_df$Variable), ]
coef_df <- rbind(coef_df,
                 data.frame(Variable = "tg_tbin_15_20",
                            Coefficient = 0, Lower_CI = 0, Upper_CI = 0))
coef_df <- coef_df[match(BIN_ORDER, coef_df$Variable), ]
coef_df$Label <- factor(BIN_LABELS, levels = BIN_LABELS)

ggplot(coef_df, aes(x = Label, y = Coefficient, group = 1)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2) +
  geom_line(linetype = "dashed") +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Temperature bins (°C)", y = "Mortality rate per 10,000") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"))

# ===========================================================================
# Coefficient >30°C par tranche d'âge
# ===========================================================================
models_age <- list(feolss2, feolss3, feolss4, feolss5,
                   feolss6, feolss7, feolss8, feolss9)
age_labels <- c("75+", "70-74", "65-69", "60-64",
                "40-59", "20-39", "10-19", "0-9")

coef_df2 <- data.frame(
  AgeGroup    = age_labels,
  Coefficient = sapply(models_age, function(m) coef(m)["tg_tbin_gt_30"]),
  Lower_CI    = sapply(models_age, function(m) confint(m)["tg_tbin_gt_30", 1]),
  Upper_CI    = sapply(models_age, function(m) confint(m)["tg_tbin_gt_30", 2])
)
coef_df2$AgeGroup <- factor(coef_df2$AgeGroup, levels = age_labels)

ggplot(coef_df2, aes(x = AgeGroup, y = Coefficient)) +
  geom_point(size = 2.5, color = "darkblue") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = "Age Groups", y = "Coefficients for Temperatures > 30°C") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"))




# ===========================================================================
# RÉGRESSIONS SECONDAIRES — JUSTE DENSITE
# ===========================================================================
base_dens  <- df[dens == 1]
base_sparse <- df[dens != 1]


summary(feolss1 <- feols(
  taux_mortalite_total ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = base_dens, se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))

summary(feolss1 <- feols(
  taux_mortalite_total ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = base_sparse, se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))

summary(feolss2 <- feols(
  taux_mortalite_75_plus ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = base_dens, se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))

summary(feolss2 <- feols(
  taux_mortalite_75_plus ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = base_sparse, se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))



# ===========================================================================
# RÉGRESSIONS SECONDAIRES — DENSITE*INCOME
# ===========================================================================
base_dens_inc <- base_dens[year > 2001]
base_sparse_inc <- base_sparse[year > 2001]


# ── Fonction utilitaire ───────────────────────────────────────────────────────
get_marginal_effects <- function(mod, income_val, vce) {
  bins <- c("tg_new_tbin_lt_0", "tg_new_tbin_0_15", "tg_new_tbin_20_28", "tg_new_tbin_gt_28")
  
  purrr::map_dfr(bins, function(b) {
    interact_name <- paste0(b, ":median_uc_z")
    
    coef_b  <- coef(mod)[b]
    coef_i  <- coef(mod)[interact_name]
    se_b    <- sqrt(vce[b, b])
    se_i    <- sqrt(vce[interact_name, interact_name])
    cov_bi  <- vce[b, interact_name]
    
    effect <- coef_b + coef_i * income_val
    se_eff <- sqrt(se_b^2 + income_val^2 * se_i^2 + 2 * income_val * cov_bi)
    
    data.frame(bin = b, estimate = effect, se = se_eff)
  })
}

# ── Modèles ───────────────────────────────────────────────────────────────────
mod_dens <- feols(
  taux_mortalite_total ~ (tg_new_tbin_lt_0 + tg_new_tbin_0_15 + tg_new_tbin_20_28 + tg_new_tbin_gt_28) * median_uc_z +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = base_dens_inc,
  cluster = ~COM,
  weights = ~value_estimated_population
)

mod_sparse <- feols(
  taux_mortalite_total ~ (tg_new_tbin_lt_0 + tg_new_tbin_0_15 + tg_new_tbin_20_28 + tg_new_tbin_gt_28) * median_uc_z +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data    = base_sparse_inc,
  cluster = ~COM,
  weights = ~value_estimated_population
)

# ── Effets marginaux ──────────────────────────────────────────────────────────
vce_dens   <- vcov(mod_dens)
vce_sparse <- vcov(mod_sparse)

ref_row <- data.frame(bin = "tg_new_tbin_15_20", estimate = 0, se = 0,
                      conf.low = 0, conf.high = 0)

results <- bind_rows(
  get_marginal_effects(mod_dens,   -1, vce_dens)   %>% mutate(density = "High density", income_label = "Low income"),
  get_marginal_effects(mod_dens,    1, vce_dens)   %>% mutate(density = "High density", income_label = "High income"),
  get_marginal_effects(mod_sparse, -1, vce_sparse) %>% mutate(density = "Low density",  income_label = "Low income"),
  get_marginal_effects(mod_sparse,  1, vce_sparse) %>% mutate(density = "Low density",  income_label = "High income")
) %>%
  mutate(conf.low = estimate - 1.96 * se, conf.high = estimate + 1.96 * se) %>%
  bind_rows(
    expand.grid(
      bin          = "tg_new_tbin_15_20",
      estimate     = 0,
      se           = 0,
      conf.low     = 0,
      conf.high    = 0,
      density      = c("High density", "Low density"),
      income_label = c("Low income", "High income"),
      stringsAsFactors = FALSE
    )
  ) %>%
  mutate(
    bin = factor(bin,
                 levels = c("tg_new_tbin_lt_0", "tg_new_tbin_0_15", "tg_new_tbin_15_20",
                            "tg_new_tbin_20_28", "tg_new_tbin_gt_28"),
                 labels = c("Below 0", "0 to 15", "15 to 20", "20 to 28", "Above 28")),
    panel = factor(paste(income_label, "&", tolower(density)),
                   levels = c("Low income & high density", "High income & high density",
                              "Low income & low density",  "High income & low density"))
  )

# ── Figure 4 ──────────────────────────────────────────────────────────────────
ggplot(results, aes(x = bin, y = estimate, group = panel,
                    ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(aes(fill = panel), alpha = 0.2) +
  geom_line(aes(color = panel), linewidth = 0.8) +
  geom_point(aes(color = panel), size = 2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.4) +
  geom_text(data = results %>% filter(bin == "Above 28"),
            aes(label = round(estimate, 3), color = panel),
            vjust = -0.8, size = 3) +
  facet_wrap(~panel, nrow = 1) +
  scale_color_manual(values = c("Low income & high density"  = "#4B0082",
                                "High income & high density" = "#1E90FF",
                                "Low income & low density"   = "#2E8B57",
                                "High income & low density"  = "#FF6347")) +
  scale_fill_manual(values  = c("Low income & high density"  = "#4B0082",
                                "High income & high density" = "#1E90FF",
                                "Low income & low density"   = "#2E8B57",
                                "High income & low density"  = "#FF6347")) +
  labs(x = "Temperature bins (°C)", y = "Mortality rate per 10,000") +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1))


# ===========================================================================
# PATERN DE REDUCTION DE LA MORTALITE
# ===========================================================================
summary(feolss1 <- feols(
  taux_mortalite_total ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df[year < 2003], se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))

summary(feolss1 <- feols(
  taux_mortalite_total ~ tg_tbin_lt_m5 + tg_tbin_m5_0 + tg_tbin_0_5 +
    tg_tbin_5_10 + tg_tbin_10_15 + tg_tbin_20_25 + tg_tbin_25_30 + tg_tbin_gt_30 +
    hubin_0_20 + hubin_40_60 + hubin_60_80 + hubin_80_100 +
    rrbin_0mm + rrbin_0_3 + rrbin_10_100 + rrbin_gt_100 +
    fgbin_0_3 + fgbin_10_20 + fgbin_gt_20 |
    COM^month + month^year + DEP^year,
  data = df[year >= 2004], se = "cluster", cluster = ~COM,
  weights = ~value_estimated_population
))



