library(data.table)
library(dplyr)
library(fixest)
library(ggplot2)
library(car)
library(sf)
library(purrr)
library(broom)

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

# ===========================================================================
# FIGURE 2: Effect of PNC activation on over-75 mortality by air conditioning group
# ===========================================================================
df_fig2 <- data.frame(
  quartile = factor(
    c("Q1-Q2\n(lowest AC)", "Q3", "Q4\n(highest AC)"),
    levels = c("Q1-Q2\n(lowest AC)", "Q3", "Q4\n(highest AC)")
  ),
  coef = c(-0.4566, -0.1360,  0.0037),
  se   = c( 0.0524,  0.0298,  0.0122),
  sig  = c(TRUE, TRUE, FALSE)
) %>%
  mutate(
    ci_lo = coef - 1.96 * se,
    ci_hi = coef + 1.96 * se
  )

ggplot(df_fig2, aes(x = quartile, y = coef)) +
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
    x = "Air conditioning penetration group (2001)",
    y = "Effect on mortality rate (75+)\ndeaths per 10,000 inhabitants"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.y       = element_text(size = 10, margin = margin(r = 10)),
    axis.title.x       = element_text(size = 10, margin = margin(t = 10)),
    axis.text          = element_text(size = 10, color = "grey20"),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.4),
    plot.margin        = margin(12, 16, 8, 8)
  )

ggsave("C:/Users/simon/Desktop/master_thesis/results/figure_pnc_ac_pooled.pdf", width = 6, height = 4.5, device = "pdf")
ggsave("C:/Users/simon/Desktop/master_thesis/results/figure_pnc_ac_pooled.png", width = 6, height = 4.5, dpi = 300)

# ===========================================================================
# FIGURE 3: TEMPERATURE x 75+ MORTALITY BY AC GROUP AND INCOME
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
# FIGURE 4: MAPS AC PREVALENCE AND HISTORICAL HEAT EXPOSURE
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
}

# ===========================================================================
# FIGURE 5: PARALLEL TRENDS VALIDATION
# ===========================================================================

# 2001 median
mediane_2001 <- df %>%
  filter(year == 2001) %>%
  summarise(m = median(taux_clim_RG, na.rm = TRUE)) %>%
  pull(m)

# classify by com in 2001
lowac_2001 <- df %>%
  filter(year == 2001) %>%
  select(COM, taux_clim_RG) %>%
  distinct() %>%
  mutate(low_ac = as.integer(taux_clim_RG < mediane_2001)) %>%
  select(COM, low_ac)

# join tout the panel
df <- df %>%
  left_join(lowac_2001, by = "COM") %>%
  mutate(tg28_x_lowac = tg_new_tbin_gt_28 * low_ac)

# Verification
df %>%
  group_by(year) %>%
  summarise(pct_lowac_na = mean(is.na(low_ac))) %>%
  print(n = 40)

# year by year estimation 
annees <- 1990:2003

coefs_total <- map_dfr(annees, function(a) {

  df_yr <- df %>%
    filter(year == a) %>%
    drop_na(taux_mortalite_total, tg_new_tbin_gt_28, tg28_x_lowac)

  tryCatch({
    m <- feols(
      taux_mortalite_total ~
        tg_new_tbin_gt_28 + tg28_x_lowac +
        tg_new_tbin_lt_0 + tg_new_tbin_0_15 + tg_new_tbin_20_28 +
        hubin_0_20 + hubin_20_40 + hubin_40_60 + hubin_60_80 +
        rrbin_0_3 + rrbin_3_10 + rrbin_10_100 + rrbin_gt_100 +
        fgbin_0_3 + fgbin_3_10 + fgbin_10_20 + fgbin_gt_20 |
        COM + month,
      data    = df_yr,
      weights = ~population_totale,
      cluster = ~COM
    )

    broom::tidy(m) %>%
      filter(term == "tg28_x_lowac") %>%
      mutate(year = a)

  }, error = function(e) {
    message("Erreur année : ", a, " — ", conditionMessage(e))
    NULL
  })
})

print(coefs_total)

coefs_total <- coefs_total %>%
  mutate(
    conf_low    = estimate - 1.96 * std.error,
    conf_high   = estimate + 1.96 * std.error,
    significant = as.factor(p.value < 0.05)
  )

# Formal test for a linear trend (excluding 2003)
mod_trend <- lm(
  estimate ~ year,
  data    = coefs_total %>% filter(year < 2003),
  weights = 1 / std.error^2
)
summary(mod_trend)

# Figure 5
ggplot(coefs_total, aes(x = year, y = estimate)) +
  geom_point(aes(shape = significant), size = 3) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 2003.5, linetype = "dotted", color = "red") +
  annotate("text", x = 2003.3, y = 3.5,
           label = "PNC introduced", hjust = 1, size = 3.5, color = "red") +
  annotate("text", x = 1997, y = 3.8,
           label = "1997: 6.25\n(SE: 2.99)", size = 3, hjust = 0.5, color = "grey40") +
  annotate("segment", x = 1997, xend = 1997, y = 3.5, yend = 4.05,
           arrow = arrow(length = unit(0.2, "cm")), color = "grey40") +
  coord_cartesian(ylim = c(-2, 4)) +
  scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16),
                     labels = c("p > 0.05", "p < 0.05"),
                     name   = "") +
  labs(
    x       = "Year",
    y       = "Differential coefficient",
    caption = "Notes: OLS estimates with 95% CI. FE: commune, month. Clustered SE at commune level.\nWeighted by population. 2003 excluded from trend test (2003 heatwave).\nLinear trend test on 1990-2002: slope = -0.003, p = 0.911."
  ) +
  theme_minimal(base_size = 12)

ggsave("C:/Users/simon/Desktop/master_thesis/results/parallel_trends_final.png",
       width = 10, height = 6, dpi = 300)
