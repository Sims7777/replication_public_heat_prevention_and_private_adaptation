library(data.table)
library(dplyr)
library(fixest)
library(ggplot2)

# ===========================================================================
# LOAD
# ===========================================================================
df <- fread("C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet6.csv")

df_pre  <- df %>% filter(year < 2004)
df_post <- df %>% filter(year >= 2004)

# ===========================================================================
# FIGURE 1 — temperature-mortality relationship, pre vs post 2004
# ===========================================================================
temp_recode <- c(
  tg_tbin_lt_m5  = "< -5",
  tg_tbin_m5_0   = "-5 to 0",
  tg_tbin_0_5    = "0 to 5",
  tg_tbin_5_10   = "5 to 10",
  tg_tbin_10_15  = "10 to 15",
  tg_tbin_20_25  = "20 to 25",
  tg_tbin_25_30  = "25 to 30",
  tg_tbin_gt_30  = "> 30"
)

order_levels <- c("< -5", "-5 to 0", "0 to 5", "5 to 10", "10 to 15",
                  "15 to 20 (REF)", "20 to 25", "25 to 30", "> 30")

weather_controls <- c("hubin_0_20", "hubin_40_60", "hubin_60_80", "hubin_80_100",
                      "rrbin_0mm", "rrbin_0_3", "rrbin_10_100", "rrbin_gt_100",
                      "fgbin_0_3", "fgbin_10_20", "fgbin_gt_20")

temp_bins <- c("tg_tbin_lt_m5", "tg_tbin_m5_0", "tg_tbin_0_5",
               "tg_tbin_5_10", "tg_tbin_10_15", "tg_tbin_20_25",
               "tg_tbin_25_30", "tg_tbin_gt_30")

fml_total <- as.formula(paste(
  "taux_mortalite_total ~",
  paste(c(temp_bins, weather_controls), collapse = " + "),
  "| COM^month + month^year + DEP^year"
))

fml_75 <- as.formula(paste(
  "taux_mortalite_75_plus ~",
  paste(c(temp_bins, weather_controls), collapse = " + "),
  "| COM^month + month^year + DEP^year"
))

feolss1 <- feols(fml_total, data = df_pre,  se = "cluster", cluster = ~COM, weights = ~value_estimated_population)
feolss2 <- feols(fml_total, data = df_post, se = "cluster", cluster = ~COM, weights = ~value_estimated_population)
feolss3 <- feols(fml_75,    data = df_pre,  se = "cluster", cluster = ~COM, weights = ~value_estimated_population)
feolss4 <- feols(fml_75,    data = df_post, se = "cluster", cluster = ~COM, weights = ~value_estimated_population)

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
    make_coef_df(m_pre,  "1980\u20132003"),
    make_coef_df(m_post, "2004\u20132019")
  ) %>%
    mutate(Variable = recode(Variable, !!!temp_recode)) %>%
    bind_rows(
      data.frame(Variable = "15 to 20 (REF)", Coefficient = 0, Lower_CI = 0, Upper_CI = 0, Periode = "1980\u20132003"),
      data.frame(Variable = "15 to 20 (REF)", Coefficient = 0, Lower_CI = 0, Upper_CI = 0, Periode = "2004\u20132019")
    )
}

palette_periods <- c("1980\u20132003" = "#4E79A7", "2004\u20132019" = "#F28E2B")

df_plot1 <- build_plot_df(feolss1, feolss2) %>%
  mutate(Variable = factor(Variable, levels = order_levels))

df_plot2 <- build_plot_df(feolss3, feolss4) %>%
  mutate(Variable = factor(Variable, levels = order_levels))

fig_theme <- theme_minimal(base_size = 14) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title    = element_text(size = 12, face = "bold"))

fig1 <- ggplot(df_plot1, aes(x = Variable, y = Coefficient, fill = Periode, color = Periode)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Periode), alpha = 0.2, color = NA) +
  geom_line(aes(group = Periode), linetype = "dashed", linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  scale_color_manual(values = palette_periods) +
  scale_fill_manual(values  = palette_periods) +
  labs(x = "Temperature bins (\u00b0C)", y = "Mortality rate per 10,000",
       color = "Period", fill = "Period") +
  fig_theme

fig2 <- ggplot(df_plot2, aes(x = Variable, y = Coefficient, fill = Periode, color = Periode)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Periode), alpha = 0.2, color = NA) +
  geom_line(aes(group = Periode), linetype = "dashed", linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  scale_color_manual(values = palette_periods) +
  scale_fill_manual(values  = palette_periods) +
  labs(x = "Temperature bins (\u00b0C)", y = "Mortality rate per 10,000",
       color = "Period", fill = "Period") +
  fig_theme

ggsave("C:/Users/simon/Desktop/master_thesis/results/fig_mortalite_totale_pre_post.png",
       plot = fig1, width = 10, height = 6, dpi = 300)
ggsave("C:/Users/simon/Desktop/master_thesis/results/fig_mortalite_75plus_pre_post.png",
       plot = fig2, width = 10, height = 6, dpi = 300)

# ===========================================================================
# TABLE A — pre/post 2004 (LaTeX)
# ===========================================================================
etable(feolss1, feolss2, feolss3, feolss4,
       keep_raw = temp_bins,
       dict = c(tg_tbin_lt_m5  = "$<$ -5 \\textdegree C",
                tg_tbin_m5_0   = "-5 to 0 \\textdegree C",
                tg_tbin_0_5    = "0 to 5 \\textdegree C",
                tg_tbin_5_10   = "5 to 10 \\textdegree C",
                tg_tbin_10_15  = "10 to 15 \\textdegree C",
                tg_tbin_20_25  = "20 to 25 \\textdegree C",
                tg_tbin_25_30  = "25 to 30 \\textdegree C",
                tg_tbin_gt_30  = "$>$ 30 \\textdegree C"),
       depvar       = FALSE,
       drop.section = "fixef",
       extralines = list(
         "_Period"               = c("1980--2003", "2004--2019", "1980--2003", "2004--2019"),
         "Controls"              = "---",
         "Wind"                  = c("Yes", "Yes", "Yes", "Yes"),
         "Rain"                  = c("Yes", "Yes", "Yes", "Yes"),
         "Humidity"              = c("Yes", "Yes", "Yes", "Yes"),
         "Fixed-effects"         = "---",
         "Municipality by Month" = c("Yes", "Yes", "Yes", "Yes"),
         "Month by Year"         = c("Yes", "Yes", "Yes", "Yes"),
         "Department by Year"    = c("Yes", "Yes", "Yes", "Yes")
       ),
       notes = "OLS estimates. Clustered (Municipality) standard-errors in parentheses. Regressions weighted by population. .p$<$0.1; *p$<$0.05; **p$<$0.01; ***p$<$0.001.",
       tpt   = FALSE,
       tex   = TRUE,
       file  = "C:/Users/simon/Desktop/master_thesis/results/table_pre_post.tex")
