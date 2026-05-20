library(data.table)
library(tidyverse)
library(haven)
library(readxl)

# ===========================================================================
# 1. BUILD NEW TEMPERATURE BINS FROM 13-BIN FILE
# ===========================================================================
bins13 <- fread("C:/Users/simon/Desktop/master_thesis/final_data/meteo_bins_13_1980_2019.csv")

bins13[, tg_new_tbin_lt_0  := tg_tbin_lt_m20 + tg_tbin_m20_m15 + tg_tbin_m15_m10 + tg_tbin_m10_m5 + tg_tbin_m5_0]
bins13[, tg_new_tbin_0_15  := tg_tbin_0_5 + tg_tbin_5_10 + tg_tbin_10_15]
bins13[, tg_new_tbin_15_20 := tg_tbin_15_20]
bins13[, tg_new_tbin_20_28 := tg_tbin_20_25 + tg_tbin_25_28]
bins13[, tg_new_tbin_gt_28 := tg_tbin_28_30 + tg_tbin_gt_30]

new_cols <- c("COM", "year", "month",
              "tg_new_tbin_lt_0", "tg_new_tbin_0_15", "tg_new_tbin_15_20",
              "tg_new_tbin_20_28", "tg_new_tbin_gt_28")

# ===========================================================================
# 2. MERGE BINS INTO REGRESSION BASE
# ===========================================================================
df_final_reg <- fread("C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet2.csv")

df_final_reg[, COM := as.character(COM)]
bins13[,       COM := as.character(COM)]

df_final_reg <- merge(df_final_reg, bins13[, ..new_cols],
                      by = c("COM", "year", "month"), all.x = TRUE)

# ===========================================================================
# 3. BUILD INCOME PANEL
# ===========================================================================
data_dir <- "C:/Users/simon/Desktop/master_thesis/income"

# 2001-2011: RFL (semicolon-delimited)
rfl_data <- map_dfr(1:11, function(i) {
  yr      <- 2000 + i
  yr2     <- str_pad(i, 2, pad = "0")
  varname <- paste0("RFUCQ2", yr2)

  read_csv2(
    file.path(data_dir, paste0("rf2", yr2, "com.csv")),
    col_types = cols(.default = "c"),
    locale    = locale(encoding = "latin1")
  ) %>%
    select(CODGEO = COM, median_uc = all_of(varname)) %>%
    mutate(
      CODGEO = str_pad(CODGEO, width = 5, side = "left", pad = "0"),
      year   = yr
    ) %>%
    filter(str_sub(CODGEO, 1, 2) >= "01" & str_sub(CODGEO, 1, 2) <= "95")
})

# 2012-2014: FILOSOFI .dta
filosofi_data_dta <- map_dfr(2012:2014, function(yr) {
  yr2     <- str_sub(yr, 3, 4)
  varname <- paste0("q2", yr2)

  read_dta(file.path(data_dir, paste0("distrib_rev_disp_", yr, ".dta"))) %>%
    zap_labels() %>%
    filter(niv == "COM") %>%
    select(CODGEO = codgeo, median_uc = all_of(varname)) %>%
    mutate(
      CODGEO = str_pad(as.character(CODGEO), width = 5, side = "left", pad = "0"),
      year   = yr
    ) %>%
    filter(str_sub(CODGEO, 1, 2) >= "01" & str_sub(CODGEO, 1, 2) <= "95")
})

# 2015-2019: FILOSOFI .xlsx
filosofi_data_xlsx <- map_dfr(2015:2019, function(yr) {
  yr2     <- str_sub(yr, 3, 4)
  varname <- paste0("ENSEMBLE_Q2", yr2)

  read_excel(file.path(data_dir, paste0("distrib_rev_disp_", yr, ".xlsx"))) %>%
    filter(NIV == "COM") %>%
    select(CODGEO, median_uc = all_of(varname)) %>%
    mutate(
      CODGEO = str_pad(as.character(CODGEO), width = 5, side = "left", pad = "0"),
      year   = yr
    ) %>%
    filter(str_sub(CODGEO, 1, 2) >= "01" & str_sub(CODGEO, 1, 2) <= "95")
})

# Combine income panel
panel <- bind_rows(
  rfl_data          %>% mutate(median_uc = as.numeric(median_uc)),
  filosofi_data_dta %>% mutate(median_uc = as.numeric(median_uc)),
  filosofi_data_xlsx %>% mutate(median_uc = as.numeric(median_uc))
) %>%
  arrange(CODGEO, year)

# ===========================================================================
# 4. MERGE INCOME INTO REGRESSION BASE
# ===========================================================================
panel_dt <- as.data.table(panel)
setnames(panel_dt, "CODGEO", "COM")
panel_dt[, COM  := str_pad(as.character(COM), width = 5, side = "left", pad = "0")]
panel_dt[, year := as.integer(year)]

df_final_reg[, COM := str_pad(as.character(COM), width = 5, side = "left", pad = "0")]

df_final_reg <- merge(df_final_reg, panel_dt[, .(COM, year, median_uc)],
                      by = c("COM", "year"), all.x = TRUE)

# ===========================================================================
# 5. EXPORT
# ===========================================================================
fwrite(df_final_reg,
       "C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet5.csv")

# ===========================================================================
# 6. DERIVED BINARY VARIABLES FOR SUBGROUP ANALYSIS
# ===========================================================================
df <- df_final_reg

# Standardized income
df[, median_uc_z := (median_uc - mean(median_uc, na.rm = TRUE)) / sd(median_uc, na.rm = TRUE)]

# Zero-out PNC variables before 2004 (plan launched that year)
df <- df %>%
  mutate(
    post_pnc = year >= 2004,
    across(
      c(pnc, dose_pnc, share_pnc, n_jours_pnc),
      ~ case_when(
        year < 2004              ~ 0,
        year >= 2004 & is.na(.) ~ 0,
        TRUE                    ~ .
      )
    )
  )

# Subgroup thresholds
med_rich   <- df %>% filter(year == 2004) %>%
                summarise(q = median(median_uc_z, na.rm = TRUE)) %>% pull(q)
seuil_dens <- 300

# Healthcare access indicator: bottom quartile of emergency access time in 2015
communes_soin <- df %>%
  filter(year == 2015) %>%
  mutate(soin = ifelse(acces_urgence <= quantile(acces_urgence, 0.25, na.rm = TRUE), 1L, 0L)) %>%
  distinct(COM, .keep_all = TRUE) %>%
  select(COM, soin)

# Build final subgroup indicators
df_reg <- df %>%
  left_join(communes_soin, by = "COM") %>%
  mutate(
    dens       = ifelse(densite > seuil_dens, 1L, 0L),
    rich       = ifelse(median_uc_z > med_rich, 1L, 0L),
    equip      = ifelse(niveau_equipements_services %in% c(3, 4), 1L, 0L),
    equip_clim = ifelse(taux_clim_RG_bin %in% c("3. Q3 : Moyen (3.2-6.8%)",
                                                 "4. Q4 : \u00c9lev\u00e9 (> 6.8%)"), 1L, 0L)
  )

# ===========================================================================
# 7. EXPORT
# ===========================================================================
fwrite(df_reg,
       "C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet6.csv")library(data.table)
library(tidyverse)
library(haven)
library(readxl)

# ===========================================================================
# 1. CRÉER LES NOUVEAUX BINS DEPUIS LE FICHIER 13 BINS
# ===========================================================================
bins13 <- fread("C:/Users/simon/Desktop/master_thesis/final_data/meteo_bins_13_1980_2019.csv")
bins13[, tg_new_tbin_lt_0   := tg_tbin_lt_m20 + tg_tbin_m20_m15 + tg_tbin_m15_m10 + tg_tbin_m10_m5 + tg_tbin_m5_0]
bins13[, tg_new_tbin_0_15   := tg_tbin_0_5 + tg_tbin_5_10 + tg_tbin_10_15]
bins13[, tg_new_tbin_15_20  := tg_tbin_15_20]
bins13[, tg_new_tbin_20_28  := tg_tbin_20_25 + tg_tbin_25_28]
bins13[, tg_new_tbin_gt_28  := tg_tbin_28_30 + tg_tbin_gt_30]
new_cols <- c("COM", "year", "month",
              "tg_new_tbin_lt_0", "tg_new_tbin_0_15", "tg_new_tbin_15_20",
              "tg_new_tbin_20_28", "tg_new_tbin_gt_28")

# ===========================================================================
# 2. FUSION AVEC df_final_reg_complet2
# ===========================================================================
df_final_reg <- fread("C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet2.csv")
df_final_reg[, COM := as.character(COM)]
bins13[, COM := as.character(COM)]
df_final_reg <- merge(df_final_reg, bins13[, ..new_cols],
                      by = c("COM", "year", "month"), all.x = TRUE)
cat("Lignes après fusion bins :", nrow(df_final_reg), "\n")
cat("NAs tg_new_tbin_gt_28 :", sum(is.na(df_final_reg$tg_new_tbin_gt_28)), "\n")

# ===========================================================================
# 3. CONSTRUCTION DU PANEL INCOME
# ===========================================================================
data_dir <- "C:/Users/simon/Desktop/master_thesis/income"

# ── 2001–2011 : RFL (délimiteur ;) ───────────────────────────────────────────

rfl_data <- map_dfr(1:11, function(i) {
  yr      <- 2000 + i
  yr2     <- str_pad(i, 2, pad = "0")
  varname <- paste0("RFUCQ2", yr2)
  
  read_csv2(
    file.path(data_dir, paste0("rf2", yr2, "com.csv")),
    col_types = cols(.default = "c"),
    locale    = locale(encoding = "latin1")
  ) |>
    select(CODGEO = COM, median_uc = all_of(varname)) |>
    mutate(
      CODGEO = str_pad(CODGEO, width = 5, side = "left", pad = "0"),
      year   = yr
    ) |>
    filter(str_sub(CODGEO, 1, 2) >= "01" & str_sub(CODGEO, 1, 2) <= "95")
})

# ── 2012–2014 : FILOSOFI .dta ─────────────────────────────────────────────────

filosofi_data_dta <- map_dfr(2012:2014, function(yr) {
  yr2     <- str_sub(yr, 3, 4)
  varname <- paste0("q2", yr2)
  
  read_dta(
    file.path(data_dir, paste0("distrib_rev_disp_", yr, ".dta"))
  ) |>
    zap_labels() |>
    filter(niv == "COM") |>
    select(CODGEO = codgeo, median_uc = all_of(varname)) |>
    mutate(
      CODGEO = str_pad(as.character(CODGEO), width = 5, side = "left", pad = "0"),
      year   = yr
    ) |>
    filter(str_sub(CODGEO, 1, 2) >= "01" & str_sub(CODGEO, 1, 2) <= "95")
})

# ── 2015–2019 : FILOSOFI .xlsx ────────────────────────────────────────────────

filosofi_data_xlsx <- map_dfr(2015:2019, function(yr) {
  yr2     <- str_sub(yr, 3, 4)
  varname <- paste0("ENSEMBLE_Q2", yr2)
  
  read_excel(
    file.path(data_dir, paste0("distrib_rev_disp_", yr, ".xlsx"))
  ) |>
    filter(NIV == "COM") |>
    select(CODGEO, median_uc = all_of(varname)) |>
    mutate(
      CODGEO = str_pad(as.character(CODGEO), width = 5, side = "left", pad = "0"),
      year   = yr
    ) |>
    filter(str_sub(CODGEO, 1, 2) >= "01" & str_sub(CODGEO, 1, 2) <= "95")
})

# ── Fusion panel ──────────────────────────────────────────────────────────────

panel <- bind_rows(
  rfl_data        |> mutate(median_uc = as.numeric(median_uc)),
  filosofi_data_dta |> mutate(median_uc = as.numeric(median_uc)),
  filosofi_data_xlsx |> mutate(median_uc = as.numeric(median_uc))
) |>
  arrange(CODGEO, year)

# ===========================================================================
# 4. FUSION AVEC df_final_reg
# ===========================================================================
panel_dt <- as.data.table(panel)
setnames(panel_dt, "CODGEO", "COM")
panel_dt[, COM  := str_pad(as.character(COM), width = 5, side = "left", pad = "0")]
panel_dt[, year := as.integer(year)]

df_final_reg[, COM := str_pad(as.character(COM), width = 5, side = "left", pad = "0")]

df_final_reg <- merge(df_final_reg, panel_dt[, .(COM, year, median_uc)],
                      by = c("COM", "year"), all.x = TRUE)
cat("NAs median_uc :", sum(is.na(df_final_reg$median_uc)), "\n")

# ===========================================================================
# 5. EXPORT
# ===========================================================================
fwrite(df_final_reg, "C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet5.csv")
cat("✓ df_final_reg_complet5.csv exporté\n")


# ===========================================================================
# 6. CREATION DE VARIABLE BINAIRE POUR SOUS GROUPE 
# ===========================================================================
#df <- fread("C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet5.csv")
df<- df_final_reg
#median income
df[, median_uc_z := (median_uc - mean(median_uc, na.rm = TRUE)) / sd(median_uc, na.rm = TRUE)]

#variable pnc a zero avec 2004
df <- df %>%
  mutate(
    # Variable temporelle : avant/après lancement PNC 2004
    post_pnc = year >= 2004
  )

df <- df %>%
  mutate(
    across(
      c(pnc, dose_pnc, share_pnc, n_jours_pnc),
      ~ case_when(
        year < 2004 ~ 0,
        year >= 2004 & is.na(.) ~ 0,
        TRUE ~ .
      ),
      .names = "{.col}"
    )
  )

#variable pnc a zero avec 2004
df <- df %>%
  mutate(
    # Variable temporelle : avant/après lancement PNC 2004
    post_pnc = year >= 2004
  )

df <- df %>%
  mutate(
    across(
      c(pnc, dose_pnc, share_pnc, n_jours_pnc),
      ~ case_when(
        year < 2004 ~ 0,
        year >= 2004 & is.na(.) ~ 0,
        TRUE ~ .
      ),
      .names = "{.col}"
    )
  )

#VARIABLES INDICATRICES
med_rich  <- df %>% filter(year == 2004) %>% summarise(q = median(median_uc_z, na.rm = TRUE)) %>% pull(q)
seuil_dens <- 300

communes_soin <- df %>%
  filter(year == 2015) %>%
  mutate(soin = ifelse(acces_urgence <= quantile(acces_urgence, 0.25, na.rm = TRUE), 1L, 0L)) %>%
  select(COM, soin) %>%
  distinct(COM, .keep_all = TRUE)

df_reg <- df %>%
  left_join(communes_soin, by = "COM") %>%
  mutate(
    dens       = ifelse(densite > seuil_dens, 1L, 0L),
    rich       = ifelse(median_uc_z > med_rich, 1L, 0L),
    equip      = ifelse(niveau_equipements_services %in% c(3, 4), 1L, 0L),
    equip_clim = ifelse(taux_clim_RG_bin %in% c("3. Q3 : Moyen (3.2-6.8%)", "4. Q4 : Élevé (> 6.8%)"), 1L, 0L)
  )
# ===========================================================================
# 7. EXPORT
# ===========================================================================
fwrite(df_reg, "C:/Users/simon/Desktop/master_thesis/final_data/df_final_reg_complet6.csv")
















