# ===========================================================================
# POPULATION_HARMONIZATION.R
# Extract and harmonize INSEE population data
# ===========================================================================

library(data.table)
library(dplyr)
library(readxl)
library(tidyr)

# ===========================================================================
# PATHS
# ===========================================================================
BASE_THESIS  <- "C:/Users/simon/Desktop/master_thesis"
BASE_POP     <- file.path(BASE_THESIS, "population_xl")
BASE_DECES   <- file.path(BASE_THESIS, "deces")
BASE_POP_OUT <- file.path(BASE_THESIS, "population_harmonized")

POP_FILE           <- file.path(BASE_POP,   "pop-sexe-age-quinquennal6822.xlsx")
TABLE_PASSAGE_FILE <- file.path(BASE_DECES, "passage-geo-2022.xlsx")

dir.create(BASE_POP_OUT, recursive = TRUE, showWarnings = FALSE)

# ===========================================================================
# CROSSWALK TABLE
# ===========================================================================
table_passage <- read_excel(TABLE_PASSAGE_FILE, sheet = "PASSAGE_GEO_2022")
names(table_passage)[1] <- "COM_AV"
names(table_passage)[2] <- "COM_AP"

table_passage_simple <- table_passage %>%
  dplyr::select(COM_AV, COM_AP) %>%
  dplyr::mutate(COM_AV = as.character(COM_AV),
                COM_AP = as.character(COM_AP)) %>%
  dplyr::filter(!is.na(COM_AV) & !is.na(COM_AP))

# ===========================================================================
# EXTRACTION FUNCTION
# ===========================================================================
extract_and_harmonize <- function(sheet_name, annee) {

  pop_raw <- read_excel(POP_FILE, sheet = sheet_name, skip = 13)
  names(pop_raw)[1:6] <- c("RR", "DR", "CR", "STABLE", "DR24", "LIBELLE")
  demo_cols <- names(pop_raw)[7:ncol(pop_raw)]

  pop <- pop_raw %>%
    dplyr::filter(!is.na(CR) & !is.na(DR)) %>%
    dplyr::mutate(
      DR  = sprintf("%02d", as.integer(DR)),
      CR  = sprintf("%03d", as.integer(CR)),
      COM = paste0(DR, CR)
    ) %>%
    # Exclude overseas territories and Corsica
    dplyr::filter(!grepl("^(97|98|99|2[AB])", COM)) %>%
    dplyr::select(COM, dplyr::all_of(demo_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(demo_cols), ~as.numeric(.)))

  # Apply geographic crosswalk and aggregate to 2022 boundaries
  pop_harmonized <- pop %>%
    dplyr::left_join(table_passage_simple, by = c("COM" = "COM_AV")) %>%
    dplyr::mutate(COM_GEO2022 = ifelse(!is.na(COM_AP), COM_AP, COM)) %>%
    dplyr::select(-COM, -COM_AP) %>%
    dplyr::group_by(COM_GEO2022) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(demo_cols), ~sum(., na.rm = TRUE)),
                     .groups = "drop") %>%
    dplyr::rename(COM = COM_GEO2022) %>%
    # Defensive post-harmonization filter
    dplyr::filter(!grepl("^2[AB]", COM))

  # Corsica check
  n_corse <- sum(grepl("^2[AB]", pop_harmonized$COM))
  if (n_corse > 0) {
    warning(sprintf("[%d] %d Corsican communes still present after harmonization.", annee, n_corse))
  }

  pop_long <- pop_harmonized %>%
    tidyr::pivot_longer(cols = -COM, names_to = "variable", values_to = "POPULATION") %>%
    dplyr::mutate(
      age_code  = as.integer(sub(".*ageq_rec(\\d+)s.*",  "\\1", variable)),
      sexe_code = as.integer(sub(".*s(\\d+)rpop.*",      "\\1", variable)),
      AGE_QUINQUENNAL = dplyr::case_when(
        age_code == 1  ~ "0-4",   age_code == 2  ~ "5-9",
        age_code == 3  ~ "10-14", age_code == 4  ~ "15-19",
        age_code == 5  ~ "20-24", age_code == 6  ~ "25-29",
        age_code == 7  ~ "30-34", age_code == 8  ~ "35-39",
        age_code == 9  ~ "40-44", age_code == 10 ~ "45-49",
        age_code == 11 ~ "50-54", age_code == 12 ~ "55-59",
        age_code == 13 ~ "60-64", age_code == 14 ~ "65-69",
        age_code == 15 ~ "70-74", age_code == 16 ~ "75-79",
        age_code == 17 ~ "80-84", age_code == 18 ~ "85-89",
        age_code == 19 ~ "90-94", age_code == 20 ~ "95+",
        TRUE           ~ NA_character_
      ),
      SEXE = dplyr::case_when(
        sexe_code == 1 ~ "Homme",
        sexe_code == 2 ~ "Femme",
        TRUE           ~ NA_character_
      )
    ) %>%
    dplyr::select(COM, AGE_QUINQUENNAL, SEXE, POPULATION) %>%
    dplyr::mutate(YEAR = annee)

  return(pop_long)
}

# ===========================================================================
# EXTRACTION BY YEAR
# ===========================================================================
sheets_dispo <- excel_sheets(POP_FILE)

annees_config <- list(
  list(sheet = "COM_1982", annee = 1982),
  list(sheet = "COM_1990", annee = 1990),
  list(sheet = "COM_1999", annee = 1999),
  list(sheet = "COM_2006", annee = 2006),
  list(sheet = "COM_2011", annee = 2011),
  list(sheet = "COM_2016", annee = 2016),
  list(sheet = "COM_2022", annee = 2022)
)

pop_list <- list()
for (i in seq_along(annees_config)) {
  config <- annees_config[[i]]
  if (!config$sheet %in% sheets_dispo) next
  pop_list[[length(pop_list) + 1]] <- extract_and_harmonize(config$sheet, config$annee)
}

# ===========================================================================
# BIND AND VALIDATE
# ===========================================================================
pop_final <- dplyr::bind_rows(pop_list) %>%
  dplyr::select(YEAR, COM, AGE_QUINQUENNAL, SEXE, POPULATION) %>%
  dplyr::arrange(YEAR, COM, AGE_QUINQUENNAL, SEXE)

n_corse_final <- length(unique(pop_final$COM[grepl("^2[AB]", pop_final$COM)]))
if (n_corse_final > 0) {
  warning(sprintf("%d Corsican communes in pop_final.", n_corse_final))
}

# ===========================================================================
# AGGREGATIONS + EXPORT (census years)
# ===========================================================================
pop_sexe_total <- pop_final %>%
  dplyr::group_by(YEAR, COM, SEXE) %>%
  dplyr::summarise(POPULATION = sum(POPULATION, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(AGE_QUINQUENNAL = "TOTAL")

pop_total <- pop_final %>%
  dplyr::group_by(YEAR, COM) %>%
  dplyr::summarise(POPULATION = sum(POPULATION, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(AGE_QUINQUENNAL = "TOTAL", SEXE = "TOTAL")

pop_complete <- dplyr::bind_rows(pop_final, pop_sexe_total, pop_total) %>%
  dplyr::arrange(YEAR, COM, AGE_QUINQUENNAL, SEXE)

fwrite(as.data.table(pop_complete),
       file.path(BASE_POP_OUT, "population_complete_1980_2022_geo2022.csv"))

# ===========================================================================
# PIVOT WIDE FOR INTERPOLATION
# ===========================================================================
pop_detail <- pop_final %>%
  dplyr::filter(!is.na(AGE_QUINQUENNAL) & !is.na(SEXE))

pop_wide <- pop_detail %>%
  tidyr::pivot_wider(
    id_cols      = c(COM, AGE_QUINQUENNAL, SEXE),
    names_from   = YEAR,
    names_prefix = "POP_",
    values_from  = POPULATION
  )

# ===========================================================================
# LINEAR INTERPOLATION BETWEEN CENSUS YEARS
# ===========================================================================
annees_extraites <- sort(unique(pop_final$YEAR))

periodes <- list()
for (i in 1:(length(annees_extraites) - 1)) {
  avant <- annees_extraites[i]
  apres <- annees_extraites[i + 1]
  annees_interp <- (avant + 1):(apres - 1)
  if (length(annees_interp) > 0) {
    periodes[[length(periodes) + 1]] <- list(
      avant  = avant,
      apres  = apres,
      annees = annees_interp
    )
  }
}

for (periode in periodes) {
  col_avant <- paste0("POP_", periode$avant)
  col_apres <- paste0("POP_", periode$apres)
  n_ans     <- periode$apres - periode$avant

  for (annee in periode$annees) {
    w <- (annee - periode$avant) / n_ans
    pop_wide[[paste0("POP_", annee)]] <-
      (1 - w) * pop_wide[[col_avant]] + w * pop_wide[[col_apres]]
  }
}

# Constant backcast for years before first census
premier_rec <- min(annees_extraites)
annees_avant_premier <- 1980:(premier_rec - 1)

if (length(annees_avant_premier) > 0) {
  col_premier <- paste0("POP_", premier_rec)
  for (annee in annees_avant_premier) {
    pop_wide[[paste0("POP_", annee)]] <- pop_wide[[col_premier]]
  }
}

# ===========================================================================
# PIVOT LONG + FINAL AGGREGATIONS
# ===========================================================================
pop_interpolee <- pop_wide %>%
  tidyr::pivot_longer(
    cols         = tidyr::starts_with("POP_"),
    names_to     = "YEAR",
    names_prefix = "POP_",
    values_to    = "POPULATION"
  ) %>%
  dplyr::mutate(YEAR = as.integer(YEAR), POPULATION = round(POPULATION, 0)) %>%
  dplyr::filter(YEAR >= 1980 & YEAR <= 2022) %>%
  dplyr::arrange(YEAR, COM, AGE_QUINQUENNAL, SEXE)

n_corse_interp <- length(unique(pop_interpolee$COM[grepl("^2[AB]", pop_interpolee$COM)]))
if (n_corse_interp > 0) {
  warning(sprintf("%d Corsican communes in interpolated data.", n_corse_interp))
}

pop_sexe_total_i <- pop_interpolee %>%
  dplyr::group_by(YEAR, COM, SEXE) %>%
  dplyr::summarise(POPULATION = sum(POPULATION, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(AGE_QUINQUENNAL = "TOTAL")

pop_total_i <- pop_interpolee %>%
  dplyr::group_by(YEAR, COM) %>%
  dplyr::summarise(POPULATION = sum(POPULATION, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(AGE_QUINQUENNAL = "TOTAL", SEXE = "TOTAL")

pop_complete_interp <- dplyr::bind_rows(pop_interpolee, pop_sexe_total_i, pop_total_i) %>%
  dplyr::arrange(YEAR, COM, AGE_QUINQUENNAL, SEXE)

# ===========================================================================
# EXPORT
# ===========================================================================
output_file <- file.path(BASE_POP_OUT, "population_interpolee_1980_2022.csv")
fwrite(as.data.table(pop_complete_interp), output_file)
