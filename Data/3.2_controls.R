library(data.table)

log_insee <- fread("C:/Users/simon/Desktop/master_thesis/insee_logement/base-cc-logement-2020_csv/base-cc-logement-2020.CSV")

vars_keep <- c(
  "CODGEO",
  "P09_RP", "P09_RP_PROP", "P09_RP_LOCHLMV",
  "P14_RP", "P14_RP_PROP", "P14_RP_LOCHLMV", "P14_RP_ACH05", "P14_RP_ACH11",
  "P20_RP", "P20_RP_PROP", "P20_RP_LOCHLMV", "P20_RP_ACH05", "P20_RP_ACH17"
)

log_insee_sel <- log_insee[, ..vars_keep]

# Reshape en long format pour les trois vagues de recensement
log_long <- rbindlist(list(
  log_insee_sel[, .(
    codgeo = CODGEO,
    year = 2009,
    n_logements = P09_RP,
    n_proprietaires = P09_RP_PROP,
    n_hlm = P09_RP_LOCHLMV,
    n_post_2000 = NA_real_,
    n_post_1990 = NA_real_
  )],
  log_insee_sel[, .(
    codgeo = CODGEO,
    year = 2014,
    n_logements = P14_RP,
    n_proprietaires = P14_RP_PROP,
    n_hlm = P14_RP_LOCHLMV,
    n_post_2000 = P14_RP_ACH11,
    n_post_1990 = P14_RP_ACH05 + P14_RP_ACH11
  )],
  log_insee_sel[, .(
    codgeo = CODGEO,
    year = 2020,
    n_logements = P20_RP,
    n_proprietaires = P20_RP_PROP,
    n_hlm = P20_RP_LOCHLMV,
    n_post_2000 = P20_RP_ACH17,
    n_post_1990 = P20_RP_ACH05 + P20_RP_ACH17
  )]
))

interpolate_panel <- function(dt) {
  # Squelette complet : toutes communes × années 2006-2020
  all_communes <- unique(dt$codgeo)
  all_years <- 2006:2020
  skeleton <- CJ(codgeo = all_communes, year = all_years)
  dt_full <- merge(skeleton, dt, by = c("codgeo", "year"), all.x = TRUE)
  
  vars_to_interpolate <- c("n_logements", "n_proprietaires", "n_hlm", 
                           "n_post_2000", "n_post_1990")
  
  # Interpolation linéaire + extrapolation arrière par commune
  dt_full[, (vars_to_interpolate) := lapply(.SD, function(x) {
    obs_years <- year[!is.na(x)]
    obs_values <- x[!is.na(x)]
    
    if (length(obs_years) >= 2) {
      # Calcule pente et intercept à partir des deux premiers points observés
      slope <- (obs_values[2] - obs_values[1]) / (obs_years[2] - obs_years[1])
      intercept <- obs_values[1] - slope * obs_years[1]
      
      # Applique la régression linéaire sur toutes les années manquantes
      ifelse(is.na(x), slope * year + intercept, x)
    } else {
      x  # Pas assez de points : garde les NA
    }
  }), by = codgeo, .SDcols = vars_to_interpolate]
  
  # Recalcule les pourcentages après interpolation des counts
  dt_full[, `:=`(
    pct_proprietaires = (n_proprietaires / n_logements) * 100,
    pct_hlm = (n_hlm / n_logements) * 100,
    pct_post_2000 = (n_post_2000 / n_logements) * 100,
    pct_post_1990 = (n_post_1990 / n_logements) * 100
  )]
  
  return(dt_full)
}

log_interpolated <- interpolate_panel(log_long)

# Vérifie la couverture temporelle
log_interpolated[, .(
  n_obs = .N,
  n_communes = uniqueN(codgeo),
  mean_logements = mean(n_logements, na.rm = TRUE),
  pct_na_logements = sum(is.na(n_logements)) / .N * 100
), by = year][order(year)]

fwrite(log_interpolated, 
       "C:/Users/simon/Desktop/master_thesis/final_data/panel_logement_insee_2006_2020_interpolated.csv")
