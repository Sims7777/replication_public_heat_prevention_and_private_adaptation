This repository contains the code and data documentation to replicate the results of Labracherie (2026). The paper estimates the causal effect of France's Plan National Canicule (PNC) on heat-related mortality using a municipal-level panel of 34,000 French communes over 1980–2019.


# Replication Package

## Structure
```
.
├── Data_preparation/     # R scripts — run in order: 1.1 → 1.2 → 2.1 → ... → 6.1
├── Download/             # Raw data and extraction scripts
└── Results/
    ├── mortality/        # Figures and regressions — heat-related mortality
    ├── AC/               # Figures and regressions — mortality × air conditioning
    └── pnc/              # Figures and regressions — mortality × AC × heat-warning plan
```
## Data

### Download folder

Two subfolders:

**data_meteo** — meteorological variables extracted via Python. Runtime is approximately 7 days on a 16 GB machine.

**data_pnc** — heat-warning bulletins from Météo France, scraped from `http://vigilance-public.meteo.fr/`

> Paths in `Download/` are Mac-formatted. Paths in `Data_preparation/` are Windows-formatted. Update both sets of paths to match your environment before running.

### Data sources

| Dataset | Source |
|---|---|
| Emergency room travel times | https://www.data.gouv.fr/datasets/diagnostic-dacces-aux-soins-urgents |
| Death records (from 1970) | https://www.data.gouv.fr/datasets/fichier-des-personnes-decedees |
| Commune density | https://www.data.gouv.fr/datasets/communes-et-villes-de-france-en-csv-excel-json-parquet-et-feather |

The following datasets were obtained via a data access request on [PROGEDO](https://www.progedo.fr):

- Base permanente des équipements (INSEE)
- Enquête logement (INSEE)
- Recensement de la population (INSEE)
- FILOSOFI (INSEE)
- RFLM (INSEE)

## Replication

Run the scripts in `Data_preparation/` sequentially (1.1 → 1.2 → 2.1 → ... → 6.1), then run the scripts in each `Results/` subfolder.
