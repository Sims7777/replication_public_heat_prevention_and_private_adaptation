# Replication Package

This repository contains the code and data documentation to replicate the results of Labracherie (2026). The paper estimates the causal effect of France's Plan National Canicule (PNC) on heat-related mortality using a municipal-level panel of 34,000 French communes over 1980–2019.

## Structure
```
.
├── Data/                
    ├── Data_preparation/ 
        └── R scripts — run in order: 1.1 → 1.2 → 2.1 → ... → 6.1
    ├── Download/         # Raw data and extraction scripts
        └── data_meteo
        └── data_pnc
└── Results/              # Figures and regressions 
    ├── mortality/        # Heat-related mortality
    ├── AC/               # Mortality × air conditioning
    ├── pnc/              # Mortality × AC × heat-warning plan
    ├── robustness/       # Mortality × heat-warning plan by building vintage and historical heat exposition
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
| Building vintage | https://www.insee.fr/fr/statistiques/8581994 |

The following datasets were obtained via a data access request on [PROGEDO](https://www.progedo.fr):

- Base permanente des équipements (INSEE)
- Enquête logement (INSEE)
- Recensement de la population (INSEE)
- FILOSOFI (INSEE)
- RFLM (INSEE)

## Replication

Run the scripts in `Data_preparation/` sequentially (1.1 → 1.2 → 2.1 → ... → 6.1), then run the scripts in each `Results/` subfolder.

## AI Assistance

During the preparation of this replication package, I used ChatGPT to assist with code organisation and cleaning (restructuring scripts, standardising naming conventions, and improving readability). All code was reviewed and validated by the author. The analytical content and results remain the sole responsibility of the author.


