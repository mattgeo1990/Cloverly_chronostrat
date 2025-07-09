# Cloverly Chronostratigraphy

This repository contains data, code, and results related to the chronostratigraphic and chemostratigraphic analysis of the mid-Cretaceous Cloverly Formation in the Western Interior Basin.

## Repository Structure

Cloverly_chronostrat/
├── data/
│   ├── processed/         # Cleaned and interpolated datasets
│   └── raw/               # Raw input data (e.g., isotope measurements, age models)
├── results/
│   ├── figures/           # Output plots for chemostratigraphy and age-depth models
│   └── tables/            # (Optional) Tables for manuscript or supplementary material
├── scripts/               # R scripts for data processing and figure generation
└── README.md              # Project overview and usage instructions

## Project Overview

This project integrates:
- Detrital zircon U-Pb geochronology
- Stable carbon isotope chemostratigraphy (δ¹³C_org)
- Age-depth modeling
- Sequence stratigraphic context

The primary goal is to improve geochronologic constraints and correlate depositional environments across key sections of the Cloverly Formation.

## Key Scripts

- `scripts/Cloverly_chemostrat_plots.R`: Generates chemostratigraphic profiles by depth and age, including rolling averages and gap-aware plotting.
- `scripts/midK_chemostrat_plots.R`: Comparative C-isotope plots of Cloverly data and global marine reference records.
- `scripts/age_model_interpolation.R`: Applies age-depth model to field-measured sections.

## Data Notes

- Raw datasets are stored in `data/raw/`, including:
  - δ¹³C_org isotope data (`*_RAW.csv`)
  - Depth-only metadata (`*_DEPTHS_ONLY.csv`)
  - Age models (`*_age_model.csv`)
- Processed datasets (with rolling averages and age assignments) are stored in `data/processed/`.

## Dependencies

The R scripts rely on the following packages:
- `tidyverse`
- `zoo`
- `patchwork`
- `here`

You can install them with:

```r
install.packages(c("tidyverse", "zoo", "patchwork", "here"))
