# 2019-2023 Correlation analysis of meteorological and influenza incidence rate in Shanghai

## Project Summary

This project investigates the correlation between weekly Influenza-Like Illness (ILI) rates and meteorological factors (such as temperature and relative humidity) in Shanghai from 2019 to 2023. By tracking historical trends, the study explores delayed meteorological effects on influenza transmission using statistical modeling to assist in public health forecasting.

---

## Project Description

### 1. Data Sources
* **Weather Data:** Historical meteorological observations (including temperature and relative humidity) retrieved via NOAA (National Oceanic and Atmospheric Administration) and ERA5 datasets.
* **Influenza Data:** Weekly ILI (Influenza-Like Illness) report metrics aggregated from regional surveillance updates.

### 2. Analysis Workflow & Scripts
* **01_data_processing.R:** Data cleaning, temporal alignment, and calculation of weekly rolling averages for meteorological metrics.
* **02_exploratory_analysis.R:** Descriptive statistics, trend visualizations, and preliminary correlation checks looking into lag effects.
* **03_statistical_models.R:** Statistical modeling to evaluate the delayed effects across the multi-year timeline.
* **04_figures.R:** Generating publication-quality plots and final trend visualizations.

### 3. Tech Stack
* **Language:** R Language
* **Key Packages:** `tidyverse`, `lubridate`, `readxl`, `worldmet`

---

## How to Run
1. Clone or download this repository to your local machine.
2. Ensure you have installed the required R libraries:
   ```R
   install.packages(c("tidyverse", "lubridate", "readxl", "worldmet"))
