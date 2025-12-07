**Forecast vs. Reality: CDC FluSight Performance in the 2024–2025 Flu Season**

**Analysis, Figures, and Poster Materials for INFO 610 (Fall 2025)**
**Author: Andrea Thompson**

**Overview**

This repository contains all data-processing scripts, analyses, and final figures used to evaluate how accurately the CDC FluSight ensemble model predicted U.S. influenza-associated hospitalizations during the 2024–2025 flu season.

The project assesses 50% and 95% prediction interval (PI) coverage, state-level variation in forecast accuracy, and point forecast performance using linear regression. These results were presented in a scientific poster for INFO 610 (Fall 2025).

**Repository Structure**

cdc-flusight-2024-25-poster-analysis/
│
├── scripts/
│ ├── analysis.R (Complete R script used to generate figures and analyze data)
│
├── figures/
│ ├── figure1_map.png
│ ├── figure2_facets.png
│ ├── figure3_donut50.png
│ ├── figure3_donut95.png
│ └── figure4_regressiontable.png
│
├── poster/
│ └── poster.pdf
│
├── README.md
└── .gitignore

**Data Sources**

Data come from the CDC Epidemic Prediction Initiative FluSight GitHub:

• Weekly FluSight ensemble forecasts
• Weekly influenza-associated hospital admissions
• Quantile forecasts for 1-, 2-, and 3-week horizons

CDC FluSight GitHub:
https://github.com/cdcepi/FluSight-forecast-hub

**Analytic Summary**
Prediction Interval Coverage

• Calculated whether observed hospitalizations fell within the 50% and 95% prediction intervals.
• Summarized national mean coverage and created donut visualizations.

State-Level Accuracy Variation

• Standardized state-level accuracy using z-scores.
• Grouped states into deviation categories based on SD bands.
• Visualized results using U.S. choropleth maps.

Point Forecast Accuracy

• Fit linear regression:
actual ~ median_forecast
• Extracted slope, intercept, R², and residual error.
• Produced a poster-friendly summary table.

Representative Time-Series Panels

Created visual comparisons for:
• National
• California
• Texas
• Vermont

These illustrate differences in volume, seasonality, and variability across locations.

**Required R Packages**

gh
purrr
dplyr
readr
stringr
ggplot2
scales
tidyr
maps
patchwork
cowplot
gt
sysfonts
showtext

Install using:

install.packages(c(
"gh","purrr","dplyr","readr","stringr","ggplot2", "scales","tidyr","maps","patchwork","cowplot","ggpubr","gt","sysfonts","showtext"
))

**How to Reproduce the Analysis**

1. Clone the repository:

git clone https://github.com/YOURUSERNAME/cdc-flusight-2024-25-poster-analysis.git

2. Open the project in RStudio.

3. Run analysis.R

**Poster**

The final academic poster is located at:

poster/poster.pdf

**License**

This project is licensed under the MIT License.
CDC data remain the property of the CDC.

**Acknowledgements**

This README.md generated in conjunction with ChatGPT.
