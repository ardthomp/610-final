**Forecast vs. Reality: CDC FluSight Performance in the 2024–2025 Flu Season**<br>
<br>
**Analysis, Figures, and Poster Materials for INFO 610 (Fall 2025)** <br>
**Author: Andrea Thompson**<br>
<br>
🦠 **Overview**<br>
<br>
This repository contains all data-processing scripts, analyses, and final figures used to evaluate how accurately the CDC FluSight ensemble model predicted U.S. influenza-associated hospitalizations during the 2024–2025 flu season.<br>
<br>
The project assesses 50% and 95% prediction interval (PI) coverage, state-level variation in forecast accuracy, and point forecast performance using linear regression. These results were presented in a scientific poster for INFO 610 (Fall 2025).<br>
<br>
🦠 **Repository Structure**<br>
```
cdc-flusight-2024-25-poster-analysis/
│
├── scripts/
│   └── analysis.R                # Complete R script used to generate figures and perform analyses
│
├── figures/
│   ├── figure1_map.png           # Choropleth map of state-level accuracy
│   ├── figure2_facets.png        # Accuracy by horizon, faceted by season week
│   ├── figure3_donut50.png       # Donut chart: 50% PI coverage
│   ├── figure3_donut95.png       # Donut chart: 95% PI coverage
│   └── figure4_regressiontable.png   # Liner regression summary table
│
├── poster/
│   └── poster.pdf                # Final academic poster
│
├── README.md                     # Documentation and project overview
└── .gitignore
```
<br>

🦠 **Data Sources**
<br>
Data come from the CDC Epidemic Prediction Initiative FluSight GitHub:<br>
<br>
• Weekly FluSight ensemble forecasts<br>
• Weekly influenza-associated hospital admissions<br>
• Quantile forecasts for 1-, 2-, and 3-week horizons<br>
<br>
CDC FluSight GitHub:<br>
https://github.com/cdcepi/FluSight-forecast-hub<br>
<br>
🦠 **Analytic Summary** <br>
<br>
Prediction Interval Coverage<br>
<br>
• Calculated whether observed hospitalizations fell within the 50% and 95% prediction intervals.<br>
• Summarized national mean coverage and created donut visualizations.<br>
<br>
State-Level Accuracy Variation<br>
<br>
• Standardized state-level accuracy using z-scores.<br>
• Grouped states into deviation categories based on SD bands.<br>
• Visualized results using U.S. choropleth maps.<br>
<br>
Point Forecast Accuracy<br>
<br>
• Fit linear regression: actual ~ median_forecast<br>
• Extracted slope, intercept, R², and residual error.<br>
• Produced a poster-friendly summary table.<br>
<br>
Representative Time-Series Panels<br>
<br>
Created visual comparisons for:<br>
• National<br>
• California<br>
• Texas<br>
• Vermont<br>

These illustrate differences in volume, seasonality, and variability across locations.<br>

🦠 **Required R Packages**<br>

gh<br>
purrr<br>
dplyr<br>
readr<br>
stringr<br>
ggplot2<br>
scales<br>
tidyr<br>
maps<br>
patchwork<br>
cowplot<br>
gt<br>
sysfonts<br>
showtext<br>
<br>
Install using:<br>
<br>
install.packages(c(
"gh","purrr","dplyr","readr","stringr","ggplot2", "scales","tidyr","maps","patchwork","cowplot","ggpubr","gt","sysfonts","showtext"
))<br>
<br>
🦠 **How to Reproduce the Analysis**<br>
<br>
1. Clone the repository:<br>
<br>
git clone https://github.com/YOURUSERNAME/cdc-flusight-2024-25-poster-analysis.git<br>
<br>
2. Open the project in RStudio.<br>
<br>
3. Run analysis.R<br>
<br>
🦠 **Poster**<br>
<br>
The final academic poster is located at:<br>
<br>
poster/poster.pdf<br>
<br>
🦠 **License**<br>
<br>
This project is licensed under the MIT License.<br>
CDC data remain the property of the CDC.<br>
<br>
🦠 **Acknowledgements**<br>
<br>
This README.md generated in conjunction with ChatGPT.
