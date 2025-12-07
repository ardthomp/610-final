# Code by Andrea Thompson for INFO 610 final project
## Note that some parts of this code were written in conjunction with AI

library(gh)
library(purrr)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(lubridate) # I do not explicitly call lubridate in my code
library(scales)
library(tidyr)
library(maps)
library(patchwork)
library(cowplot)
library(gt)

# Create dataset by importing the CDC's weekly FluSight summaries and joining with the actual weekly outcomes

## Importing the CDC's weekly FluSight summaries from their GitHub
owner <- "cdcepi"
repo  <- "FluSight-forecast-hub"
base_dir <- "weekly-summaries"
base_raw <- "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/main/weekly-summaries"

## Get all objects in the weekly-summaries folder
items <- gh("/repos/{owner}/{repo}/contents/{path}",
            owner = owner, repo = repo, path = base_dir, .limit = Inf)

### Filter directories that have dates in the name (so I can join all the weeks into one dataset of forecasts)
date_dirs <- items %>%
  keep(~ .x$type == "dir" && str_detect(.x$name, "^\\d{4}-\\d{2}-\\d{2}$")) %>%
  map_chr("name") %>%
  sort()

### Safe read
read_csv_safe <- function(path) {
  tryCatch(read_csv(path, show_col_types = FALSE), error = function(e) NULL)
}

### Cleaning the forecast data
#### ------- note that the forecast data has predictions for each US state/territory and a national forecast. Each of these predictions actually contains three forecasts (1, 2, and 3 week horizons). So my sample = (predictions for states/territories + national predictions)*3----------

## Import
forecast_urls <- paste0(base_raw, "/", date_dirs, "/", date_dirs, "_flu_forecasts_data.csv")

## Read + attach folder name
all_forecasts <- map2_dfr(
  forecast_urls, date_dirs,
  ~ {
    df <- read_csv_safe(.x)
    if (is.null(df)) return(NULL)
    df %>% mutate(week_folder = .y)
  }
)

all_forecasts <- all_forecasts %>%
  filter(tolower(model) == "flusight-ensemble") %>% # using only forecasts from the ensemble model, not other teams' submissions
  rename(week_ending_date = target_end_date) %>% # standardizing column names so that I can join it with outcome dataset later
  mutate(week_ending_date = as.Date(week_ending_date)) # making sure the values in this column are consistently formulated as dates

## Importing the actual weekly outcomes from the CDC's GitHub

actual_urls <- paste0(base_raw, "/", date_dirs, "/", date_dirs, "_flu_target_hospital_admissions_data.csv")

### Cleaning the true outcome data 

all_actuals <- map2_dfr(
  actual_urls, date_dirs,
  ~ {
    df <- read_csv_safe(.x)
    if (is.null(df)) return(NULL)
    df |> mutate(week_folder = .y)
  }
) %>%
  mutate(week_ending_date = as.Date(week_ending_date)) # ensuring this column name is consistent so I can use it to join the forecast dataset to the outcome dataset

actuals_one <- all_actuals %>%
  select(location_name, week_ending_date, value, week_folder) %>%
  arrange(location_name, week_ending_date, desc(week_folder)) %>%
  group_by(location_name, week_ending_date) %>%
  summarise(
    actual = value[which(!is.na(value))[1]],
    .groups = "drop"
  )

## Merging the forecasts with the weekly true outcomes and creating the correct_50 and correct_95 variables to indicate whether the true outcome was within the 50% PI and 95% PI, respectively

data <- all_forecasts %>%
  left_join(actuals_one, by = c("location_name", "week_ending_date")) %>%
  filter(
    !is.na(actual),
    !is.na(quantile_0.25), !is.na(quantile_0.75), # 50% PI = between the 25th and 75th quantiles
    !is.na(quantile_0.025), !is.na(quantile_0.975) # 95% PI = between the 2.5th and 97.5th quantiles
  ) %>%
  mutate(
    correct_50 = if_else(actual < quantile_0.25 | actual > quantile_0.75, 0L, 1L), # was the value within the 50% PI?
    correct_95 = if_else(actual < quantile_0.025 | actual > quantile_0.975, 0L, 1L) # was the value within the 95% PI?
  )

# ------------ Liner regression for the CDC's model --------------
overall_model <- lm(actual ~ quantile_0.5, data = data) # Using the median because it is the center of the PIs
summary(overall_model) # Generate summary table


# -----------------FIGURE 1: State Accuracy Map with Z-score Bins-------------------

## Finding accuracy rate for each state and calculating z-scores
state_accuracy <- state_accuracy %>%
  mutate(
    z50 = (avg_correct_50 - mean(avg_correct_50, na.rm = TRUE)) /
      sd(avg_correct_50, na.rm = TRUE),
    z95 = (avg_correct_95 - mean(avg_correct_95, na.rm = TRUE)) /
      sd(avg_correct_95, na.rm = TRUE)
  )

## Dividing the z-scores into bins

sd_cut_labels <- function(z) {
  cut(
    z,
    breaks = c(-Inf, -1, -0.5, 0.5, 1, Inf), # the z-score bins I thought made the most sense
    labels = c(
      "Much Lower Than Avg (z ≤ −1.0)", # abbreviations in the labels to make the legend narrower in my figure
      "Lower Than Avg (−1.0 < z ≤ −0.5)",
      "Near Avg (−0.5 < z ≤ +0.5)",
      "Higher Than Avg (+0.5 < z ≤ +1.0)",
      "Much Higher Than Avg (z ≥ +1.0)"
    ),
    include.lowest = TRUE
  )
}

## Putting the states into my z-score bins

state_accuracy <- state_accuracy %>% 
  mutate(
    sd_cat_50 = sd_cut_labels(z50),
    sd_cat_95 = sd_cut_labels(z95)
  )

## Create a table as the basis for my map

states_map <- map_data("state") # I want to map on the state level (in this case the continental US states + territories)

accuracy_long <- state_accuracy %>% # Organizing the states in their quintile bins in the table
  mutate(region = tolower(location_name)) %>%
  select(region, sd_cat_50, sd_cat_95) %>%
  pivot_longer(
    cols = starts_with("sd_cat_"),
    names_to = "PI_Type",
    values_to = "SD_Category"
  ) %>%
  mutate(
    PI_Type = recode(PI_Type,
                     "sd_cat_50" = "50% Prediction Interval",
                     "sd_cat_95" = "95% Prediction Interval")
  )

map_long <- states_map %>% 
  left_join(accuracy_long, by = "region")

## Custom color palette for my figure

bluegreen_palette <- c(
  "#CFFCFF",
  "#AAEFDF",
  "#9EE37D",
  "#63C132",
  "#358600"
)

names(bluegreen_palette) <- levels(accuracy_long$SD_Category) # Linking the colors to the bins

## Map formatting

base_geom <- list(
  geom_polygon(color = "white", linewidth = 0.25),
  coord_fixed(1.3),
  scale_fill_manual(values = bluegreen_palette,
                    name = "Accuracy Relative to National Mean\n(z-score bands)"),
  theme_void(base_size = 14),
  theme(strip.text = element_text(size = 18, face = "bold"))
)

## Function to generate maps using the z-score bins

plot_map <- function(type_label) {
  ggplot(filter(map_long, PI_Type == type_label),
         aes(long, lat, group = group, fill = SD_Category)) +
    base_geom
}

## Generating 50% and 95% PI maps specifically

p_map_50 <- plot_map("50% Prediction Interval")
p_map_95 <- plot_map("95% Prediction Interval")

## Making the maps share one legend so the resulting figure is cleaner

legend_shared <- cowplot::get_legend(
  p_map_50 +
    theme(legend.position = "right",
          legend.key.size = unit(0.8, "lines"))
)

legend_shared <- patchwork::wrap_elements(full = legend_shared)

### Deleting the old legends now that I have this new shared one

p50_clean <- p_map_50 + theme(legend.position = "none")
p95_clean <- p_map_95 + theme(legend.position = "none")

## Formatting the map's title block so that it will be more compact for my poster

map_title <- wrap_elements(
  full = grid::grobTree(
    grid::textGrob(
      "Deviation of State-Level Forecast Accuracy From the National Mean",
      x = 0.5, y = 0.60,
      gp = grid::gpar(fontsize = 22, fontface = "bold")
    ),
    grid::textGrob(
      "Z-Score Classification for 50% and 95% Prediction Interval Coverage",
      x = 0.5, y = 0.34,
      gp = grid::gpar(fontsize = 14)
    )
  )
)

## Combining maps + title + shared legend

layout <- "
AAAAAAA
BBBBCCC
BBBBCCC
"

final_map <- map_title +
  (p50_clean + p95_clean) +
  legend_shared +
  plot_layout(
    design  = layout,
    heights = c(0.22, 0.78, 0.78),
    widths  = c(1, 1, 0.55)
  )

## Print figure
final_map

# ----------------FIGURE 2: Representative States with 50% and 95% PI Ribbons------------------

## Choosing my representative states and defining each state's 50% and 95% PI
rep_states <- data %>%
  mutate(location_name = ifelse(location_name == "US", "National", location_name)) %>%
  filter(location_name %in% c("National", "California", "Texas", "Vermont")) %>%
  group_by(location_name, week_ending_date) %>%
  summarise(
    actual = mean(actual),
    median_forecast = mean(quantile_0.5),
    lower_50 = mean(quantile_0.25),
    upper_50 = mean(quantile_0.75),
    lower_95 = mean(quantile_0.025),
    upper_95 = mean(quantile_0.975),
    .groups = "drop"
  ) %>%
  mutate(location_name = factor(location_name,
                                levels = c("National", "California", "Texas", "Vermont")))

p_facets <- ggplot(rep_states, aes(x = week_ending_date)) +
  
## PI ribbons
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = "95% PI"), alpha = 0.6) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50, fill = "50% PI"), alpha = 0.7) +
  
## Median (center of the PIs) and actual outcome lines
  geom_line(aes(y = actual, color = "Actual"), size = 1.2) +
  geom_line(aes(y = median_forecast, color = "Median Forecast"),
            size = 1.2, linetype = "dashed") +
  
## Facets
  facet_wrap(~ location_name, scales = "free_y", ncol = 1) +
  
## Choosing consistent color palette
  scale_color_manual(values = c(
    "Actual" = "#744b88",
    "Median Forecast" = "#358600"
  )) +

  scale_fill_manual(values = c(
    "50% PI" = "#8ecc70",
    "95% PI" = "#CEF1BE"
  )) +
  
## Defining the x-axis (time range) for the figures--otherwise it includes all of summer 2025, which is not part of flu season
  scale_x_date(
    limits = c(as.Date("2024-12-01"), as.Date("2025-06-30")),
    breaks = seq(as.Date("2024-12-01"), as.Date("2025-06-01"), by = "1 month"),
    labels = scales::date_format("%b %Y"),
    expand = c(0, 0)
  ) +
  
## Labels for the figure
  labs(
    title = "Forecast vs Actual Hospital Admissions",
    subtitle = "50% and 95% Prediction Intervals for Selected Locations",
    x = "Month",
    y = "Hospital Admissions",
    color = "",
    fill  = ""
  ) +
  
## Clean theme
theme_minimal(base_size = 16) +
  theme(
    ### Legend
    legend.position = "top",
    legend.margin = margin(b = 10),
    
    #### Removing grey background
    strip.background = element_blank(),
    strip.text = element_text(size = 16, face = "bold"),
    
    ### Setting gridlines
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey92"),
    
    ### x-axis text
    axis.text.x = element_text(angle = 45, hjust = 1),
    
    ### Title subtitle
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  )

## Print figure
p_facets

# --------------------------FIGURE 3: Donut Charts ------------------------------------------------------------

## Generating overall means for the center of the donuts
acc_50 <- mean(data$correct_50) * 100
acc_95 <- mean(data$correct_95) * 100

## Function to create donut chart that reflects PI coverage
donut <- function(value, title, fill_color) {
  df <- data.frame(
    category = c("Correct", "Incorrect"),
    value = c(value, 100 - value)
  )

## Donut formatting for poster
  ggplot(df, aes(x = 2, y = value, fill = category)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    scale_fill_manual(values = c("Correct" = fill_color, "Incorrect" = "gray85")) +
    xlim(0.5, 2.5) +
    theme_void() +
    annotate("text", x = 0.5, y = 0,
             label = paste0(round(value, 1), "%"),
             size = 9, fontface = "bold") +
    labs(title = title) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "none"
    )
}

## Create individual donut charts
p_donut_50 <- donut(acc_50, "50% Prediction Interval", "#22577a") # Titles for each donut
p_donut_95 <- donut(acc_95, "95% Prediction Interval", "#358600")

## Combine side-by-side
p_donuts <- p_donut_50 + p_donut_95 +
  plot_annotation(
    title = "Prediction Interval Coverage", # Big title for the whole figure
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  )

## Print figure
p_donuts

