library(dplyr)
library(readr)

# Define file paths
raw_file <- here::here("data", "raw", "state_rgdp_population_2000_2025.csv")
output_dir <- "processed_data"
output_dir <- here::here("data", "process")
output_file <- file.path(output_dir, "state_rgdp_population_2000_2025_processed.csv")

# Create processed_data folder if it does not exist
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Load raw data
raw_data <- read_csv(raw_file, show_col_types = FALSE)

# Calculate economic indicators
processed_data <- raw_data %>%
  
  # Select and rename required columns
  transmute(state_abbr, year, real_gdp_millions=real_gdp_millions_chained_2017_usd,population_thousands) %>%
  
  # Sort by state and year
  arrange(state_abbr, year) %>%
  
  # Calculate growth rates within each state
  group_by(state_abbr) %>%
  
  mutate(
    real_gdp_per_capita =
      real_gdp_millions / population_thousands * 1000,
    real_gdp_growth_rate =
      real_gdp_millions / lag(real_gdp_millions) - 1,
    population_growth_rate =
      population_thousands / lag(population_thousands) - 1,
    real_gdp_per_capita_growth_rate =
      real_gdp_per_capita / lag(real_gdp_per_capita) - 1,
    population_growth_rate_share_of_real_gdp_growth_rate =
      if_else(
        !is.na(real_gdp_growth_rate) &
          real_gdp_growth_rate != 0,
        population_growth_rate / real_gdp_growth_rate,
        NA_real_
      ),
    real_gdp_per_capita_growth_rate_share_of_real_gdp_growth_rate =
      if_else(
        !is.na(real_gdp_growth_rate) &
          real_gdp_growth_rate != 0,
        real_gdp_per_capita_growth_rate /
          real_gdp_growth_rate,
        NA_real_
      )
  ) %>%
  ungroup()

# Save processed data
write_csv(processed_data, output_file, na = "")
