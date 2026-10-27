
# Calculate state-level GDP, population, and per capita growth.
here::i_am("code/03_analyze_data.R")

library(dplyr)
library(readr)

raw_dir <- here::here("data", "raw")
out_dir <- here::here("data", "process")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Load raw GDP and population data.
gdp <- read_csv(
  file.path(raw_dir, "state_real_gdp_2000_2025.csv"),
  show_col_types = FALSE
)

pop <- read_csv(
  file.path(raw_dir, "state_population_2000_2025.csv"),
  show_col_types = FALSE
)

# 2. Validate state-year coverage and data values.
expected <- expand.grid(
  state_abbr = state.abb,
  year = 2000:2025
)

check_data <- function(df, value_col, suffix) {
  required <- c("state", "state_abbr", "year",
                "series_id", value_col)
  
  if (!all(required %in% names(df))) {
    stop("Required columns are missing.")
  }
  
  if (anyNA(df[required])) {
    stop("Missing values detected.")
  }
  
  if (!is.numeric(df[[value_col]]) ||
      any(!is.finite(df[[value_col]]) | df[[value_col]] <= 0)) {
    stop("Invalid numeric values detected.")
  }
  
  if (anyDuplicated(df[c("state_abbr", "year")])) {
    stop("Duplicate state-year observations detected.")
  }
  
  if (nrow(anti_join(expected, df,
                     by = c("state_abbr", "year"))) > 0 ||
      nrow(anti_join(df, expected,
                     by = c("state_abbr", "year"))) > 0) {
    stop("State-year coverage is incomplete or contains extra rows.")
  }
  
  correct_names <- state.name[match(df$state_abbr, state.abb)]
  
  if (any(df$state != correct_names) ||
      any(df$series_id != paste0(df$state_abbr, suffix))) {
    stop("State names or FRED series IDs are incorrect.")
  }
}

check_data(
  gdp,
  "real_gdp_millions_chained_2017_usd",
  "RGSP"
)

check_data(
  pop,
  "population_thousands",
  "POP"
)

# 3. Merge datasets and calculate annual growth rates.
results <- gdp %>%
  select(
    state, state_abbr, year,
    real_gdp_millions_chained_2017_usd
  ) %>%
  inner_join(
    pop %>%
      select(state, state_abbr, year, population_thousands),
    by = c("state", "state_abbr", "year")
  ) %>%
  arrange(state_abbr, year) %>%
  group_by(state_abbr) %>%
  mutate(
    real_gdp_per_capita_chained_2017_usd =
      1000 * real_gdp_millions_chained_2017_usd /
      population_thousands,
    
    real_gdp_growth_pct =
      100 * (
        real_gdp_millions_chained_2017_usd /
          lag(real_gdp_millions_chained_2017_usd) - 1
      ),
    
    population_growth_pct =
      100 * (
        population_thousands /
          lag(population_thousands) - 1
      ),
    
    real_gdp_per_capita_growth_pct =
      100 * (
        real_gdp_per_capita_chained_2017_usd /
          lag(real_gdp_per_capita_chained_2017_usd) - 1
      )
  ) %>%
  ungroup()

# 4. Calculate each component's share of GDP growth.
results <- results %>%
  mutate(
    population_share_of_gdp_growth_pct = if_else(
      !is.na(real_gdp_growth_pct) &
        abs(real_gdp_growth_pct) > 1e-10,
      100 * population_growth_pct / real_gdp_growth_pct,
      NA_real_
    ),
    
    per_capita_share_of_gdp_growth_pct = if_else(
      !is.na(real_gdp_growth_pct) &
        abs(real_gdp_growth_pct) > 1e-10,
      100 * real_gdp_per_capita_growth_pct / real_gdp_growth_pct,
      NA_real_
    )
  )

# 5. Verify approximate and exact GDP growth identities.
results <- results %>%
  mutate(
    approximate_gdp_growth_pct =
      population_growth_pct + real_gdp_per_capita_growth_pct,
    
    approximation_error_pp =
      real_gdp_growth_pct - approximate_gdp_growth_pct,
    
    interaction_term_pp =
      population_growth_pct *
      real_gdp_per_capita_growth_pct / 100,
    
    exact_gdp_growth_pct =
      approximate_gdp_growth_pct + interaction_term_pp,
    
    exact_error_pp =
      real_gdp_growth_pct - exact_gdp_growth_pct,
    
    exact_identity_verified = if_else(
      is.na(exact_error_pp),
      NA,
      abs(exact_error_pp) < 1e-8
    )
  )

# 6. Check the exact identity and save all results.
if (any(results$exact_identity_verified %in% FALSE)) {
  warning("Some observations failed the exact GDP growth identity.")
}

write_csv(
  results,
  file.path(out_dir, "Analysis_Results.csv")
)

# Remove the obsolete verification CSV from the previous version.
old_file <- file.path(
  out_dir, "GDP_Growth_Identity_Verification.csv"
)

if (file.exists(old_file)) {
  file.remove(old_file)
}

message("Analysis completed successfully.")
message("Saved ", nrow(results), " rows to data/process/Analysis_Results.csv")
