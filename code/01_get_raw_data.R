# Download annual real GDP and population for all 50 U.S. states.
here::i_am("code/01_get_raw_data.R")

# Load the FRED API key.
key_file <- here::here(".Renviron")
readRenviron(key_file)

start_date <- as.Date("2000-01-01")
end_date   <- as.Date("2025-12-31")

# Retrieve annual FRED series for each state.
get_state_data <- function(suffix, value_column) {
  pieces <- lapply(seq_along(state.abb), function(i) {
    series_id <- paste0(state.abb[i], suffix)
    observations <- tryCatch(
      fredr::fredr(
        series_id = series_id,
        observation_start = start_date,
        observation_end = end_date,
        units = "lin"
      )
    )
    
    Sys.sleep(0.1)
    
    data.frame(
      state = state.name[i],
      state_abbr = state.abb[i],
      year = as.integer(format(observations$date, "%Y")),
      series_id = series_id,
      value = observations$value
    )
  })
  
  result <- do.call(rbind, pieces)
  names(result)[names(result) == "value"] <- value_column
  rownames(result) <- NULL
  result
}

# Download real GDP and population.
gdp <- get_state_data("RGSP", "real_gdp_millions_chained_2017_usd")
population <- get_state_data("POP", "population_thousands")

# Merge GDP and population data.
raw_data <- merge(
  gdp,
  population,
  by = c("state", "state_abbr", "year"),
  all = TRUE,
  suffixes = c("_gdp", "_population")
)

# Save the raw data.
raw_dir <- here::here("data", "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  raw_data,
  here::here("data", "raw", "state_rgdp_population_2000_2025.csv")
)