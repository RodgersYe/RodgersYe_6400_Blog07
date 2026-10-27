# Download annual real GDP and population for all 50 U.S. states.
here::i_am("code/01_get_raw_data.R")

# Load the FRED API key.
key_file <- here::here(".Renviron")

if (file.exists(key_file)) {
  readRenviron(key_file)
}

if (!nzchar(Sys.getenv("FRED_API_KEY"))) {
  stop("FRED_API_KEY is missing. Add it to the project's .Renviron file.")
}

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
      ),
      error = function(e) {
        stop("FRED download failed for ", series_id, ": ", conditionMessage(e))
      }
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

if (nrow(gdp) != 50L * 26L || nrow(population) != 50L * 26L ||
    anyNA(gdp$real_gdp_millions_chained_2017_usd) ||
    anyNA(population$population_thousands)) {
  warning("Some state-year observations are missing. Check the CSV files.")
}

# Save the raw datasets.
raw_dir <- here::here("data", "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  gdp,
  here::here("data", "raw", "state_real_gdp_2000_2025.csv")
)

readr::write_csv(
  population,
  here::here("data", "raw", "state_population_2000_2025.csv")
)

message("Saved ", nrow(gdp), " GDP rows and ",
        nrow(population), " population rows to data/raw/.")
