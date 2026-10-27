# Check completeness and validity of the downloaded state data.
here::i_am("code/02_clean_data.R")

years <- 2000:2025
expected <- expand.grid(state_abbr = state.abb, year = years,
                        stringsAsFactors = FALSE)
output_dir <- here::here("data", "process")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Validate state-year coverage, identifiers, and numeric values.
check_data <- function(filename, value_col, suffix, dataset) {
  x <- readr::read_csv(
    here::here("data", "raw", filename),
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
  required <- c("state", "state_abbr", "year", "series_id", value_col)
  if (!all(required %in% names(x))) {
    stop("Missing columns in ", filename, ": ",
         paste(setdiff(required, names(x)), collapse = ", "))
  }

  year_text <- x$year
  x$year <- suppressWarnings(as.integer(year_text))
  x[[value_col]] <- suppressWarnings(as.numeric(x[[value_col]]))

  valid_state <- x$state_abbr %in% state.abb
  valid_year <- !is.na(year_text) & grepl("^[0-9]{4}$", year_text) &
    x$year %in% years
  valid_key <- valid_state & valid_year
  key <- paste(x$state_abbr, x$year, sep = ":")
  expected_key <- paste(expected$state_abbr, expected$year, sep = ":")

  missing <- expected[!expected_key %in% key[valid_key], ]
  extra <- x[!valid_key, ]
  duplicates <- x[valid_key & (duplicated(key) | duplicated(key, fromLast = TRUE)), ]
  invalid_value <- is.na(x[[value_col]]) | !is.finite(x[[value_col]]) |
    x[[value_col]] <= 0
  wrong_name <- !valid_state | is.na(x$state) |
    x$state != state.name[match(x$state_abbr, state.abb)]
  wrong_series <- is.na(x$series_id) |
    x$series_id != paste0(x$state_abbr, suffix)

  by_state <- do.call(rbind, lapply(seq_along(state.abb), function(i) {
    idx <- which(x$state_abbr == state.abb[i])
    inside <- idx[valid_year[idx]]
    n_unique <- length(unique(x$year[inside]))
    problems <- c(26L - n_unique, sum(!valid_year[idx]),
                  sum(duplicated(x$year[inside])), sum(invalid_value[idx]),
                  sum(wrong_name[idx]), sum(wrong_series[idx]))
    data.frame(
      dataset = dataset, state = state.name[i], state_abbr = state.abb[i],
      observed_rows = length(idx), unique_years = n_unique,
      missing_years = problems[1], extra_rows = problems[2],
      duplicate_rows = problems[3], invalid_values = problems[4],
      state_name_errors = problems[5], series_id_errors = problems[6],
      status = if (all(problems == 0) && length(idx) == 26L) "PASS" else "FAIL"
    )
  }))

  counts <- c(rows = nrow(x), missing = nrow(missing), extra = nrow(extra),
              duplicate = sum(duplicated(key[valid_key])),
              invalid_value = sum(invalid_value), wrong_name = sum(wrong_name),
              wrong_series = sum(wrong_series))
  ok <- all(counts[c("missing", "extra", "duplicate", "invalid_value",
                     "wrong_name", "wrong_series")] == 0)
  list(data = x, by_state = by_state, counts = counts, ok = ok,
       missing = missing, extra = extra, duplicates = duplicates,
       bad_values = x[invalid_value, ], bad_names = x[wrong_name, ],
       bad_series = x[wrong_series, ], keys = unique(key[valid_key]))
}

gdp <- check_data("state_real_gdp_2000_2025.csv",
                  "real_gdp_millions_chained_2017_usd", "RGSP", "Real GDP")
pop <- check_data("state_population_2000_2025.csv",
                  "population_thousands", "POP", "Population")

# Save a state-level summary, including any missing or extra observations.
summary <- rbind(gdp$by_state, pop$by_state)
readr::write_csv(summary, here::here("data", "process", "state_data_summary.csv"))

same_keys <- setequal(gdp$keys, pop$keys)
passed <- gdp$ok && pop$ok && same_keys

# Write a report with counts and specific problem locations.
format_rows <- function(rows, limit = 20L) {
  if (nrow(rows) == 0) return("None")
  examples <- paste0(rows$state_abbr, " / ", rows$year)
  examples <- unique(examples)
  out <- paste(utils::head(examples, limit), collapse = ", ")
  if (length(examples) > limit) out <- paste0(out, ", ...")
  out
}

report <- c(
  "# State Population and Real GDP: Data Validation Report",
  "",
  "Period: 2000-2025 | Coverage: 50 U.S. states (excluding DC)",
  "Expected: 50 states x 26 years = 1,300 rows per dataset",
  paste0("Overall result: **", if (passed) "PASS" else "FAIL", "**"),
  "",
  "## Validation summary",
  "| Check | Real GDP | Population |",
  "|---|---:|---:|",
  sprintf("| Rows | %d | %d |", gdp$counts["rows"], pop$counts["rows"]),
  sprintf("| Missing state-years | %d | %d |", gdp$counts["missing"], pop$counts["missing"]),
  sprintf("| Out-of-scope rows | %d | %d |", gdp$counts["extra"], pop$counts["extra"]),
  sprintf("| Duplicate state-years (excess) | %d | %d |", gdp$counts["duplicate"], pop$counts["duplicate"]),
  sprintf("| Missing/invalid/nonpositive values | %d | %d |", gdp$counts["invalid_value"], pop$counts["invalid_value"]),
  sprintf("| Incorrect state names | %d | %d |", gdp$counts["wrong_name"], pop$counts["wrong_name"]),
  sprintf("| Incorrect series IDs | %d | %d |", gdp$counts["wrong_series"], pop$counts["wrong_series"]),
  paste0("| GDP/population keys match | ", if (same_keys) "Yes" else "No", " | |"),
  "",
  "## GDP issues (state / year)",
  paste0("- Missing: ", format_rows(gdp$missing)),
  paste0("- Out of scope: ", format_rows(gdp$extra)),
  paste0("- Duplicates: ", format_rows(gdp$duplicates)),
  paste0("- Invalid values: ", format_rows(gdp$bad_values)),
  paste0("- Incorrect state names: ", format_rows(gdp$bad_names)),
  paste0("- Incorrect series IDs: ", format_rows(gdp$bad_series)),
  "",
  "## Population issues (state / year)",
  paste0("- Missing: ", format_rows(pop$missing)),
  paste0("- Out of scope: ", format_rows(pop$extra)),
  paste0("- Duplicates: ", format_rows(pop$duplicates)),
  paste0("- Invalid values: ", format_rows(pop$bad_values)),
  paste0("- Incorrect state names: ", format_rows(pop$bad_names)),
  paste0("- Incorrect series IDs: ", format_rows(pop$bad_series)),
  "",
  "Note: This checks file structure and internal consistency, not the accuracy of FRED's published figures."
)
writeLines(report, here::here("data", "process", "data_validation_report.md"))

# Export a combined panel only when both raw datasets pass validation.
if (passed) {
  panel <- merge(
    gdp$data[c("state", "state_abbr", "year", "real_gdp_millions_chained_2017_usd")],
    pop$data[c("state_abbr", "year", "population_thousands")],
    by = c("state_abbr", "year")
  )
  panel <- panel[order(panel$state_abbr, panel$year), ]
  readr::write_csv(panel, here::here("data", "process", "state_panel_2000_2025.csv"))
} else {
  warning("Validation failed. Review data_validation_report.md before merging.")
}

message("Validation ", if (passed) "PASSED" else "FAILED",
        ". Reports saved to data/process/.")
