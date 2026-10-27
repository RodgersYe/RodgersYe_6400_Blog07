# ============================================================================
# 04_make_figures.R
# Combine two original graphs, keeping the intended order:
#   Figure 01: Population share of real GDP growth (from 04_make_figures_updated.R)
#   Figure 02: Population versus real GDP with yearly OLS (from 07_make_population_gdp_scatter.R)
#
# Run from the RodgersYe_6400_Blog07 project root in RStudio:
#   source("code/04_make_figures.R")
#
# Input:
#   data/process/Analysis_Results.csv
#
# Outputs:
#   figures/figure_01_population_share_by_state.html
#   figures/figure_02_population_vs_real_gdp_2000_2025.html
#   data/process/population_gdp_ols_by_year.csv
# ============================================================================

# 0. Libraries and shared data -------------------------------------------------
needed <- c("plotly", "htmlwidgets")
missing_packages <- needed[!vapply(needed, requireNamespace,
                                    logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "),
       ". Install using install.packages().", call. = FALSE)
}

# 2. Load data ----------------------------------------------------------------
input_candidates <- c(
  file.path("data", "process", "Analysis_Results.csv"),
  file.path("data", "process", "Analysis_Results(2).csv"),
  "Analysis_Results(2).csv"
)
found <- input_candidates[file.exists(input_candidates)]
if (!length(found)) {
  stop("Can't find Analysis_Results.csv. Open the .Rproj file and run ",
       "the script from your project root.", call. = FALSE)
}
input_file <- found[[1]]
dat <- read.csv(input_file, stringsAsFactors = FALSE,
                check.names = FALSE)
required <- c("state", "state_abbr", "year", "population_thousands",
              "real_gdp_millions_chained_2017_usd")
missing_cols <- setdiff(required, names(dat))
if (length(missing_cols)) {
  stop("Required CSV columns missing: ", paste(missing_cols, collapse = ", "),
       call. = FALSE)
}

# Restrict to the 50 states (exclude D.C. if it appears in future data).
dat <- dat[dat$state_abbr %in% state.abb, , drop = FALSE]
dat$year <- as.integer(dat$year)
dat <- dat[order(dat$year, dat$state_abbr), , drop = FALSE]
if (anyNA(dat[, c("year", "population_thousands",
                   "real_gdp_millions_chained_2017_usd")]) ||
    any(dat$population_thousands <= 0) ||
    any(dat$real_gdp_millions_chained_2017_usd <= 0)) {
  stop("Missing, zero or negative years/population/GDP values found.",
       call. = FALSE)
}
if (anyDuplicated(dat[c("year", "state_abbr")])) {
  stop("Duplicate state-year observations in the data.", call. = FALSE)
}
years <- sort(unique(dat$year))
if (!setequal(dat$state_abbr, state.abb) ||
    any(table(dat$year) != 50L) ||
    nrow(dat) != 50L * length(years)) {
  stop("Expected exactly one row for each of the 50 U.S. states in every year.",
       call. = FALSE)
}


# 1. Figure 01: Population share of real GDP growth ---------------------------
# Reuse the shared dataset; Figure 01 covers only 2001-2025.
share_required <- c("real_gdp_growth_pct", "real_gdp_per_capita_growth_pct",
                    "population_growth_pct", "population_share_of_gdp_growth_pct")
missing_share_cols <- setdiff(share_required, names(dat))
if (length(missing_share_cols)) {
  stop("Missing columns for Figure 01: ", paste(missing_share_cols, collapse = ", "),
       ". Run 03_analyze_data.R first.", call. = FALSE)
}

state_order <- sort(state.abb)
share_data <- dat[dat$year >= 2001 & dat$year <= 2025, , drop = FALSE]
if (nrow(share_data) != 50L * 25L ||
    !setequal(unique(share_data$year), 2001:2025) ||
    any(table(share_data$year) != 50L) ||
    anyNA(share_data[c("state", "state_abbr", "year",
                       "real_gdp_growth_pct", "real_gdp_per_capita_growth_pct",
                       "population_growth_pct")]) ||
    any(!is.finite(as.matrix(share_data[c(
      "real_gdp_growth_pct", "real_gdp_per_capita_growth_pct",
      "population_growth_pct"
    )])))) {
  stop("The Figure 01 data (50 states, 2001-2025) are incomplete or invalid.",
       call. = FALSE)
}

# Undefined shares can occur when real GDP growth is zero / very close to zero.
# Keep the original share values and tooltips, but omit undefined ratios from bars.
share_data$population_share_of_gdp_growth_pct[
  !is.finite(share_data$population_share_of_gdp_growth_pct)
] <- NA_real_
share_data$state_abbr <- factor(share_data$state_abbr, levels = state_order)
share_data$share_label <- ifelse(
  is.na(share_data$population_share_of_gdp_growth_pct),
  "Undefined (real GDP growth near zero)",
  sprintf("%+.2f%%", share_data$population_share_of_gdp_growth_pct)
)
share_data$share_hover <- paste0(
  "<b>", share_data$state, " (", share_data$state_abbr, ")</b>",
  "<br>Year: ", share_data$year,
  "<br>POP share: ", share_data$share_label,
  "<br>RGDP: ", sprintf("%+.2f%%", share_data$real_gdp_growth_pct),
  "<br>PC: ", sprintf("%+.2f%%", share_data$real_gdp_per_capita_growth_pct),
  "<br>POP: ", sprintf("%+.2f%%", share_data$population_growth_pct)
)

# Extreme ratios occur when real GDP growth is close to zero.
# The view buttons allow focused comparison or the complete range.
if (all(is.na(share_data$population_share_of_gdp_growth_pct))) {
  stop("All population shares are missing.")
}

max_share <- max(abs(share_data$population_share_of_gdp_growth_pct),
                 na.rm = TRUE)
full_limit <- max(200, ceiling(max_share / 1000) * 1000)
full_tick <- max(200, ceiling(full_limit / 5 / 100) * 100)

view_buttons <- list(
  list(
    label = "Focus (+/-200%)",
    method = "relayout",
    args = list(list("yaxis.range" = c(-200, 200), "yaxis.dtick" = 20))
  ),
  list(
    label = "Full range",
    method = "relayout",
    args = list(list(
      "yaxis.range" = c(-full_limit, full_limit),
      "yaxis.dtick" = full_tick
    ))
  )
)

fig_share <- plotly::plot_ly(height = 800)
fig_share <- plotly::add_bars(
  fig_share,
    data = share_data,
    x = ~state_abbr,
    y = ~population_share_of_gdp_growth_pct,
    frame = ~year,
    ids = ~state_abbr,
    name = "Population share of GDP growth",
    marker = list(color = "#FBE7A1",
                  line = list(color = "#D4B95A", width = 0.6)),
    text = ~share_hover,
    hoverinfo = "text"
  )
fig_share <- plotly::layout(
  fig_share,
    title = "Figure 01: Population Growth Rate Share of Real GDP Growth Rate by State, 2001-2025",
    showlegend = FALSE,
    bargap = 0.18,
    xaxis = list(
      title = "State (alphabetical by abbreviation)",
      type = "category",
      categoryorder = "array",
      categoryarray = state_order,
      tickangle = -60,
      tickfont = list(size = 10),
      showgrid = FALSE
    ),
    yaxis = list(
      title = "Population share of real GDP growth (%)",
      range = c(-200, 200),
      tick0 = 0,
      dtick = 20,
      ticksuffix = "%",
      showgrid = TRUE,
      griddash = "dash",
      gridcolor = "rgba(80,95,110,0.20)",
      zeroline = TRUE
    ),
    updatemenus = list(list(
      type = "buttons",
      direction = "right",
      x = 0, y = 1.12,
      xanchor = "left", yanchor = "bottom",
      buttons = view_buttons
    )),
    annotations = list(list(
      x = 0, y = -0.34, xref = "paper", yref = "paper",
      xanchor = "left", yanchor = "top", showarrow = FALSE,
      align = "left", font = list(size = 11, color = "#495566"),
      text = paste0(
        "<b>Notes:</b> POP share = 100 x POP / RGDP; ",
        "POP = population growth rate; ",
        "RGDP = real GDP growth rate; ",
        "PC = real GDP per capita growth rate.<br>",
        "Shares may be negative or above 100% when RGDP is small or negative. ",
        "Focus clips bars beyond +/-200%; Full range displays all values.<br>",
        "This ratio is not a causal contribution. ",
        "Source: FRED (BEA and U.S. Census Bureau)."
      )
    )),
    margin = list(t = 125, b = 250, l = 95, r = 40)
  )
fig_share <- plotly::animation_opts(fig_share, frame = 900, transition = 0,
                                    redraw = TRUE)
fig_share <- plotly::animation_slider(fig_share,
                                     currentvalue = list(prefix = "Year: "))
fig_share <- plotly::animation_button(fig_share, x = 1, xanchor = "right",
                                     y = 0, yanchor = "bottom")
fig_share <- plotly::config(fig_share, displaylogo = FALSE, responsive = TRUE)


# Export Figure 01 ------------------------------------------------------------
out_dir <- "figures"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
share_file <- file.path(out_dir, "figure_01_population_share_by_state.html")
tryCatch(
  htmlwidgets::saveWidget(fig_share, file = share_file, selfcontained = TRUE,
                          title = "Figure 01: Population Share of Real GDP Growth"),
  error = function(e) {
    warning("Self-contained Figure 01 HTML export failed: ", conditionMessage(e),
            ". Exporting with a dependency folder instead.")
    htmlwidgets::saveWidget(fig_share, file = share_file, selfcontained = FALSE,
                            title = "Figure 01: Population Share of Real GDP Growth")
  }
)
message("Saved Figure 01: ", normalizePath(share_file))


# 2. Figure 02: Population vs. Real GDP and yearly OLS ------------------------
# Units are explicit so the fitted coefficients are interpretable:
#   x: population (millions of people), original population is in thousands
#   y: real GDP (billions of chained 2017 USD), original GDP is in millions
dat$population_millions <- dat$population_thousands / 1000
dat$gdp_billions <- dat$real_gdp_millions_chained_2017_usd / 1000

# 3. Run a separate cross-sectional OLS regression for each year ---------------
# Ordinary least squares: real GDP = intercept + slope * population.
fit_results <- lapply(years, function(yr) {
  d <- dat[dat$year == yr, , drop = FALSE]
  fit <- lm(gdp_billions ~ population_millions, data = d)
  data.frame(year = yr, n_states = nrow(d),
             slope_a = unname(coef(fit)[[2]]),
             intercept_b = unname(coef(fit)[[1]]),
             r_squared = unname(summary(fit)$r.squared))
})
reg <- do.call(rbind, fit_results)

# Draw each yearly regression line on a consistent x-domain.
x_max <- max(dat$population_millions) * 1.08
y_max <- max(dat$gdp_billions) * 1.12
line_x <- seq(0, x_max, length.out = 120)
reg_lines <- do.call(rbind, lapply(seq_len(nrow(reg)), function(i) {
  r <- reg[i, ]
  data.frame(
    year = r$year,
    population_millions = line_x,
    fitted_gdp_billions = r$slope_a * line_x + r$intercept_b,
    equation = sprintf("<b>%d OLS regression</b><br>y = %.2fx %+.2f<br>R-squared = %.4f",
                       r$year, r$slope_a, r$intercept_b, r$r_squared)
  )
}))

# Optional: equation is always visible on the graph, not only on hover.
reg$equation_label <- sprintf(
  "<b>%d fitted line</b><br>y = %.2fx %+.2f<br>R\u00b2 = %.4f",
  reg$year, reg$slope_a, reg$intercept_b, reg$r_squared
)
labels <- data.frame(
  year = reg$year,
  population_millions = rep(x_max * 0.025, length(years)),
  gdp_billions = rep(y_max * 0.93, length(years)),
  label = reg$equation_label
)

# Hover includes the state and values; growth indicators are shown if present.
fmt_growth <- function(x) ifelse(is.na(x), "N/A", sprintf("%+.2f%%", x))
fmt_num <- function(x, digits = 2) formatC(x, format = "f", digits = digits,
                                            big.mark = ",")
dat$hover_text <- paste0(
  "<b>", dat$state, " (", dat$state_abbr, ")</b>",
  "<br>Year: ", dat$year,
  "<br>Population: ", fmt_num(dat$population_millions, 3), " million",
  "<br>Real GDP: $", fmt_num(dat$gdp_billions, 1),
  " billion (chained 2017 USD)"
)
if ("population_growth_pct" %in% names(dat)) {
  dat$hover_text <- paste0(dat$hover_text,
                           "<br>Population growth: ",
                           fmt_growth(dat$population_growth_pct))
}
if ("real_gdp_growth_pct" %in% names(dat)) {
  dat$hover_text <- paste0(dat$hover_text,
                           "<br>Real GDP growth: ",
                           fmt_growth(dat$real_gdp_growth_pct))
}

# 4. Animated Plotly figure ----------------------------------------------------
# Separate traces for fitted line, state dots and equation. Each uses the
# SAME frame variable, so selecting a year updates all three simultaneously.
fig_gdp <- plotly::plot_ly(height = 710)

fig_gdp <- plotly::add_trace(
  fig_gdp, data = reg_lines,
  x = ~population_millions, y = ~fitted_gdp_billions,
  frame = ~year, type = "scatter", mode = "lines",
  name = "Yearly OLS fit",
  line = list(color = "#203A5E", width = 3.5),
  text = ~equation, hoverinfo = "text",
  showlegend = TRUE,
  inherit = FALSE
)
fig_gdp <- plotly::add_trace(
  fig_gdp, data = dat,
  x = ~population_millions, y = ~gdp_billions,
  frame = ~year, ids = ~state_abbr,
  type = "scatter", mode = "markers",
  name = "U.S. states",
  marker = list(size = 10, color = "#D94B57", opacity = 0.85,
                line = list(color = "white", width = 1)),
  text = ~hover_text, hoverinfo = "text",
  showlegend = TRUE,
  inherit = FALSE
)
fig_gdp <- plotly::add_trace(
  fig_gdp, data = labels,
  x = ~population_millions, y = ~gdp_billions,
  frame = ~year,
  type = "scatter", mode = "text",
  text = ~label, textposition = "middle right",
  textfont = list(color = "#203A5E", size = 14),
  hoverinfo = "skip", showlegend = FALSE,
  inherit = FALSE
)

fig_gdp <- plotly::layout(
  fig_gdp,
  title = list(
    text = "Figure 02: Population and Real GDP Across 50 U.S. States | 2000-2025",
    x = 0.5, xanchor = "center"
  ),
  xaxis = list(
    title = "Population (million people)",
    range = c(0, x_max), fixedrange = FALSE,
    showgrid = TRUE, gridcolor = "#E9EEF3", zeroline = FALSE
  ),
  yaxis = list(
    title = "Real GDP (billion chained 2017 USD)",
    range = c(0, y_max), fixedrange = FALSE,
    showgrid = TRUE, gridcolor = "#E9EEF3", zeroline = FALSE,
    tickprefix = "$", tickformat = ",.0f"
  ),
  plot_bgcolor = "white", paper_bgcolor = "white",
  font = list(family = "Arial", size = 13, color = "#263445"),
  legend = list(orientation = "h", x = 0.72, y = 1.04,
                xanchor = "center", yanchor = "bottom"),
  margin = list(l = 95, r = 50, t = 115, b = 105),
  hovermode = "closest"
)

fig_gdp <- plotly::animation_opts(fig_gdp, frame = 750, transition = 350,
                              easing = "linear", redraw = FALSE)
fig_gdp <- plotly::animation_slider(
  fig_gdp, currentvalue = list(prefix = "Year: ", font = list(size = 16)),
  pad = list(t = 52)
)
fig_gdp <- plotly::animation_button(fig_gdp, x = 1, xanchor = "right",
                                y = 0, yanchor = "bottom")


# Export Figure 02 and OLS regression summary -------------------------------
gdp_file <- file.path(out_dir, "figure_02_population_vs_real_gdp_2000_2025.html")
tryCatch(
  htmlwidgets::saveWidget(fig_gdp, file = gdp_file, selfcontained = TRUE),
  error = function(e) {
    warning("Self-contained Figure 02 HTML export failed: ", conditionMessage(e),
            ". Exporting with a dependency folder instead.")
    htmlwidgets::saveWidget(fig_gdp, file = gdp_file, selfcontained = FALSE)
  }
)

reg_csv <- file.path("data", "process", "population_gdp_ols_by_year.csv")
if (!dir.exists(dirname(reg_csv))) {
  dir.create(dirname(reg_csv), recursive = TRUE)
}
write.csv(reg[c("year", "n_states", "slope_a", "intercept_b", "r_squared")],
          file = reg_csv, row.names = FALSE)
message("Saved Figure 02: ", normalizePath(gdp_file))
message("Saved yearly regression coefficients: ", normalizePath(reg_csv))
message("For each year: GDP (billions) = a * population (millions) + b")
message("Finished: exactly two interactive figures created from ", input_file)

if (interactive()) print(fig_gdp)
