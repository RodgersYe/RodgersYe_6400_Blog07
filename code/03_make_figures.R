# 03_make_figures.R
# Final Interactive Real GDP Growth Decomposition

# Load packages
library(plotly)
library(htmlwidgets)

# Set file paths
input_file <- file.path("data", "process", "state_rgdp_population_2000_2025_processed.csv")
output_dir <- "figures"
output_file <- file.path(output_dir,"figure_1_gdp_growth_decomposition_by_state.html")
if (!dir.exists(output_dir)) {dir.create(output_dir, recursive = TRUE)}

# Read and prepare data
df <- read.csv(input_file, stringsAsFactors = FALSE)
required_cols <- c(
  "state_abbr",
  "year",
  "real_gdp_growth_rate",
  "population_growth_rate",
  "real_gdp_per_capita_growth_rate")

state_lookup <- setNames(datasets::state.name, datasets::state.abb)

df$state_name <- unname(state_lookup[df$state_abbr])

df <- df[
  df$year >= 2001 &
    df$year <= 2025 &
    !is.na(df$state_name) &
    is.finite(df$real_gdp_growth_rate) &
    is.finite(df$population_growth_rate) &
    is.finite(df$real_gdp_per_capita_growth_rate),]

df <- df[order(df$year, df$state_abbr),]

state_order <- sort(unique(df$state_abbr))

# Convert growth rates to percentage units
df$rgdp_pct <- df$real_gdp_growth_rate * 100
df$pop_pct <- df$population_growth_rate * 100
df$pc_pct <- df$real_gdp_per_capita_growth_rate * 100

# Animation year variable
df$year_frame <- factor(df$year, levels = sort(unique(df$year)))

# Create hover information
df$hover_text <- sprintf(
  paste0(
    "<b>%s (%s)</b>",
    "<br>Year: %d",
    "<br>Real GDP growth (RGDP): %+.2f%%",
    "<br>Per-capita real GDP growth (PC): %+.2f%%",
    "<br>Population growth (POP): %+.2f%%"),
  df$state_name,
  df$state_abbr,
  df$year,
  df$rgdp_pct,
  df$pc_pct,
  df$pop_pct)

# Set fixed Y-axis range
# Include positive and negative stacked components
positive_stack <- pmax(df$pc_pct, 0) + pmax(df$pop_pct, 0)
negative_stack <- pmin(df$pc_pct, 0) + pmin(df$pop_pct, 0)
all_values <- c(
  df$rgdp_pct,
  positive_stack,
  negative_stack)

# Preserve the template range and expand only if necessary
y_lower <- min(-14, 2 * floor((min(all_values) - 0.5) / 2))
y_upper <- max(24, 2 * ceiling((max(all_values) + 0.5) / 2))
fixed_y_range <- c(y_lower, y_upper)

# Create interactive decomposition chart
fig1 <- plot_ly(
  data = df,
  x = ~state_abbr,
  y = ~pc_pct,
  frame = ~year_frame,
  ids = ~state_abbr,
  type = "bar",
  name = "Real GDP per capita growth (PC)",
  marker = list(
    color = "#B8E4C7",
    line = list(
      color = "#1F77B4",
      width = 0.5
    )
  ),
  text = ~hover_text,
  hoverinfo = "text",
  height = 800
) %>%
  
  add_bars(
    y = ~pop_pct,
    name = "Population growth (POP)",
    marker = list(
      color = "#FBE7A1",
      line = list(
        color = "#FF7F0E",
        width = 0.5
      )
    ),
    text = ~hover_text,
    hoverinfo = "text"
  ) %>%
  
  add_markers(
    y = ~rgdp_pct,
    name = "Observed real GDP growth (RGDP)",
    marker = list(
      color = "#EF88B5",
      size = 9,
      line = list(
        color = "#B63C76",
        width = 1
      )
    ),
    text = ~hover_text,
    hoverinfo = "text"
  )

# Figure notes
figure_notes <- paste0(
  "<b>Notes:</b> ",
  "RGDP = annual real GDP growth; ",
  "PC = real GDP per capita growth; ",
  "POP = population growth.",
  "<br>",
  "Green + yellow bars show PC + POP ",
  "(approximate decomposition); ",
  "pink dots show observed RGDP. ",
  "The difference is PC × POP / 100 ",
  "percentage points.",
  "<br>",
  "Positive and negative bars stack on ",
  "opposite sides of zero. ",
  "Source: FRED (BEA and U.S. Census Bureau).")

# Set chart appearance
fig1 <- fig1 %>%
  layout(
    title = list(
      text = paste0(
        "Figure 1. Real GDP Growth Decomposition ",
        "Across U.S. States, 2001-2025"
      ),
      x = 0.5,
      xanchor = "center",
      font = list(
        family = "Arial",
        size = 20
      )
    ),
    barmode = "relative",
    hovermode = "closest",
    xaxis = list(
      title = "State (alphabetical by abbreviation)",
      type = "category",
      categoryorder = "array",
      categoryarray = state_order,
      tickangle = -60,
      tickfont = list(size = 10),
      showgrid = FALSE,
      automargin = TRUE
    ),
    yaxis = list(
      title = "Real GDP growth rate and components (%)",
      range = fixed_y_range,
      autorange = FALSE,
      fixedrange = TRUE,
      tick0 = 0,
      dtick = 2,
      ticksuffix = "%",
      showgrid = TRUE,
      griddash = "dash",
      gridcolor = "rgba(80,95,110,0.25)",
      zeroline = TRUE,
      zerolinecolor = "#555555",
      zerolinewidth = 1,
      minor = list(
        tick0 = 0,
        dtick = 0.2,
        showgrid = TRUE,
        griddash = "dash",
        gridcolor = "rgba(80,95,110,0.09)"
      )
    ),
    legend = list(
      orientation = "h",
      x = 0,
      y = 1.08,
      xanchor = "left"
    ),
    annotations = list(
      list(
        x = 0,
        y = -0.34,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "top",
        showarrow = FALSE,
        align = "left",
        text = figure_notes,
        font = list(
          family = "Arial",
          size = 11,
          color = "#495566"
        )
      )
    ),
    showlegend = TRUE,
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    font = list(
      family = "Arial",
      size = 12
    ),
    margin = list(
      l = 85,
      r = 35,
      t = 125,
      b = 250
    )
  )

# Configure year animation
fig1 <- fig1 %>%
  animation_opts(
    frame = 900,
    transition = 0,
    easing = "linear",
    redraw = TRUE,
    mode = "immediate"
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Year: ",
      xanchor = "right",
      font = list(
        size = 16,
        color = "#555555"
      )
    ),
    pad = list(t = 50)
  ) %>%
  animation_button(
    label = "Play",
    x = 1,
    xanchor = "right",
    y = 0,
    yanchor = "bottom",
    pad = list(
      t = 60,
      r = 5
    )
  )

# Configure and export
fig1 <- fig1 %>%
  config(
    responsive = TRUE,
    displayModeBar = TRUE,
    displaylogo = FALSE)

htmlwidgets::saveWidget(
  widget = fig1,
  file = output_file,
  selfcontained = TRUE,
  title = paste0(
    "Figure 1 - Real GDP Growth Decomposition ",
    "Across U.S. States"))
