# How Does Population Growth Drive Real GDP Growth?

**Blog Post 07 — Interactive Data Visualization with Plotly**  
**Computational Methods for Economists | Fall 2026**

An interactive study of how population growth and real GDP growth relate across **all 50 U.S. states from 2000 through 2025**. The project uses annual Census population estimates and BEA real GDP data accessed through the Federal Reserve Economic Data (FRED) API, processes them in R, and presents the results in a Quarto blog post with interactive Plotly charts.

## Research question

**How does population growth relate to real GDP growth across U.S. states, and how does that relationship vary over time?**

A growing population may expand labor supply, demand, and economic activity, but real GDP can also increase because output *per resident* rises. This project distinguishes those two accounting components rather than treating population gains as proof of economic success.

The analysis is **descriptive, not causal**. A ratio of population growth to GDP growth is *not* an estimate of the causal share of GDP growth produced by population change.

## Interactive blog and figures

The Quarto post is [`Blog07_Population_GDP.qmd`](Blog07_Population_GDP.qmd). It embeds two interactive Plotly HTML files:

| Figure | File | What to explore |
|---|---|---|
| **1. Population share of real GDP growth** | [`figures/figure_01_population_share_by_state.html`](figures/figure_01_population_share_by_state.html) | Move the year slider across 2001–2025; hover to compare population and GDP growth; switch between focused and full-range views. |
| **2. State population versus real GDP** | [`figures/figure_02_population_vs_real_gdp_2000_2025.html`](figures/figure_02_population_vs_real_gdp_2000_2025.html) | Compare economic and population **levels** across states from 2000–2025; hover for state details and use the time controls. |

**Important:** Figure 2 compares *levels*, not growth rates. The two visualizations complement each other but cannot establish a causal effect of population growth on GDP. Both HTML files must be present at the exact relative paths shown above when the QMD is rendered or published. Other experimental figures under `figures/` are optional and are not embedded in this particular post.

## Data

| Variable | Original provider | FRED series pattern | Unit | Frequency |
|---|---|---|---|---|
| State real GDP | U.S. Bureau of Economic Analysis (BEA) | `<STATE>RGSP`, e.g. [`TXRGSP`](https://fred.stlouisfed.org/series/TXRGSP) | Millions of chained 2017 U.S. dollars | Annual |
| State resident population | U.S. Census Bureau | `<STATE>POP`, e.g. [`TXPOP`](https://fred.stlouisfed.org/series/TXPOP) | Thousands of people | Annual |

- **Coverage:** 50 U.S. states, excluding Washington, D.C.
- **Levels:** 2000–2025 (26 years; 1,300 state-year records).
- **Annual growth rates:** 2001–2025 (25 years; up to 1,250 state-year records). Growth for 2000 is undefined because 1999 is not included.
- **Access:** [FRED](https://fred.stlouisfed.org/) through the `fredr` R package and a locally stored FRED API key.
- **Timing caveat:** Population observations refer to approximately July 1, whereas real GDP measures production during the calendar year; their annual reference windows are not identical.
- **Revisions:** Data available from FRED may be revised. Re-running the acquisition script later may not reproduce every archived value exactly unless the original CSV inputs are retained.

### Main derived measures

For state $s$ and year $t$:

$$
\text{Real GDP per capita}_{s,t}
= \frac{\text{Real GDP}_{s,t}}{\text{Population}_{s,t}}.
$$

The code converts the original units (GDP in millions, population in thousands) before reporting GDP per person in chained 2017 dollars. Annual percentage growth is

$$
g_{X,s,t}=100\left(\frac{X_{s,t}}{X_{s,t-1}}-1\right).
$$

For ordinary percentage growth rates, the **exact accounting identity** is

$$
g_{GDP}=g_{Population}+g_{GDP\ per\ capita}
+\frac{g_{Population}\,g_{GDP\ per\ capita}}{100}.
$$

Ignoring the small interaction term gives the familiar approximation $g_{GDP}\approx g_{Population}+g_{GDP\ per\ capita}$.

The project also reports

$$
\text{Population share of GDP growth}=100\frac{g_{Population}}{g_{GDP}}.
$$

This ratio can be negative, greater than 100%, or very unstable when GDP growth is near zero. It describes relative growth rates, **not causal contribution**. The analysis file also includes an exact-identity check and its numerical residual.

## Repository structure

The following is the intended **combined analysis-and-blog layout**. Keep the files you use under version control; optional exploration scripts and figures may differ between local copies.

```text
RodgersYe_6400_Blog07/
├── RodgersYe_6400_Blog07.Rproj
├── Blog07_Population_GDP.qmd
├── README.md
├── .gitignore
├── .Renviron                         # local secret; never commit
├── code/
│   ├── 01_get_raw_data.R
│   ├── 02_clean_data.R
│   ├── 03_analyze_data.R
│   ├── 04_make_figures.R
│   ├── 05_make_quadrant_bubble.R     # optional exploration
│   ├── 06_make_growth_map.R         # optional exploration
│   └── 07_make_gdp_growth_map.R     # optional exploration
├── data/
│   ├── raw/
│   │   ├── state_real_gdp_2000_2025.csv
│   │   └── state_population_2000_2025.csv
│   └── process/
│       ├── data_validation_report.md
│       ├── state_data_summary.csv
│       ├── state_panel_2000_2025.csv
│       └── Analysis_Results.csv
└── figures/
    ├── figure_01_population_share_by_state.html
    ├── figure_02_population_vs_real_gdp_2000_2025.html
    └── ...                              # optional additional charts
```

`data/process/Analysis_Results.csv` contains one row per state-year and the underlying levels, growth rates, relative shares, and GDP growth identity checks. The validation report and state summary document missing years, duplicated records, invalid values, and state/series ID consistency.

**Figure reproducibility note:** Some experimental visualization scripts save HTML files with *different names* from the two files referenced in the blog. Keep the exact two HTML assets above, or update the QMD's `iframe src` and `resources` paths to match your regenerated files. For full source-level reproduction of the **specific two blog figures**, the repository must also include the scripts that generated those exact Plotly widgets; pre-rendered HTML by itself is not a substitute for their source code.

## Requirements

- R and RStudio (recommended)
- [Quarto CLI](https://quarto.org/docs/get-started/) for rendering the `.qmd`
- A [FRED API key](https://fred.stlouisfed.org/docs/api/api_key.html) **only if re-downloading the source data**
- R packages: `here`, `fredr`, `readr`, `dplyr`, `plotly`, `htmlwidgets`

Install the R packages once:

```r
install.packages(c("here", "fredr", "readr", "dplyr", "plotly", "htmlwidgets"))
```

## Reproduce the analysis

### 1. Open the project

Open `RodgersYe_6400_Blog07.Rproj` in RStudio. Project-relative paths are handled through `here::here()`; do not replace them with machine-specific absolute paths.

### 2. Configure the FRED API key privately

Create a `.Renviron` file in the **project root** containing:

```dotenv
FRED_API_KEY=YOUR_FRED_API_KEY
```

The `.Renviron` file must **not** be committed or uploaded. Include the following in `.gitignore`:

```gitignore
.Renviron
.Rhistory
.RData
.Rproj.user/
```

If a secret was previously committed, `.gitignore` alone will not untrack it; remove it from Git history as appropriate and rotate the exposed key. You do **not** need an API key to render the supplied QMD or inspect the already-saved CSV and HTML files.

### 3. Run the R pipeline

In the RStudio Console:

```r
source(here::here("code", "01_get_raw_data.R"))
source(here::here("code", "02_clean_data.R"))
source(here::here("code", "03_analyze_data.R"))
```

1. `01_get_raw_data.R` downloads and stores original annual state GDP and population observations in `data/raw/`.
2. `02_clean_data.R` validates 50-state, 2000–2025 coverage and creates quality-control reports in `data/process/`.
3. `03_analyze_data.R` calculates growth and per-capita measures and saves `data/process/Analysis_Results.csv`.

**Check your local version of script 03:** the latest project design places identity-verification columns **inside** `Analysis_Results.csv`. An older script may create a separate `GDP_Growth_Identity_Verification.csv`; that file is not required by the blog.

### 4. Render the blog

From the project root, run in a terminal:

```bash
quarto render Blog07_Population_GDP.qmd --to html
```

Or open `Blog07_Population_GDP.qmd` in RStudio and click **Render**. The generated `Blog07_Population_GDP.html` embeds the two existing Plotly widgets by iframe. Rendering the blog does **not** require re-downloading the data or running all R scripts.

If you move the QMD into another Quarto website, copy the `figures/` directory alongside it and update the relative iframe and YAML `resources` paths as necessary. Publish the figure HTML files along with the page; otherwise the iframes will be blank. The QMD intentionally sets `embed-resources: false` to preserve these external HTML assets.

## Interpretation and limitations

- A higher population-growth-to-GDP-growth ratio does **not** prove stronger population-driven causation; it can rise simply because GDP growth slows.
- Large-population states generally have larger economies, but **economic size** is different from **economic growth**.
- Real GDP per capita is not household income, median well-being, or a direct measure of labor productivity.
- Industry mix, migration, employment, capital accumulation, prices, and economic shocks can affect state patterns. This project does not separately identify their causal roles.
- These are state-level descriptive comparisons, not a causal research design or individual-level panel analysis.

## Data sources and references

- [Federal Reserve Economic Data (FRED)](https://fred.stlouisfed.org/)
- [U.S. Bureau of Economic Analysis (BEA)](https://www.bea.gov/data/gdp/gdp-state)
- [U.S. Census Bureau — Population Estimates](https://www.census.gov/programs-surveys/popest.html)
- [FRED API documentation](https://fred.stlouisfed.org/docs/api/fred/)
- [Quarto documentation](https://quarto.org/docs/)
- [Plotly for R](https://plotly.com/r/)

## Submission checklist

- [ ] The Quarto post renders locally with **both interactive figures** functioning.
- [ ] The published post includes the two Plotly HTML assets at the correct relative URLs.
- [ ] Data sources, units, methods, limitations, and the takeaway are documented.
- [ ] The repository includes raw data or a functioning retrieval script, cleaned/processed results, and the scripts needed to reproduce the **actual blog figures**.
- [ ] `.Renviron` and other secrets are excluded from Git.
- [ ] Submit **both** the published blog URL and the GitHub repository URL, as required for Blog Post 07.
