
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pitchimpact <img src="man/figures/logo_pitchimpact_oficial.png" align="right" width="130"/>

<!-- badges: start -->

<!-- badges: end -->

**pitchimpact** is an R package under development focused on **football
performance analysis** using advanced statistics sourced from **FBref**.

The main goal of the package is to **standardize, integrate, and analyze
player- and team-level data** from multiple FBref tables (Standard
Stats, Shooting, Passing, Defensive Actions, and Possession), providing
a clean and reproducible workflow for exploratory analysis, modeling,
and visualization.

This project is primarily designed for **academic and learning
purposes**, with a strong emphasis on data cleaning, schema design, and
end-to-end analytical workflows.  
In the long term, it aims to serve as a foundation for applied football
analytics projects.

## Installation

You can install the development version of pitchimpact from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ifylopez/pitchimpact")
#> ✔ Updated metadata database: 5.48 MB in 4 files.
#> ℹ Updating metadata database✔ Updating metadata database ... done
#>  
#> → Will update 1 package.
#> → Will download 1 package with unknown size.
#> + pitchimpact 0.0.0.9000 → 0.0.0.9000 [bld][cmp][dl] (GitHub: 6cf6121)
#> ℹ Getting 1 pkg with unknown size
#> ✔ Cached copy of pitchimpact 0.0.0.9000 (source) is the latest build
#> ✔ Installed pitchimpact 0.0.0.9000 (github::ifylopez/pitchimpact@6cf6121) (253ms)
#> ✔ 1 pkg + 31 deps: kept 24, upd 1 [33s]
```

## First steps

Load the package with:

``` r
library(pitchimpact)
```

## Included example datasets

The package includes several **example datasets** derived from FBref
tables. These datasets are intended to illustrate the expected structure
of the data and to support reproducible examples, vignettes, and
workflows.

| Dataset name | Description |
|----|----|
| `standard_example` | Standard season-level player statistics |
| `shooting_example` | Shooting and shot quality metrics |
| `passing_example` | Passing volume, accuracy, and creativity metrics |
| `defensive_actions_example` | Defensive actions and pressures |
| `possession_example` | Ball control, carries, and possession metrics |
| `master_example` | Fully merged player-season dataset |

### Quick example

``` r
data(standard_example)
dplyr::select(standard_example, player, goals, xg) |> head()
#> # A tibble: 6 × 3
#>   player          goals    xg
#>   <chr>           <dbl> <dbl>
#> 1 Agustín Sández      2   1.5
#> 2 Jorge Broun         0   0  
#> 3 Víctor Malcorra     7   5.9
#> 4 Franco Ibarra       1   0.6
#> 5 Emanuel Coronel     0   0.2
#> 6 Jaminton Campaz     4   3
```

## Available functions

### Data import and standardization

The core functionality of the package revolves around importing FBref
tables exported as CSV files and converting them into a consistent
schema:

- **`import_fbref_standard()`**  
  Imports and standardizes the *Standard Stats* table.

<!-- -->

- **`import_fbref_shooting()`**  
  Imports and standardizes the *Shooting* table.

<!-- -->

- **`import_fbref_passing()`**  
  Imports and standardizes the *Passing* table.

<!-- -->

- **`import_fbref_defensive_actions()`**  
  Imports and standardizes the *Defensive Actions* table.

<!-- -->

- **`import_fbref_possession()`**  
  Imports and standardizes the *Possession* table.

Each function returns **one row per player-season**, with cleaned
variable names and consistent data types.

### Table integration

- **`merge_fbref_tables()`**  
  Merges multiple standardized FBref tables into a single **master
  player-season dataset**.

The resulting table preserves one row per player and aggregates all
available metrics across statistical domains.

## Basic workflow

The package includes detailed vignettes that walk through the complete
workflow, from exporting FBref tables to building a unified dataset:

- **From FBref to a master table**

- (More coming soon)

You can access them with:

``` r
browseVignettes("pitchimpact")
#> No vignettes found by browseVignettes("pitchimpact")
```

## Roadmap

The following roadmap outlines the current scope and planned development
of **pitchimpact**.

### ✅ Current features

- [x] Import FBref *Standard Stats* tables
- [x] Import FBref *Shooting* tables
- [x] Import FBref *Passing* tables
- [x] Import FBref *Defensive Actions* tables
- [x] Import FBref *Possession* tables
- [x] Standardized player-season schema across all tables
- [x] Merge multiple FBref tables into a single master dataset
- [x] Built-in example datasets for reproducible workflows
- [x] End-to-end vignette: *From FBref to master table*

### 🚧 In progress

- [ ] Improved validation and informative error messages
- [ ] Position-based summaries (defenders, midfielders, forwards)
- [ ] Basic player profiling functions

### 🔜 Planned features

- [ ] Team-level aggregated metrics
- [ ] Per-90 recalculation utilities
- [ ] Similarity metrics between players
- [ ] Simple visualization helpers for exploratory analysis
- [ ] Support for multi-season datasets

### 🌱 Long-term ideas

- [ ] Integration with match-level data
- [ ] Expected impact metrics by position
- [ ] Interactive dashboards (Shiny / Quarto)
- [ ] Applied case studies using real teams

The roadmap is intentionally flexible and may evolve as the project
grows.

## Contributing

Contributions, suggestions, and feedback are very welcome! Please refer
to the [Contributing Guide](.github/CONTRIBUTING.md) for details on how
to get involved.

## Code of Conduct

We want **pitchimpact** to be a respectful, inclusive, and collaborative
project. All interactions are governed by our [Code of
Conduct](CODE_OF_CONDUCT.md). By participating, you agree to abide by
these guidelines.

### Author

- **Mateo López**  
  <https://github.com/ifylopez>
