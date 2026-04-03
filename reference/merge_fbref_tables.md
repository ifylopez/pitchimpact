# Merge FBref tables into a player-season master table

Joins standardized FBref tables into a single player-season dataset
keyed by team, season, and player. When `standard` is provided,
overlapping columns from other tables are joined with table-specific
suffixes (e.g. `_shooting`) and then dropped, keeping the `standard`
version as source of truth.

## Usage

``` r
merge_fbref_tables(
  standard = NULL,
  shooting = NULL,
  passing = NULL,
  defensive = NULL,
  possession = NULL,
  key = c("team", "season", "player"),
  strict = TRUE
)
```

## Arguments

- standard:

  Tibble from
  [`import_fbref_standard()`](https://ifylopez.github.io/pitchimpact/reference/import_fbref_standard.md).
  Recommended.

- shooting:

  Tibble from
  [`import_fbref_shooting()`](https://ifylopez.github.io/pitchimpact/reference/import_fbref_shooting.md).
  Optional.

- passing:

  Tibble from
  [`import_fbref_passing()`](https://ifylopez.github.io/pitchimpact/reference/import_fbref_passing.md).
  Optional.

- defensive:

  Tibble from
  [`import_fbref_defensive_actions()`](https://ifylopez.github.io/pitchimpact/reference/import_fbref_defensive_actions.md).
  Optional.

- possession:

  Tibble from
  [`import_fbref_possession()`](https://ifylopez.github.io/pitchimpact/reference/import_fbref_possession.md).
  Optional.

- key:

  Character vector of join keys. Default: c("team","season","player").

- strict:

  If TRUE, stops when duplicated keys are detected within any input
  table.

## Value

A tibble with one row per player-season and columns from all provided
tables.

## Examples

``` r
# Load example datasets included in the package
data(standard_example)
data(shooting_example)

# Merge tables into a player-season master dataset
master <- merge_fbref_tables(
  standard_example,
  shooting_example
)

# Inspect result
head(master)
#> # A tibble: 6 × 40
#>   team    season player nation_raw nation_code pos_raw pos_primary pos_secondary
#>   <chr>    <dbl> <chr>  <chr>      <chr>       <chr>   <chr>       <chr>        
#> 1 Rosari…   2025 Agust… py PAR     PAR         DF      DF          NA           
#> 2 Rosari…   2025 Jorge… ar ARG     ARG         GK      GK          NA           
#> 3 Rosari…   2025 Vícto… ar ARG     ARG         MF      MF          NA           
#> 4 Rosari…   2025 Franc… ar ARG     ARG         MF      MF          NA           
#> 5 Rosari…   2025 Emanu… ar ARG     ARG         DF      DF          NA           
#> 6 Rosari…   2025 Jamin… co COL     COL         MF,FW   MF          FW           
#> # ℹ 32 more variables: age <dbl>, mp <dbl>, starts <dbl>, minutes <dbl>,
#> #   nineties <dbl>, goals <dbl>, assists <dbl>, goals_assists <dbl>,
#> #   non_pen_goals <dbl>, pens_made <dbl>, pens_att <dbl>, yellow <dbl>,
#> #   red <dbl>, xg <dbl>, npxg <dbl>, xag <dbl>, npxg_plus_xag <dbl>,
#> #   prog_carries <dbl>, prog_passes <dbl>, prog_passes_received <dbl>,
#> #   shots <dbl>, shots_on_target <dbl>, shots_on_target_pct <dbl>,
#> #   shots_per90 <dbl>, shots_on_target_per90 <dbl>, goals_per_shot <dbl>, …
```
