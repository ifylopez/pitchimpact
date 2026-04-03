# Example FBref Standard Stats dataset

A cleaned and standardized example dataset containing FBref "Standard
Stats" for a single team and season. Each row represents one
player-season.

## Usage

``` r
standard_example
```

## Format

A tibble with one row per player-season and 28 columns:

- team:

  Team name.

- season:

  Season identifier.

- player:

  Player name.

- nation_raw:

  Original FBref nation cell (e.g., "ar ARG").

- nation_code:

  Three-letter nation code extracted from `nation_raw`.

- pos_raw:

  Original FBref position string (e.g., "MF,FW").

- pos_primary:

  Primary position (before comma).

- pos_secondary:

  Secondary position (after comma), if present; otherwise NA.

- age:

  Player age.

- mp:

  Matches played.

- starts:

  Games started.

- minutes:

  Minutes played.

- nineties:

  Minutes played divided by 90.

- goals:

  Goals scored.

- assists:

  Assists.

- goals_assists:

  Goals + assists.

- non_pen_goals:

  Non-penalty goals.

- pens_made:

  Penalty kicks made.

- pens_att:

  Penalty kicks attempted.

- yellow:

  Yellow cards.

- red:

  Red cards.

- xg:

  Expected goals (xG).

- npxg:

  Non-penalty expected goals (npxG).

- xag:

  Expected assisted goals (xAG).

- npxg_plus_xag:

  Non-penalty xG plus xAG.

- prog_carries:

  Progressive carries.

- prog_passes:

  Progressive passes.

- prog_passes_received:

  Progressive passes received.

## Source

FBref (<https://fbref.com>)

## Details

This dataset follows the schema produced by
[`import_fbref_standard()`](https://ifylopez.github.io/pitchimpact/reference/import_fbref_standard.md)
and is intended for examples, tests, and vignettes.

## Examples

``` r
data(standard_example, package = "pitchimpact")
dplyr::glimpse(standard_example)
#> Rows: 46
#> Columns: 28
#> $ team                 <chr> "Rosario Central", "Rosario Central", "Rosario Ce…
#> $ season               <dbl> 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2…
#> $ player               <chr> "Agustín Sández", "Jorge Broun", "Víctor Malcorra…
#> $ nation_raw           <chr> "py PAR", "ar ARG", "ar ARG", "ar ARG", "ar ARG",…
#> $ nation_code          <chr> "PAR", "ARG", "ARG", "ARG", "ARG", "COL", "ARG", …
#> $ pos_raw              <chr> "DF", "GK", "MF", "MF", "DF", "MF,FW", "DF", "DF"…
#> $ pos_primary          <chr> "DF", "GK", "MF", "MF", "DF", "MF", "DF", "DF", "…
#> $ pos_secondary        <chr> NA, NA, NA, NA, NA, "FW", NA, NA, NA, NA, NA, "MF…
#> $ age                  <dbl> 24, 38, 37, 23, 27, 24, 36, 28, 24, 29, 30, 36, 2…
#> $ mp                   <dbl> 30, 29, 27, 28, 29, 25, 23, 26, 23, 29, 19, 15, 1…
#> $ starts               <dbl> 30, 29, 27, 27, 24, 23, 23, 19, 17, 16, 16, 15, 1…
#> $ minutes              <dbl> 2575, 2609, 2389, 2302, 2167, 1924, 1834, 1883, 1…
#> $ nineties             <dbl> 28.6, 29.0, 26.5, 25.6, 24.1, 21.4, 20.4, 20.9, 1…
#> $ goals                <dbl> 2, 0, 7, 1, 0, 4, 0, 0, 0, 3, 0, 7, 5, 2, 2, 2, 0…
#> $ assists              <dbl> 1, 0, 5, 1, 3, 1, 2, 0, 0, 3, 0, 3, 1, 0, 0, 1, 1…
#> $ goals_assists        <dbl> 3, 0, 12, 2, 3, 5, 2, 0, 0, 6, 0, 10, 6, 2, 2, 3,…
#> $ non_pen_goals        <dbl> 2, 0, 3, 1, 0, 4, 0, 0, 0, 3, 0, 3, 5, 2, 2, 2, 0…
#> $ pens_made            <dbl> 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0…
#> $ pens_att             <dbl> 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0…
#> $ yellow               <dbl> 8, 3, 10, 9, 8, 6, 6, 2, 4, 4, 4, 4, 4, 2, 3, 2, …
#> $ red                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ xg                   <dbl> 1.5, 0.0, 5.9, 0.6, 0.2, 3.0, 0.8, 0.7, 0.5, 3.9,…
#> $ npxg                 <dbl> 1.5, 0.0, 2.7, 0.6, 0.2, 3.0, 0.8, 0.7, 0.5, 3.9,…
#> $ xag                  <dbl> 2.0, 0.0, 3.7, 0.4, 1.0, 4.0, 0.4, 0.0, 0.3, 3.5,…
#> $ npxg_plus_xag        <dbl> 3.5, 0.0, 6.4, 1.0, 1.2, 7.0, 1.3, 0.7, 0.8, 7.4,…
#> $ prog_carries         <dbl> 58, 0, 48, 21, 42, 78, 3, 9, 12, 15, 5, 50, 6, 22…
#> $ prog_passes          <dbl> 121, 0, 159, 120, 94, 90, 32, 34, 58, 27, 45, 85,…
#> $ prog_passes_received <dbl> 79, 0, 114, 14, 71, 202, 3, 1, 4, 76, 0, 97, 37, …
```
