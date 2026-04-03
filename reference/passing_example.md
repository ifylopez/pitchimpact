# Example FBref Passing dataset

A cleaned and standardized example dataset containing FBref "Passing"
statistics for a single team and season. Each row represents one
player-season.

## Usage

``` r
passing_example
```

## Format

A tibble with one row per player-season and 33 columns:

- team:

  Team name.

- season:

  Season identifier.

- player:

  Player name.

- nation_raw:

  Original FBref nation cell.

- nation_code:

  Three-letter nation code extracted from `nation_raw`.

- pos_raw:

  Original FBref position string.

- pos_primary:

  Primary position.

- pos_secondary:

  Secondary position, if present; otherwise NA.

- age:

  Player age.

- nineties:

  Minutes played divided by 90.

- passes_completed:

  Passes completed.

- passes_attempted:

  Passes attempted.

- pass_completion_pct:

  Pass completion percentage.

- total_pass_distance:

  Total passing distance (yards).

- prog_pass_distance:

  Progressive passing distance (yards).

- short_completed:

  Short passes completed (5–15 yards).

- short_attempted:

  Short passes attempted (5–15 yards).

- short_completion_pct:

  Short pass completion percentage (5–15 yards).

- medium_completed:

  Medium passes completed (15–30 yards).

- medium_attempted:

  Medium passes attempted (15–30 yards).

- medium_completion_pct:

  Medium pass completion percentage (15–30 yards).

- long_completed:

  Long passes completed (30+ yards).

- long_attempted:

  Long passes attempted (30+ yards).

- long_completion_pct:

  Long pass completion percentage (30+ yards).

- assists:

  Assists.

- xag:

  Expected assisted goals (xAG).

- xa:

  Expected assists (xA).

- assists_minus_xag:

  Assists minus xAG.

- key_passes:

  Key passes.

- passes_final_third:

  Passes into final third.

- passes_penalty_area:

  Passes into penalty area.

- crosses_penalty_area:

  Crosses into penalty area.

- progressive_passes:

  Progressive passes.

## Source

FBref (<https://fbref.com>)

## Details

This dataset follows the schema produced by
[`import_fbref_passing()`](https://ifylopez.github.io/pitchimpact/reference/import_fbref_passing.md)
and is intended for examples, tests, and vignettes.

## Examples

``` r
data("passing_example", package = "pitchimpact")
dplyr::select(passing_example, player, passes_completed, total_pass_distance) |> head()
#> # A tibble: 6 × 3
#>   player          passes_completed total_pass_distance
#>   <chr>                      <dbl>               <dbl>
#> 1 Agustín Sández               992               16479
#> 2 Jorge Broun                  607               17973
#> 3 Víctor Malcorra              863               17414
#> 4 Franco Ibarra                874               16987
#> 5 Emanuel Coronel              814               13602
#> 6 Jaminton Campaz              517                8645
```
