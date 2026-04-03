# Example FBref Defensive Actions dataset

A cleaned and standardized example dataset containing FBref "Defensive
Actions" statistics for a single team and season. Each row represents
one player-season.

## Usage

``` r
defensive_actions_example
```

## Format

A tibble with one row per player-season and 26 columns:

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

- tackles:

  Total tackles.

- tackles_won:

  Tackles won.

- tackles_def_3rd:

  Tackles in defensive third.

- tackles_mid_3rd:

  Tackles in middle third.

- tackles_att_3rd:

  Tackles in attacking third.

- dribblers_tackled:

  Dribblers tackled.

- dribbles_challenged:

  Dribbles challenged.

- dribblers_tackled_pct:

  Percent of dribblers tackled.

- challenges_lost:

  Challenges lost.

- blocks:

  Blocks (total).

- shots_blocked:

  Shots blocked.

- passes_blocked:

  Passes blocked.

- interceptions:

  Interceptions.

- tackles_plus_interceptions:

  Tackles plus interceptions.

- clearances:

  Clearances.

- errors_leading_to_shot:

  Errors leading to an opponent shot.

## Source

FBref (<https://fbref.com>)

## Details

This dataset follows the schema produced by
[`import_fbref_defensive_actions()`](https://ifylopez.github.io/pitchimpact/reference/import_fbref_defensive_actions.md)
and is intended for examples, tests, and vignettes.

## Examples

``` r
data(defensive_actions_example, package = "pitchimpact")
dplyr::select(defensive_actions_example, player, tackles, interceptions) |> head()
#> # A tibble: 6 × 3
#>   player          tackles interceptions
#>   <chr>             <dbl>         <dbl>
#> 1 Agustín Sández       58            38
#> 2 Jorge Broun           0             0
#> 3 Víctor Malcorra      30            13
#> 4 Franco Ibarra       114            32
#> 5 Emanuel Coronel      66            40
#> 6 Jaminton Campaz      44            11
```
