# Example FBref Shooting dataset

A cleaned and standardized example dataset containing FBref "Shooting"
statistics for a single team and season. Each row represents one
player-season.

## Usage

``` r
shooting_example
```

## Format

A tibble with one row per player-season and 27 columns:

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

- goals:

  Goals scored.

- shots:

  Total shots.

- shots_on_target:

  Shots on target.

- shots_on_target_pct:

  Shots on target percentage.

- shots_per90:

  Shots per 90.

- shots_on_target_per90:

  Shots on target per 90.

- goals_per_shot:

  Goals per shot.

- goals_per_shot_on_target:

  Goals per shot on target.

- avg_shot_distance:

  Average shot distance (yards).

- shots_free_kicks:

  Shots from free kicks.

- pens_made:

  Penalty kicks made.

- pens_att:

  Penalty kicks attempted.

- xg:

  Expected goals (xG).

- npxg:

  Non-penalty expected goals (npxG).

- npxg_per_shot:

  Non-penalty xG per shot.

- goals_minus_xg:

  Goals minus xG.

- non_pen_goals_minus_npxg:

  Non-penalty goals minus npxG.

## Source

FBref (<https://fbref.com>)

## Details

This dataset follows the schema produced by
[`import_fbref_shooting()`](https://ifylopez.github.io/pitchimpact/reference/import_fbref_shooting.md)
and is intended for examples, tests, and vignettes.

## Examples

``` r
data(shooting_example, package = "pitchimpact")
dplyr::select(shooting_example, player, shots, shots_on_target) |> head()
#> # A tibble: 6 × 3
#>   player          shots shots_on_target
#>   <chr>           <dbl>           <dbl>
#> 1 Agustín Sández     16               5
#> 2 Jorge Broun         0               0
#> 3 Víctor Malcorra    31              11
#> 4 Franco Ibarra      19               5
#> 5 Emanuel Coronel     7               1
#> 6 Jaminton Campaz    46              18
```
