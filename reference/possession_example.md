# Example FBref Possession dataset

A cleaned and standardized example dataset containing FBref "Possession"
statistics for a single team and season. Each row represents one
player-season.

## Usage

``` r
possession_example
```

## Format

A tibble with one row per player-season and 32 columns:

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

- touches:

  Total touches.

- touches_def_pen:

  Touches in defensive penalty area.

- touches_def_3rd:

  Touches in defensive third.

- touches_mid_3rd:

  Touches in middle third.

- touches_att_3rd:

  Touches in attacking third.

- touches_att_pen:

  Touches in attacking penalty area.

- touches_live:

  Live-ball touches.

- take_ons_att:

  Take-ons attempted.

- take_ons_succ:

  Successful take-ons.

- take_ons_succ_pct:

  Successful take-on percentage.

- take_ons_tackled:

  Times tackled during take-on attempts.

- take_ons_tackled_pct:

  Tackled during take-on percentage.

- carries:

  Carries.

- carry_total_dist:

  Total carrying distance (yards).

- carry_prg_dist:

  Progressive carrying distance (yards).

- carry_prg_c:

  Progressive carries.

- carries_final_third:

  Carries into final third.

- carries_pen_area:

  Carries into penalty area.

- miscontrols:

  Miscontrols.

- dispossessed:

  Dispossessed.

- passes_received:

  Passes received.

- prog_passes_received:

  Progressive passes received (possession table).

## Source

FBref (<https://fbref.com>)

## Details

This dataset follows the schema produced by
[`import_fbref_possession()`](https://ifylopez.github.io/pitchimpact/reference/import_fbref_possession.md)
and is intended for examples, tests, and vignettes.

## Examples

``` r
data("possession_example", package = "pitchimpact")
dplyr::select(possession_example, player, touches, carries) |> head()
#> # A tibble: 6 × 3
#>   player          touches carries
#>   <chr>             <dbl>   <dbl>
#> 1 Agustín Sández     1680     733
#> 2 Jorge Broun         824     362
#> 3 Víctor Malcorra    1467     808
#> 4 Franco Ibarra      1471     648
#> 5 Emanuel Coronel    1403     592
#> 6 Jaminton Campaz    1065     656
```
