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
