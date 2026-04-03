# Import FBref Defensive Actions (season-level) and standardize schema

Imports an FBref "Defensive Actions" table (FBref -\> copy/paste into
Google Sheets -\> export CSV) and returns a cleaned, standardized data
frame with one row per player-season.

## Usage

``` r
import_fbref_defensive_actions(path, team, season, header_row = 2)
```

## Arguments

- path:

  Path to the CSV file exported from Google Sheets.

- team:

  Team name to attach as context (e.g., "Rosario Central").

- season:

  Season label to attach as context (numeric or character).

- header_row:

  Integer. Row (1-indexed) containing the real headers. Default is 2 for
  a Google Sheets export.

## Value

A tibble with standardized defensive action statistics (one row per
player-season).

## Details

The export usually contains a first row with group labels. Use
`header_row` to specify the real header row (typically 2).

## Examples

``` r
if (FALSE) { # \dontrun{
# Example workflow using a CSV exported from FBref via Google Sheets
import_fbref_defensive_actions(
  path = "defensive_actions.csv",
  team = "Rosario Central",
  season = 2023
)
} # }
```
