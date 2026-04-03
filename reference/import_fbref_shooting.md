# Import FBref Shooting (season-level) and standardize schema

Imports an FBref "Shooting" table (common workflow: FBref -\> copy/paste
into Google Sheets -\> export CSV) and returns a cleaned, standardized
data frame with one row per player-season.

## Usage

``` r
import_fbref_shooting(path, team, season, header_row = 2)
```

## Arguments

- path:

  Path to the CSV file exported from Google Sheets.

- team:

  Team name to attach as context (e.g., "Rosario Central").

- season:

  Season label to attach as context (numeric or character).

- header_row:

  Integer. The row (1-indexed) containing the real headers. Default is 2
  for a Google Sheets export.

## Value

A tibble with standardized schema (one row per player-season).

## Details

The export often includes a first row with group labels ("Standard",
"Expected") and a trailing "Matches" column. Use `header_row` to specify
which row contains the real headers (typically 2).

Note: Google Sheets may convert the columns "G-xG" and "np:G-xG" into
formulas and export "#ERROR!". This function ignores those raw values
and recomputes:

- goals_minus_xg = goals - xg

- non_pen_goals_minus_npxg = (goals - pens_made) - npxg

## Examples

``` r
if (FALSE) { # \dontrun{
# Example workflow using a CSV exported from FBref via Google Sheets
import_fbref_shooting(
  path = "shooting.csv",
  team = "Rosario Central",
  season = 2023
)
} # }
```
