# Import FBref Possession (season-level) and standardize schema

Imports an FBref "Possession" table (FBref -\> copy/paste into Google
Sheets -\> export CSV) and returns a cleaned, standardized data frame
with one row per player-season.

## Usage

``` r
import_fbref_possession(path, team, season, header_row = 2)
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

A tibble with standardized possession statistics (one row per
player-season).

## Details

The export includes a first row with group labels; `header_row`
specifies the row containing the true headers (typically 2). This
function keeps the season totals columns present in the Sheets export
and ignores the trailing "Matches" column.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example workflow using a CSV exported from FBref via Google Sheets
import_fbref_possession(
  path = "possession.csv",
  team = "Rosario Central",
  season = 2023
)
} # }
```
