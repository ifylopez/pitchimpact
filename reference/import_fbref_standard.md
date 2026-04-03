# Import FBref Standard Stats (season-level) and standardize schema

Imports an FBref "Standard Stats" table (common workflow: FBref -\>
copy/paste into Google Sheets -\> export CSV) and returns a cleaned,
standardized data frame with one row per player-season.

## Usage

``` r
import_fbref_standard(path, team, season, header_row = 2)
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

A tibble with one row per player-season and a standardized schema,
including cleaned playing time, performance, expected metrics,
progression statistics, and normalized nationality and position fields.

## Details

The export often includes a first row with group labels ("Standard",
"Expected") and a trailing "Matches" column. Use `header_row` to specify
which row contains the real headers (typically 2).

The function keeps season totals only and removes duplicated per-90
columns. Per-90 metrics should be recomputed later from minutes and
nineties.

Nationality and position fields are standardized: nationality is split
into a raw field (`nation_raw`) and a cleaned three-letter country code
(`nation_code`), while positions are parsed into primary and secondary
roles (`pos_primary`, `pos_secondary`) when applicable.
