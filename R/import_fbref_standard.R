#' Import FBref Standard Stats (season-level) and standardize schema
#'
#' Imports an FBref "Standard Stats" table (tailored to the common workflow:
#' FBref table -> copy/paste into Google Sheets -> export as CSV) and returns a
#' cleaned, standardized data frame with one row per player-season.
#'
#' FBref tables pasted into spreadsheets often include extra header rows
#' (group labels such as "Playing Time", "Performance", "Per 90 Minutes") and
#' duplicated statistics (season totals and per-90 values).
#' Use `header_row` to specify which row contains the real column names
#' (e.g., 2 for a Google Sheets export).
#'
#' The function keeps season totals only and removes duplicated per-90 columns.
#' Per-90 metrics should be recomputed later from minutes and nineties.
#'
#' Nationality and position fields are standardized:
#' nationality is split into a raw field (`nation_raw`) and a cleaned
#' three-letter country code (`nation_code`), while positions are parsed into
#' primary and secondary roles (`pos_primary`, `pos_secondary`) when applicable.
#'
#' @param path Path to the CSV file exported from Google Sheets.
#' @param team Team name to attach as context (e.g., "Rosario Central").
#' @param season Season label to attach as context (numeric or character).
#' @param header_row Integer. The row (1-indexed) containing the real headers.
#'   Default is 2 for a Google Sheets export.
#'
#' @return A tibble with one row per player-season and a standardized schema,
#' including cleaned playing time, performance, expected metrics, progression
#' statistics, and normalized nationality and position fields.
#'
#' @export
import_fbref_standard <- function(path, team, season, header_row = 2) {
  if (missing(path) || !is.character(path) || length(path) != 1L) {
    stop("`path` must be a single file path (character).", call. = FALSE)
  }
  if (!file.exists(path)) stop("File does not exist: ", path, call. = FALSE)
  if (missing(team) || is.null(team) || !nzchar(team)) stop("`team` must be a non-empty string.", call. = FALSE)
  if (missing(season) || is.null(season) || (is.character(season) && !nzchar(season))) {
    stop("`season` must be provided (numeric or non-empty string).", call. = FALSE)
  }
  if (!is.numeric(header_row) || length(header_row) != 1L || header_row < 1) {
    stop("`header_row` must be a single positive integer.", call. = FALSE)
  }
  header_row <- as.integer(header_row)

  # Read with no headers; we will set headers from header_row
  df_raw <- readr::read_csv(path, col_names = FALSE, show_col_types = FALSE)
  if (nrow(df_raw) < header_row + 1L) stop("Not enough rows for the given `header_row`.", call. = FALSE)

  new_names <- trimws(as.character(unlist(df_raw[header_row, ])))
  new_names[new_names == ""] <- NA_character_
  if (!any(tolower(new_names) == "player", na.rm = TRUE)) {
    stop("`header_row` does not contain FBref headers (missing 'Player').", call. = FALSE)
  }

  df <- df_raw[(header_row + 1L):nrow(df_raw), , drop = FALSE]
  names(df) <- new_names

  # Drop completely empty columns (common with merged cells from Sheets)
  df <- df[, colSums(!is.na(df) & df != "") > 0, drop = FALSE]

  # Clean names to match what you showed (e.g., 90.0 -> x90; xG -> x_g)
  df <- janitor::clean_names(df)

  # Helper: safe numeric (handles "" and thousands commas)
  to_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x))))

  # Validate required columns exist in THIS schema
  required <- c("player", "nation", "pos", "age", "mp", "starts", "min", "x90")
  missing_req <- setdiff(required, names(df))
  if (length(missing_req) > 0) {
    stop("Missing required columns after cleaning names: ", paste(missing_req, collapse = ", "), call. = FALSE)
  }

  # Keep only the "totals" version of stats:
  # - drop per90 duplicates that end in _2
  # - drop matches link column
  df <- df |>
    dplyr::select(-dplyr::matches("_2$"), -dplyr::any_of("matches"))

  # Build standardized output (schema for your package)
  out <- df |>
    dplyr::transmute(
      team = team,
      season = season,

      player = trimws(as.character(player)),

      # --- Nation cleanup ---
      nation_raw = trimws(as.character(nation)),
      nation_code = dplyr::if_else(
        is.na(nation_raw) | nation_raw == "",
        NA_character_,
        sub(".*\\s+([A-Z]{3})\\s*$", "\\1", nation_raw)
      ),

      # --- Position cleanup ---
      pos_raw = trimws(as.character(pos)),
      pos_primary = dplyr::if_else(
        is.na(pos_raw) | pos_raw == "",
        NA_character_,
        sub("\\s*,.*$", "", pos_raw)
      ),
      pos_secondary = dplyr::if_else(
        is.na(pos_raw) | pos_raw == "" | !grepl(",", pos_raw),
        NA_character_,
        trimws(sub("^.*?,", "", pos_raw))
      ),

      age = to_num(age),
      mp = to_num(mp),
      starts = to_num(starts),
      minutes = to_num(min),
      nineties = to_num(x90),

      goals = to_num(gls),
      assists = to_num(ast),
      goals_assists = to_num(g_a),
      non_pen_goals = to_num(g_pk),
      pens_made = to_num(pk),
      pens_att = to_num(p_katt),

      yellow = to_num(crd_y),
      red = to_num(crd_r),

      xg = to_num(x_g),
      npxg = to_num(npx_g),
      xag = to_num(x_ag),
      npxg_plus_xag = to_num(npx_g_x_ag),

      prog_carries = to_num(prg_c),
      prog_passes = to_num(prg_p),
      prog_passes_received = to_num(prg_r)
    ) |>
    dplyr::filter(!is.na(player) & player != "")

  out
}
