#' Import FBref Shooting (season-level) and standardize schema
#'
#' Imports an FBref "Shooting" table (common workflow: FBref -> copy/paste into
#' Google Sheets -> export CSV) and returns a cleaned, standardized data frame
#' with one row per player-season.
#'
#' The export often includes a first row with group labels ("Standard", "Expected")
#' and a trailing "Matches" column. Use `header_row` to specify which row contains
#' the real headers (typically 2).
#'
#' Note: Google Sheets may convert the columns "G-xG" and "np:G-xG" into formulas
#' and export "#ERROR!". This function ignores those raw values and recomputes:
#' - goals_minus_xg = goals - xg
#' - non_pen_goals_minus_npxg = (goals - pens_made) - npxg
#'
#' @param path Path to the CSV file exported from Google Sheets.
#' @param team Team name to attach as context (e.g., "Rosario Central").
#' @param season Season label to attach as context (numeric or character).
#' @param header_row Integer. The row (1-indexed) containing the real headers.
#'   Default is 2 for a Google Sheets export.
#'
#' @return A tibble with standardized schema (one row per player-season).
#' @export
import_fbref_shooting <- function(path, team, season, header_row = 2) {
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

  # Read with no headers; we'll set them from header_row
  df_raw <- readr::read_csv(path, col_names = FALSE, show_col_types = FALSE)
  if (nrow(df_raw) < header_row + 1L) stop("Not enough rows for the given `header_row`.", call. = FALSE)

  new_names <- trimws(as.character(unlist(df_raw[header_row, ])))
  new_names[new_names == ""] <- NA_character_
  if (!any(tolower(new_names) == "player", na.rm = TRUE)) {
    stop("`header_row` does not contain FBref headers (missing 'Player').", call. = FALSE)
  }

  df <- df_raw[(header_row + 1L):nrow(df_raw), , drop = FALSE]
  names(df) <- new_names

  # Drop completely empty columns (from merged cells in Sheets)
  df <- df[, colSums(!is.na(df) & df != "") > 0, drop = FALSE]

  # Shooting export should have at least 22 meaningful columns + possibly "Matches"
  if (ncol(df) < 22) {
    stop("Unexpected number of columns for Shooting table. Got ", ncol(df), ".", call. = FALSE)
  }

  # Keep first 22 columns (ignore trailing Matches if present)
  df <- df[, 1:22, drop = FALSE]

  # Rename by position (stable FBref Shooting schema for your Sheets export)
  names(df) <- c(
    "player","nation","pos","age","nineties","goals",
    "shots","shots_on_target","shots_on_target_pct",
    "shots_per90","shots_on_target_per90",
    "goals_per_shot","goals_per_shot_on_target",
    "avg_shot_distance","shots_free_kicks",
    "pens_made","pens_att",
    "xg","npxg","npxg_per_shot",
    "g_minus_xg_raw","np_g_minus_xg_raw"
  )

  # Helpers
  to_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
  to_chr <- function(x) trimws(as.character(x))

  out <- df |>
    dplyr::transmute(
      team = team,
      season = season,

      player = to_chr(player),

      # Nationality
      nation_raw = to_chr(nation),
      nation_code = {
        code <- stringr::str_extract(nation_raw, "[A-Z]{3}")
        code
      },

      # Position
      pos_raw = to_chr(pos),
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
      nineties = to_num(nineties),

      goals = to_num(goals),
      shots = to_num(shots),
      shots_on_target = to_num(shots_on_target),
      shots_on_target_pct = to_num(shots_on_target_pct),
      shots_per90 = to_num(shots_per90),
      shots_on_target_per90 = to_num(shots_on_target_per90),

      goals_per_shot = to_num(goals_per_shot),
      goals_per_shot_on_target = to_num(goals_per_shot_on_target),
      avg_shot_distance = to_num(avg_shot_distance),

      shots_free_kicks = to_num(shots_free_kicks),
      pens_made = to_num(pens_made),
      pens_att = to_num(pens_att),

      xg = to_num(xg),
      npxg = to_num(npxg),
      npxg_per_shot = to_num(npxg_per_shot),

      # Recompute columns that often become "#ERROR!" in Sheets exports
      goals_minus_xg = goals - xg,
      non_pen_goals_minus_npxg = (goals - pens_made) - npxg
    ) |>
    dplyr::filter(!is.na(player) & player != "")

  out
}
