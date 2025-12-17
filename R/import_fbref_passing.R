#' Import FBref Passing (season-level) and standardize schema
#'
#' Imports an FBref "Passing" table (FBref -> copy/paste into Google Sheets -> CSV)
#' and returns a cleaned, standardized data frame with one row per player-season.
#'
#' The export usually contains a first row with group labels and duplicated or
#' formula-broken columns. Use `header_row` to specify the real header row
#' (typically 2 for Google Sheets exports).
#'
#' Google Sheets may convert the column "Assists minus expected goals assisted"
#' into a formula and export "#ERROR!". This function ignores the raw value and
#' recomputes it as: assists - xag.
#'
#' @param path Path to the CSV file exported from Google Sheets.
#' @param team Team name to attach as context (e.g., "Rosario Central").
#' @param season Season label to attach as context (numeric or character).
#' @param header_row Integer. Row (1-indexed) containing the real headers.
#'   Default is 2 for a Google Sheets export.
#'
#' @return A tibble with standardized passing statistics (one row per player-season).
#' @export
import_fbref_passing <- function(path, team, season, header_row = 2) {

  # ---- checks ---------------------------------------------------------------
  if (!file.exists(path)) stop("File does not exist: ", path, call. = FALSE)
  if (missing(team) || !nzchar(team)) stop("`team` must be a non-empty string.", call. = FALSE)
  if (missing(season)) stop("`season` must be provided.", call. = FALSE)

  # ---- read raw -------------------------------------------------------------
  df_raw <- readr::read_csv(path, col_names = FALSE, show_col_types = FALSE)
  if (nrow(df_raw) < header_row + 1L) stop("Not enough rows for given `header_row`.", call. = FALSE)

  headers <- trimws(as.character(unlist(df_raw[header_row, ])))
  headers[headers == ""] <- NA_character_

  if (!any(tolower(headers) == "player", na.rm = TRUE)) {
    stop("`header_row` does not contain FBref headers (missing 'Player').", call. = FALSE)
  }

  df <- df_raw[(header_row + 1L):nrow(df_raw), , drop = FALSE]
  names(df) <- headers

  # drop empty columns
  df <- df[, colSums(!is.na(df) & df != "") > 0, drop = FALSE]

  # ---- keep expected FBref Passing columns by position ----------------------
  # Stable for your Sheets export (up to Progressive Passes)
  df <- df[, 1:28, drop = FALSE]

  names(df) <- c(
    "player","nation","pos","age","nineties",
    "passes_completed","passes_attempted","pass_completion_pct",
    "total_pass_distance","prog_pass_distance",
    "short_completed","short_attempted","short_completion_pct",
    "medium_completed","medium_attempted","medium_completion_pct",
    "long_completed","long_attempted","long_completion_pct",
    "assists","xag","xa","assists_minus_xag_raw",
    "key_passes","passes_final_third","passes_penalty_area",
    "crosses_penalty_area","progressive_passes"
  )

  # ---- helpers --------------------------------------------------------------
  to_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
  to_chr <- function(x) trimws(as.character(x))

  # ---- build output ---------------------------------------------------------
  out <- df |>
    dplyr::transmute(
      team = team,
      season = season,

      player = to_chr(player),

      # nationality
      nation_raw = to_chr(nation),
      nation_code = stringr::str_extract(nation_raw, "[A-Z]{3}"),

      # position
      pos_raw = to_chr(pos),
      pos_primary = dplyr::if_else(
        is.na(pos_raw) | pos_raw == "",
        NA_character_,
        sub("\\s*,.*$", "", pos_raw)
      ),
      pos_secondary = dplyr::if_else(
        is.na(pos_raw) | !grepl(",", pos_raw),
        NA_character_,
        trimws(sub("^.*?,", "", pos_raw))
      ),

      age = to_num(age),
      nineties = to_num(nineties),

      passes_completed = to_num(passes_completed),
      passes_attempted = to_num(passes_attempted),
      pass_completion_pct = to_num(pass_completion_pct),

      total_pass_distance = to_num(total_pass_distance),
      prog_pass_distance = to_num(prog_pass_distance),

      short_completed = to_num(short_completed),
      short_attempted = to_num(short_attempted),
      short_completion_pct = to_num(short_completion_pct),

      medium_completed = to_num(medium_completed),
      medium_attempted = to_num(medium_attempted),
      medium_completion_pct = to_num(medium_completion_pct),

      long_completed = to_num(long_completed),
      long_attempted = to_num(long_attempted),
      long_completion_pct = to_num(long_completion_pct),

      assists = to_num(assists),
      xag = to_num(xag),
      xa = to_num(xa),

      # recomputed column (Sheets-safe)
      assists_minus_xag = assists - xag,

      key_passes = to_num(key_passes),
      passes_final_third = to_num(passes_final_third),
      passes_penalty_area = to_num(passes_penalty_area),
      crosses_penalty_area = to_num(crosses_penalty_area),
      progressive_passes = to_num(progressive_passes)
    ) |>
    dplyr::filter(!is.na(player) & player != "")

  out
}
