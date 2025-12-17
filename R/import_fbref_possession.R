#' Import FBref Possession (season-level) and standardize schema
#'
#' Imports an FBref "Possession" table (FBref -> copy/paste into Google Sheets ->
#' export CSV) and returns a cleaned, standardized data frame with one row per
#' player-season.
#'
#' The export includes a first row with group labels; `header_row` specifies the
#' row containing the true headers (typically 2). This function keeps the season
#' totals columns present in the Sheets export and ignores the trailing "Matches"
#' column.
#'
#' @param path Path to the CSV file exported from Google Sheets.
#' @param team Team name to attach as context (e.g., "Rosario Central").
#' @param season Season label to attach as context (numeric or character).
#' @param header_row Integer. Row (1-indexed) containing the real headers.
#'   Default is 2 for a Google Sheets export.
#'
#' @return A tibble with standardized possession statistics (one row per player-season).
#' @export
import_fbref_possession <- function(path, team, season, header_row = 2) {
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

  df_raw <- readr::read_csv(path, col_names = FALSE, show_col_types = FALSE)
  if (nrow(df_raw) < header_row + 1L) stop("Not enough rows for the given `header_row`.", call. = FALSE)

  headers <- trimws(as.character(unlist(df_raw[header_row, ])))
  headers[headers == ""] <- NA_character_
  if (!any(tolower(headers) == "player", na.rm = TRUE)) {
    stop("`header_row` does not contain FBref headers (missing 'Player').", call. = FALSE)
  }

  df <- df_raw[(header_row + 1L):nrow(df_raw), , drop = FALSE]
  names(df) <- headers

  # drop empty columns created by merged cells
  df <- df[, colSums(!is.na(df) & df != "") > 0, drop = FALSE]

  # Your Possession export has exactly 28 columns (including trailing Matches)
  if (ncol(df) < 28) {
    stop("Unexpected number of columns for Possession table. Expected 28, got ", ncol(df), ".", call. = FALSE)
  }
  df <- df[, 1:28, drop = FALSE]

  # Rename by stable position in YOUR export (matches your `new_names`)
  names(df) <- c(
    "player","nation","pos","age","nineties",
    "touches","touches_def_pen","touches_def_3rd","touches_mid_3rd","touches_att_3rd",
    "touches_att_pen","touches_live",
    "take_ons_att","take_ons_succ","take_ons_succ_pct","take_ons_tackled","take_ons_tackled_pct",
    "carries","carry_total_dist","carry_prg_dist","carry_prg_c",
    "carries_final_third","carries_pen_area",
    "miscontrols","dispossessed",
    "passes_received","prog_passes_received",
    "matches"
  )

  # drop trailing matches column
  df <- dplyr::select(df, -matches)

  to_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
  to_chr <- function(x) trimws(as.character(x))

  out <- df |>
    dplyr::transmute(
      team = team,
      season = season,

      player = to_chr(player),

      nation_raw = to_chr(nation),
      nation_code = stringr::str_extract(nation_raw, "[A-Z]{3}"),

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

      touches = to_num(touches),
      touches_def_pen = to_num(touches_def_pen),
      touches_def_3rd = to_num(touches_def_3rd),
      touches_mid_3rd = to_num(touches_mid_3rd),
      touches_att_3rd = to_num(touches_att_3rd),
      touches_att_pen = to_num(touches_att_pen),
      touches_live = to_num(touches_live),

      take_ons_att = to_num(take_ons_att),
      take_ons_succ = to_num(take_ons_succ),
      take_ons_succ_pct = to_num(take_ons_succ_pct),
      take_ons_tackled = to_num(take_ons_tackled),
      take_ons_tackled_pct = to_num(take_ons_tackled_pct),

      carries = to_num(carries),
      carry_total_dist = to_num(carry_total_dist),
      carry_prg_dist = to_num(carry_prg_dist),
      carry_prg_c = to_num(carry_prg_c),
      carries_final_third = to_num(carries_final_third),
      carries_pen_area = to_num(carries_pen_area),

      miscontrols = to_num(miscontrols),
      dispossessed = to_num(dispossessed),

      passes_received = to_num(passes_received),
      prog_passes_received = to_num(prog_passes_received)
    ) |>
    dplyr::filter(!is.na(player) & player != "")

  out
}
