#' Import FBref Defensive Actions (season-level) and standardize schema
#'
#' Imports an FBref "Defensive Actions" table (FBref -> copy/paste into Google
#' Sheets -> export CSV) and returns a cleaned, standardized data frame with one
#' row per player-season.
#'
#' The export usually contains a first row with group labels. Use `header_row`
#' to specify the real header row (typically 2).
#'
#' @param path Path to the CSV file exported from Google Sheets.
#' @param team Team name to attach as context (e.g., "Rosario Central").
#' @param season Season label to attach as context (numeric or character).
#' @param header_row Integer. Row (1-indexed) containing the real headers.
#'   Default is 2 for a Google Sheets export.
#'
#' @return A tibble with standardized defensive action statistics (one row per player-season).
#' @export
import_fbref_defensive_actions <- function(path, team, season, header_row = 2) {
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

  # Read without headers so we can use header_row explicitly
  df_raw <- readr::read_csv(path, col_names = FALSE, show_col_types = FALSE)
  if (nrow(df_raw) < header_row + 1L) {
    stop("Not enough rows for the given `header_row`.", call. = FALSE)
  }

  # Set headers from header_row
  headers <- trimws(as.character(unlist(df_raw[header_row, ])))
  headers[headers == ""] <- NA_character_

  if (!any(tolower(headers) == "player", na.rm = TRUE)) {
    stop("`header_row` does not contain FBref headers (missing 'Player').", call. = FALSE)
  }

  df <- df_raw[(header_row + 1L):nrow(df_raw), , drop = FALSE]
  names(df) <- headers

  # Drop completely empty columns (common with merged cells from Sheets)
  df <- df[, colSums(!is.na(df) & df != "") > 0, drop = FALSE]

  # Defensive Actions table expected columns (your listed schema):
  # Player Nation Pos Age 90s
  # Tackles TklW TklDef3rd TklMid3rd TklAtt3rd
  # DribblersTackled Challenges DribblersTackled% Lost
  # Blocks Sh Blocks Pass
  # Int Tkl+Int Clr Err
  if (ncol(df) < 21) {
    stop(
      "Unexpected number of columns for Defensive Actions. Expected at least 21, got ",
      ncol(df), ".",
      call. = FALSE
    )
  }

  # Keep first 21 columns (ignore trailing Matches/link column if present)
  df <- df[, 1:21, drop = FALSE]

  # Rename by stable position (this avoids issues with weird header text)
  names(df) <- c(
    "player","nation","pos","age","nineties",
    "tackles","tackles_won",
    "tackles_def_3rd","tackles_mid_3rd","tackles_att_3rd",
    "dribblers_tackled","dribbles_challenged","dribblers_tackled_pct","challenges_lost",
    "blocks","shots_blocked","passes_blocked",
    "interceptions","tackles_plus_interceptions","clearances",
    "err"
  )

  # Helpers
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

      tackles = to_num(tackles),
      tackles_won = to_num(tackles_won),

      tackles_def_3rd = to_num(tackles_def_3rd),
      tackles_mid_3rd = to_num(tackles_mid_3rd),
      tackles_att_3rd = to_num(tackles_att_3rd),

      dribblers_tackled = to_num(dribblers_tackled),
      dribbles_challenged = to_num(dribbles_challenged),
      dribblers_tackled_pct = to_num(dribblers_tackled_pct),
      challenges_lost = to_num(challenges_lost),

      blocks = to_num(blocks),
      shots_blocked = to_num(shots_blocked),
      passes_blocked = to_num(passes_blocked),

      interceptions = to_num(interceptions),
      tackles_plus_interceptions = to_num(tackles_plus_interceptions),
      clearances = to_num(clearances),

      errors_leading_to_shot = to_num(err)
    ) |>
    dplyr::filter(!is.na(player) & player != "")

  out
}
