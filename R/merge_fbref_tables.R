#' Merge FBref tables into a player-season master table
#'
#' Joins standardized FBref tables into a single player-season dataset keyed by
#' team, season, and player. When `standard` is provided, overlapping columns
#' from other tables are joined with table-specific suffixes (e.g. `_shooting`)
#' and then dropped, keeping the `standard` version as source of truth.
#'
#' @param standard Tibble from `import_fbref_standard()`. Recommended.
#' @param shooting Tibble from `import_fbref_shooting()`. Optional.
#' @param passing Tibble from `import_fbref_passing()`. Optional.
#' @param defensive Tibble from `import_fbref_defensive_actions()`. Optional.
#' @param possession Tibble from `import_fbref_possession()`. Optional.
#' @param key Character vector of join keys. Default: c("team","season","player").
#' @param strict If TRUE, stops when duplicated keys are detected within any input table.
#'
#' @return A tibble with one row per player-season and columns from all provided tables.
#' @export
merge_fbref_tables <- function(
    standard = NULL,
    shooting = NULL,
    passing = NULL,
    defensive = NULL,
    possession = NULL,
    key = c("team", "season", "player"),
    strict = TRUE
) {

  tables <- list(
    standard = standard,
    shooting = shooting,
    passing = passing,
    defensive = defensive,
    possession = possession
  )

  tables <- tables[!vapply(tables, is.null, logical(1))]
  if (length(tables) == 0) stop("Provide at least one table to merge.", call. = FALSE)

  # ---- helpers --------------------------------------------------------------
  normalize <- function(df, name) {
    missing_key <- setdiff(key, names(df))
    if (length(missing_key) > 0) {
      stop("Table `", name, "` is missing key columns: ", paste(missing_key, collapse = ", "), call. = FALSE)
    }

    df <- df |>
      dplyr::mutate(
        team = as.character(.data$team),
        season = .data$season,
        player = trimws(as.character(.data$player))
      )

    dup <- df |>
      dplyr::count(dplyr::across(dplyr::all_of(key))) |>
      dplyr::filter(.data$n > 1)

    if (nrow(dup) > 0) {
      msg <- paste0(
        "Table `", name, "` has duplicated keys (", paste(key, collapse = ", "), ").\n",
        paste(utils::capture.output(print(utils::head(dup, 10))), collapse = "\n")
      )
      if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
    }

    df
  }

  tables <- Map(normalize, tables, names(tables))

  # Overlapping columns we want to KEEP from `standard` (drop from other tables)
  overlap_keep_standard <- c(
    # metadata
    "nation_raw","nation_code","pos_raw","pos_primary","pos_secondary","age","nineties",
    # playing time context (standard is the authoritative one)
    "mp","starts","minutes",
    # common scoring/expected columns that appear in multiple FBref tables
    "goals","assists","xg","npxg","xag","pens_made","pens_att"
  )

  # Start from standard if available (best practice)
  if (!is.null(tables$standard)) {
    joined <- tables$standard
    tables$standard <- NULL
  } else {
    # otherwise start from the first provided table
    nm <- names(tables)[1]
    joined <- tables[[1]]
    tables[[1]] <- NULL
  }

  # Sequential full joins:
  # any collisions get a suffix like _shooting, _passing, etc.
  for (nm in names(tables)) {
    joined <- dplyr::full_join(
      joined,
      tables[[nm]],
      by = key,
      suffix = c("", paste0("_", nm))
    )

    # If we have a standard base, drop the duplicated columns from this table
    # (only those that actually collided and therefore got the suffix)
    if (!is.null(standard)) {
      to_drop <- paste0(overlap_keep_standard, "_", nm)
      to_drop <- to_drop[to_drop %in% names(joined)]
      if (length(to_drop) > 0) {
        joined <- dplyr::select(joined, -dplyr::all_of(to_drop))
      }
    }
  }

  joined
}
