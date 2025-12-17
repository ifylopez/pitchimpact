#' Example FBref Shooting dataset
#'
#' A cleaned and standardized example dataset containing FBref "Shooting" statistics
#' for a single team and season. Each row represents one player-season.
#'
#' This dataset follows the schema produced by \code{import_fbref_shooting()} and is
#' intended for examples, tests, and vignettes.
#'
#' @format A tibble with one row per player-season and 27 columns:
#' \describe{
#'   \item{team}{Team name.}
#'   \item{season}{Season identifier.}
#'   \item{player}{Player name.}
#'   \item{nation_raw}{Original FBref nation cell.}
#'   \item{nation_code}{Three-letter nation code extracted from \code{nation_raw}.}
#'   \item{pos_raw}{Original FBref position string.}
#'   \item{pos_primary}{Primary position.}
#'   \item{pos_secondary}{Secondary position, if present; otherwise NA.}
#'   \item{age}{Player age.}
#'   \item{nineties}{Minutes played divided by 90.}
#'   \item{goals}{Goals scored.}
#'   \item{shots}{Total shots.}
#'   \item{shots_on_target}{Shots on target.}
#'   \item{shots_on_target_pct}{Shots on target percentage.}
#'   \item{shots_per90}{Shots per 90.}
#'   \item{shots_on_target_per90}{Shots on target per 90.}
#'   \item{goals_per_shot}{Goals per shot.}
#'   \item{goals_per_shot_on_target}{Goals per shot on target.}
#'   \item{avg_shot_distance}{Average shot distance (yards).}
#'   \item{shots_free_kicks}{Shots from free kicks.}
#'   \item{pens_made}{Penalty kicks made.}
#'   \item{pens_att}{Penalty kicks attempted.}
#'   \item{xg}{Expected goals (xG).}
#'   \item{npxg}{Non-penalty expected goals (npxG).}
#'   \item{npxg_per_shot}{Non-penalty xG per shot.}
#'   \item{goals_minus_xg}{Goals minus xG.}
#'   \item{non_pen_goals_minus_npxg}{Non-penalty goals minus npxG.}
#' }
#'
#' @source FBref (\url{https://fbref.com})
#'
#' @examples
#' data(shooting_example, package = "pitchimpact")
#' dplyr::select(shooting_example, player, shots, shots_on_target) |> head()
"shooting_example"
