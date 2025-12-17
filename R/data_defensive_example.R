#' Example FBref Defensive Actions dataset
#'
#' A cleaned and standardized example dataset containing FBref "Defensive Actions"
#' statistics for a single team and season. Each row represents one player-season.
#'
#' This dataset follows the schema produced by \code{import_fbref_defensive_actions()}
#' and is intended for examples, tests, and vignettes.
#'
#' @format A tibble with one row per player-season and 26 columns:
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
#'   \item{tackles}{Total tackles.}
#'   \item{tackles_won}{Tackles won.}
#'   \item{tackles_def_3rd}{Tackles in defensive third.}
#'   \item{tackles_mid_3rd}{Tackles in middle third.}
#'   \item{tackles_att_3rd}{Tackles in attacking third.}
#'   \item{dribblers_tackled}{Dribblers tackled.}
#'   \item{dribbles_challenged}{Dribbles challenged.}
#'   \item{dribblers_tackled_pct}{Percent of dribblers tackled.}
#'   \item{challenges_lost}{Challenges lost.}
#'   \item{blocks}{Blocks (total).}
#'   \item{shots_blocked}{Shots blocked.}
#'   \item{passes_blocked}{Passes blocked.}
#'   \item{interceptions}{Interceptions.}
#'   \item{tackles_plus_interceptions}{Tackles plus interceptions.}
#'   \item{clearances}{Clearances.}
#'   \item{errors_leading_to_shot}{Errors leading to an opponent shot.}
#' }
#'
#' @source FBref (\url{https://fbref.com})
#'
#' @examples
#' data(defensive_actions_example, package = "pitchimpact")
#' dplyr::select(defensive_actions_example, player, tackles, interceptions) |> head()
"defensive_actions_example"

