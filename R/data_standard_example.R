#' Example FBref Standard Stats dataset
#'
#' A cleaned and standardized example dataset containing FBref "Standard Stats"
#' for a single team and season. Each row represents one player-season.
#'
#' This dataset follows the schema produced by \code{import_fbref_standard()} and is
#' intended for examples, tests, and vignettes.
#'
#' @format A tibble with one row per player-season and 28 columns:
#' \describe{
#'   \item{team}{Team name.}
#'   \item{season}{Season identifier.}
#'   \item{player}{Player name.}
#'   \item{nation_raw}{Original FBref nation cell (e.g., "ar ARG").}
#'   \item{nation_code}{Three-letter nation code extracted from \code{nation_raw}.}
#'   \item{pos_raw}{Original FBref position string (e.g., "MF,FW").}
#'   \item{pos_primary}{Primary position (before comma).}
#'   \item{pos_secondary}{Secondary position (after comma), if present; otherwise NA.}
#'   \item{age}{Player age.}
#'   \item{mp}{Matches played.}
#'   \item{starts}{Games started.}
#'   \item{minutes}{Minutes played.}
#'   \item{nineties}{Minutes played divided by 90.}
#'   \item{goals}{Goals scored.}
#'   \item{assists}{Assists.}
#'   \item{goals_assists}{Goals + assists.}
#'   \item{non_pen_goals}{Non-penalty goals.}
#'   \item{pens_made}{Penalty kicks made.}
#'   \item{pens_att}{Penalty kicks attempted.}
#'   \item{yellow}{Yellow cards.}
#'   \item{red}{Red cards.}
#'   \item{xg}{Expected goals (xG).}
#'   \item{npxg}{Non-penalty expected goals (npxG).}
#'   \item{xag}{Expected assisted goals (xAG).}
#'   \item{npxg_plus_xag}{Non-penalty xG plus xAG.}
#'   \item{prog_carries}{Progressive carries.}
#'   \item{prog_passes}{Progressive passes.}
#'   \item{prog_passes_received}{Progressive passes received.}
#' }
#'
#' @source FBref (\url{https://fbref.com})
#'
#' @examples
#' data(standard_example, package = "pitchimpact")
#' dplyr::glimpse(standard_example)
"standard_example"
