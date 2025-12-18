#' Example FBref Master Player-Season Dataset
#'
#' A cleaned and fully merged example dataset combining multiple FBref tables
#' (Standard Stats, Shooting, Passing, Defensive Actions, and Possession)
#' for a single team and season.
#'
#' Each row represents one player-season. The dataset is produced by
#' \code{merge_fbref_tables()} using standardized inputs from the corresponding
#' \code{import_fbref_*()} functions.
#'
#' The master dataset is intended for end-to-end workflow examples,
#' exploratory analysis, modeling, and visualization.
#'
#' @format A tibble with one row per player-season and 99 columns:
#'
#' \describe{
#'   \item{team}{Team name.}
#'   \item{season}{Season identifier.}
#'   \item{player}{Player name.}
#'
#'   \item{nation_raw}{Original FBref nation cell (e.g., "ar ARG").}
#'   \item{nation_code}{Three-letter nation code extracted from \code{nation_raw}.}
#'
#'   \item{pos_raw}{Original FBref position string (e.g., "MF,FW").}
#'   \item{pos_primary}{Primary playing position.}
#'   \item{pos_secondary}{Secondary playing position, if available; otherwise NA.}
#'
#'   \item{age}{Player age.}
#'   \item{mp}{Matches played.}
#'   \item{starts}{Games started.}
#'   \item{minutes}{Minutes played.}
#'   \item{nineties}{Minutes played divided by 90.}
#'
#'   \item{goals}{Goals scored.}
#'   \item{assists}{Assists.}
#'   \item{goals_assists}{Goals plus assists.}
#'   \item{non_pen_goals}{Non-penalty goals.}
#'   \item{pens_made}{Penalty kicks made.}
#'   \item{pens_att}{Penalty kicks attempted.}
#'
#'   \item{yellow}{Yellow cards.}
#'   \item{red}{Red cards.}
#'
#'   \item{xg}{Expected goals (xG).}
#'   \item{npxg}{Non-penalty expected goals (npxG).}
#'   \item{xag}{Expected assisted goals (xAG).}
#'   \item{npxg_plus_xag}{Non-penalty xG plus xAG.}
#'
#'   \item{prog_carries}{Progressive carries.}
#'   \item{prog_passes}{Progressive passes.}
#'   \item{prog_passes_received}{Progressive passes received (Standard table).}
#'
#'   \item{shots}{Total shots.}
#'   \item{shots_on_target}{Shots on target.}
#'   \item{shots_on_target_pct}{Shots on target percentage.}
#'   \item{shots_per90}{Shots per 90 minutes.}
#'   \item{shots_on_target_per90}{Shots on target per 90 minutes.}
#'   \item{goals_per_shot}{Goals per shot.}
#'   \item{goals_per_shot_on_target}{Goals per shot on target.}
#'   \item{avg_shot_distance}{Average shot distance (yards).}
#'   \item{shots_free_kicks}{Shots from free kicks.}
#'   \item{npxg_per_shot}{Non-penalty xG per shot.}
#'   \item{goals_minus_xg}{Goals minus xG.}
#'   \item{non_pen_goals_minus_npxg}{Non-penalty goals minus npxG.}
#'
#'   \item{passes_completed}{Passes completed.}
#'   \item{passes_attempted}{Passes attempted.}
#'   \item{pass_completion_pct}{Pass completion percentage.}
#'   \item{total_pass_distance}{Total passing distance (yards).}
#'   \item{prog_pass_distance}{Progressive passing distance (yards).}
#'
#'   \item{short_completed}{Short passes completed (5–15 yards).}
#'   \item{short_attempted}{Short passes attempted (5–15 yards).}
#'   \item{short_completion_pct}{Short pass completion percentage.}
#'
#'   \item{medium_completed}{Medium passes completed (15–30 yards).}
#'   \item{medium_attempted}{Medium passes attempted (15–30 yards).}
#'   \item{medium_completion_pct}{Medium pass completion percentage.}
#'
#'   \item{long_completed}{Long passes completed (30+ yards).}
#'   \item{long_attempted}{Long passes attempted (30+ yards).}
#'   \item{long_completion_pct}{Long pass completion percentage.}
#'
#'   \item{xa}{Expected assists (xA).}
#'   \item{assists_minus_xag}{Assists minus xAG.}
#'   \item{key_passes}{Key passes.}
#'   \item{passes_final_third}{Passes into the final third.}
#'   \item{passes_penalty_area}{Passes into the penalty area.}
#'   \item{crosses_penalty_area}{Crosses into the penalty area.}
#'   \item{progressive_passes}{Progressive passes (Passing table).}
#'
#'   \item{tackles}{Total tackles.}
#'   \item{tackles_won}{Tackles won.}
#'   \item{tackles_def_3rd}{Tackles in defensive third.}
#'   \item{tackles_mid_3rd}{Tackles in middle third.}
#'   \item{tackles_att_3rd}{Tackles in attacking third.}
#'
#'   \item{dribblers_tackled}{Dribblers tackled.}
#'   \item{dribbles_challenged}{Dribbles challenged.}
#'   \item{dribblers_tackled_pct}{Percentage of dribblers tackled.}
#'   \item{challenges_lost}{Challenges lost.}
#'
#'   \item{blocks}{Total blocks.}
#'   \item{shots_blocked}{Shots blocked.}
#'   \item{passes_blocked}{Passes blocked.}
#'   \item{interceptions}{Interceptions.}
#'   \item{tackles_plus_interceptions}{Tackles plus interceptions.}
#'   \item{clearances}{Clearances.}
#'   \item{errors_leading_to_shot}{Errors leading to an opponent shot.}
#'
#'   \item{touches}{Total touches.}
#'   \item{touches_def_pen}{Touches in defensive penalty area.}
#'   \item{touches_def_3rd}{Touches in defensive third.}
#'   \item{touches_mid_3rd}{Touches in middle third.}
#'   \item{touches_att_3rd}{Touches in attacking third.}
#'   \item{touches_att_pen}{Touches in attacking penalty area.}
#'   \item{touches_live}{Live-ball touches.}
#'
#'   \item{take_ons_att}{Take-ons attempted.}
#'   \item{take_ons_succ}{Successful take-ons.}
#'   \item{take_ons_succ_pct}{Successful take-on percentage.}
#'   \item{take_ons_tackled}{Times tackled during take-ons.}
#'   \item{take_ons_tackled_pct}{Tackled during take-on percentage.}
#'
#'   \item{carries}{Carries.}
#'   \item{carry_total_dist}{Total carrying distance (yards).}
#'   \item{carry_prg_dist}{Progressive carrying distance (yards).}
#'   \item{carry_prg_c}{Progressive carries.}
#'   \item{carries_final_third}{Carries into final third.}
#'   \item{carries_pen_area}{Carries into penalty area.}
#'
#'   \item{miscontrols}{Miscontrols.}
#'   \item{dispossessed}{Dispossessed.}
#'   \item{passes_received}{Passes received.}
#'   \item{prog_passes_received_possession}{Progressive passes received (Possession table).}
#' }
#'
#' @source
#' FBref (\url{https://fbref.com})
#'
#' @examples
#' data(master_example, package = "pitchimpact")
#' dplyr::glimpse(master_example)
"master_example"
