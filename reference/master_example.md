# Example FBref Master Player-Season Dataset

A cleaned and fully merged example dataset combining multiple FBref
tables (Standard Stats, Shooting, Passing, Defensive Actions, and
Possession) for a single team and season.

## Usage

``` r
master_example
```

## Format

A tibble with one row per player-season and 99 columns:

- team:

  Team name.

- season:

  Season identifier.

- player:

  Player name.

- nation_raw:

  Original FBref nation cell (e.g., "ar ARG").

- nation_code:

  Three-letter nation code extracted from `nation_raw`.

- pos_raw:

  Original FBref position string (e.g., "MF,FW").

- pos_primary:

  Primary playing position.

- pos_secondary:

  Secondary playing position, if available; otherwise NA.

- age:

  Player age.

- mp:

  Matches played.

- starts:

  Games started.

- minutes:

  Minutes played.

- nineties:

  Minutes played divided by 90.

- goals:

  Goals scored.

- assists:

  Assists.

- goals_assists:

  Goals plus assists.

- non_pen_goals:

  Non-penalty goals.

- pens_made:

  Penalty kicks made.

- pens_att:

  Penalty kicks attempted.

- yellow:

  Yellow cards.

- red:

  Red cards.

- xg:

  Expected goals (xG).

- npxg:

  Non-penalty expected goals (npxG).

- xag:

  Expected assisted goals (xAG).

- npxg_plus_xag:

  Non-penalty xG plus xAG.

- prog_carries:

  Progressive carries.

- prog_passes:

  Progressive passes.

- prog_passes_received:

  Progressive passes received (Standard table).

- shots:

  Total shots.

- shots_on_target:

  Shots on target.

- shots_on_target_pct:

  Shots on target percentage.

- shots_per90:

  Shots per 90 minutes.

- shots_on_target_per90:

  Shots on target per 90 minutes.

- goals_per_shot:

  Goals per shot.

- goals_per_shot_on_target:

  Goals per shot on target.

- avg_shot_distance:

  Average shot distance (yards).

- shots_free_kicks:

  Shots from free kicks.

- npxg_per_shot:

  Non-penalty xG per shot.

- goals_minus_xg:

  Goals minus xG.

- non_pen_goals_minus_npxg:

  Non-penalty goals minus npxG.

- passes_completed:

  Passes completed.

- passes_attempted:

  Passes attempted.

- pass_completion_pct:

  Pass completion percentage.

- total_pass_distance:

  Total passing distance (yards).

- prog_pass_distance:

  Progressive passing distance (yards).

- short_completed:

  Short passes completed (5–15 yards).

- short_attempted:

  Short passes attempted (5–15 yards).

- short_completion_pct:

  Short pass completion percentage.

- medium_completed:

  Medium passes completed (15–30 yards).

- medium_attempted:

  Medium passes attempted (15–30 yards).

- medium_completion_pct:

  Medium pass completion percentage.

- long_completed:

  Long passes completed (30+ yards).

- long_attempted:

  Long passes attempted (30+ yards).

- long_completion_pct:

  Long pass completion percentage.

- xa:

  Expected assists (xA).

- assists_minus_xag:

  Assists minus xAG.

- key_passes:

  Key passes.

- passes_final_third:

  Passes into the final third.

- passes_penalty_area:

  Passes into the penalty area.

- crosses_penalty_area:

  Crosses into the penalty area.

- progressive_passes:

  Progressive passes (Passing table).

- tackles:

  Total tackles.

- tackles_won:

  Tackles won.

- tackles_def_3rd:

  Tackles in defensive third.

- tackles_mid_3rd:

  Tackles in middle third.

- tackles_att_3rd:

  Tackles in attacking third.

- dribblers_tackled:

  Dribblers tackled.

- dribbles_challenged:

  Dribbles challenged.

- dribblers_tackled_pct:

  Percentage of dribblers tackled.

- challenges_lost:

  Challenges lost.

- blocks:

  Total blocks.

- shots_blocked:

  Shots blocked.

- passes_blocked:

  Passes blocked.

- interceptions:

  Interceptions.

- tackles_plus_interceptions:

  Tackles plus interceptions.

- clearances:

  Clearances.

- errors_leading_to_shot:

  Errors leading to an opponent shot.

- touches:

  Total touches.

- touches_def_pen:

  Touches in defensive penalty area.

- touches_def_3rd:

  Touches in defensive third.

- touches_mid_3rd:

  Touches in middle third.

- touches_att_3rd:

  Touches in attacking third.

- touches_att_pen:

  Touches in attacking penalty area.

- touches_live:

  Live-ball touches.

- take_ons_att:

  Take-ons attempted.

- take_ons_succ:

  Successful take-ons.

- take_ons_succ_pct:

  Successful take-on percentage.

- take_ons_tackled:

  Times tackled during take-ons.

- take_ons_tackled_pct:

  Tackled during take-on percentage.

- carries:

  Carries.

- carry_total_dist:

  Total carrying distance (yards).

- carry_prg_dist:

  Progressive carrying distance (yards).

- carry_prg_c:

  Progressive carries.

- carries_final_third:

  Carries into final third.

- carries_pen_area:

  Carries into penalty area.

- miscontrols:

  Miscontrols.

- dispossessed:

  Dispossessed.

- passes_received:

  Passes received.

- prog_passes_received_possession:

  Progressive passes received (Possession table).

## Source

FBref (<https://fbref.com>)

## Details

Each row represents one player-season. The dataset is produced by
[`merge_fbref_tables()`](https://ifylopez.github.io/pitchimpact/reference/merge_fbref_tables.md)
using standardized inputs from the corresponding `import_fbref_*()`
functions.

The master dataset is intended for end-to-end workflow examples,
exploratory analysis, modeling, and visualization.

## Examples

``` r
data(master_example, package = "pitchimpact")
dplyr::glimpse(master_example)
#> Rows: 47
#> Columns: 99
#> $ team                            <chr> "Rosario Central", "Rosario Central", …
#> $ season                          <dbl> 2025, 2025, 2025, 2025, 2025, 2025, 20…
#> $ player                          <chr> "Agustín Sández", "Jorge Broun", "Víct…
#> $ nation_raw                      <chr> "py PAR", "ar ARG", "ar ARG", "ar ARG"…
#> $ nation_code                     <chr> "PAR", "ARG", "ARG", "ARG", "ARG", "CO…
#> $ pos_raw                         <chr> "DF", "GK", "MF", "MF", "DF", "MF,FW",…
#> $ pos_primary                     <chr> "DF", "GK", "MF", "MF", "DF", "MF", "D…
#> $ pos_secondary                   <chr> NA, NA, NA, NA, NA, "FW", NA, NA, NA, …
#> $ age                             <dbl> 24, 38, 37, 23, 27, 24, 36, 28, 24, 29…
#> $ mp                              <dbl> 30, 29, 27, 28, 29, 25, 23, 26, 23, 29…
#> $ starts                          <dbl> 30, 29, 27, 27, 24, 23, 23, 19, 17, 16…
#> $ minutes                         <dbl> 2575, 2609, 2389, 2302, 2167, 1924, 18…
#> $ nineties                        <dbl> 28.6, 29.0, 26.5, 25.6, 24.1, 21.4, 20…
#> $ goals                           <dbl> 2, 0, 7, 1, 0, 4, 0, 0, 0, 3, 0, 7, 5,…
#> $ assists                         <dbl> 1, 0, 5, 1, 3, 1, 2, 0, 0, 3, 0, 3, 1,…
#> $ goals_assists                   <dbl> 3, 0, 12, 2, 3, 5, 2, 0, 0, 6, 0, 10, …
#> $ non_pen_goals                   <dbl> 2, 0, 3, 1, 0, 4, 0, 0, 0, 3, 0, 3, 5,…
#> $ pens_made                       <dbl> 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0,…
#> $ pens_att                        <dbl> 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0,…
#> $ yellow                          <dbl> 8, 3, 10, 9, 8, 6, 6, 2, 4, 4, 4, 4, 4…
#> $ red                             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ xg                              <dbl> 1.5, 0.0, 5.9, 0.6, 0.2, 3.0, 0.8, 0.7…
#> $ npxg                            <dbl> 1.5, 0.0, 2.7, 0.6, 0.2, 3.0, 0.8, 0.7…
#> $ xag                             <dbl> 2.0, 0.0, 3.7, 0.4, 1.0, 4.0, 0.4, 0.0…
#> $ npxg_plus_xag                   <dbl> 3.5, 0.0, 6.4, 1.0, 1.2, 7.0, 1.3, 0.7…
#> $ prog_carries                    <dbl> 58, 0, 48, 21, 42, 78, 3, 9, 12, 15, 5…
#> $ prog_passes                     <dbl> 121, 0, 159, 120, 94, 90, 32, 34, 58, …
#> $ prog_passes_received            <dbl> 79, 0, 114, 14, 71, 202, 3, 1, 4, 76, …
#> $ shots                           <dbl> 16, 0, 31, 19, 7, 46, 13, 10, 7, 19, 7…
#> $ shots_on_target                 <dbl> 5, 0, 11, 5, 1, 18, 0, 2, 0, 11, 1, 10…
#> $ shots_on_target_pct             <dbl> 31.3, NA, 35.5, 26.3, 14.3, 39.1, 0.0,…
#> $ shots_per90                     <dbl> 0.56, 0.00, 1.17, 0.74, 0.29, 2.15, 0.…
#> $ shots_on_target_per90           <dbl> 0.17, 0.00, 0.41, 0.20, 0.04, 0.84, 0.…
#> $ goals_per_shot                  <dbl> 0.13, NA, 0.10, 0.05, 0.00, 0.09, 0.00…
#> $ goals_per_shot_on_target        <dbl> 0.40, NA, 0.27, 0.20, 0.00, 0.22, NA, …
#> $ avg_shot_distance               <dbl> 15.0, NA, 23.6, 25.5, 23.8, 20.2, 13.1…
#> $ shots_free_kicks                <dbl> 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0,…
#> $ npxg_per_shot                   <dbl> 0.10, NA, 0.09, 0.03, 0.03, 0.07, 0.06…
#> $ goals_minus_xg                  <dbl> 0.5, 0.0, 1.1, 0.4, -0.2, 1.0, -0.8, -…
#> $ non_pen_goals_minus_npxg        <dbl> 0.5, 0.0, 0.3, 0.4, -0.2, 1.0, -0.8, -…
#> $ passes_completed                <dbl> 992, 607, 863, 874, 814, 517, 599, 653…
#> $ passes_attempted                <dbl> 1324, 789, 1236, 1106, 1109, 754, 694,…
#> $ pass_completion_pct             <dbl> 74.9, 76.9, 69.8, 79.0, 73.4, 68.6, 86…
#> $ total_pass_distance             <dbl> 16479, 17973, 17414, 16987, 13602, 864…
#> $ prog_pass_distance              <dbl> 7837, 13405, 6360, 5931, 6042, 2610, 5…
#> $ short_completed                 <dbl> 491, 79, 327, 319, 400, 262, 139, 172,…
#> $ short_attempted                 <dbl> 563, 79, 401, 390, 475, 326, 156, 204,…
#> $ short_completion_pct            <dbl> 87.2, 100.0, 81.5, 81.8, 84.2, 80.4, 8…
#> $ medium_completed                <dbl> 411, 302, 362, 410, 332, 189, 383, 410…
#> $ medium_attempted                <dbl> 521, 311, 475, 483, 429, 259, 409, 458…
#> $ medium_completion_pct           <dbl> 78.9, 97.1, 76.2, 84.9, 77.4, 73.0, 93…
#> $ long_completed                  <dbl> 69, 225, 157, 126, 63, 50, 74, 63, 57,…
#> $ long_attempted                  <dbl> 163, 396, 276, 187, 138, 89, 121, 116,…
#> $ long_completion_pct             <dbl> 42.3, 56.8, 56.9, 67.4, 45.7, 56.2, 61…
#> $ xa                              <dbl> 1.0, 0.0, 4.0, 0.6, 1.2, 2.9, 0.3, 0.1…
#> $ assists_minus_xag               <dbl> -1.0, 0.0, 1.3, 0.6, 2.0, -3.0, 1.6, 0…
#> $ key_passes                      <dbl> 18, 0, 46, 11, 11, 20, 5, 0, 4, 14, 2,…
#> $ passes_final_third              <dbl> 97, 9, 107, 114, 56, 60, 30, 22, 45, 1…
#> $ passes_penalty_area             <dbl> 11, 0, 39, 7, 14, 25, 0, 0, 2, 9, 4, 3…
#> $ crosses_penalty_area            <dbl> 6, 0, 6, 2, 3, 9, 0, 0, 1, 3, 0, 10, 0…
#> $ progressive_passes              <dbl> 121, 0, 159, 120, 94, 90, 32, 34, 58, …
#> $ tackles                         <dbl> 58, 0, 30, 114, 66, 44, 26, 32, 58, 22…
#> $ tackles_won                     <dbl> 38, 0, 17, 72, 46, 29, 18, 20, 30, 13,…
#> $ tackles_def_3rd                 <dbl> 35, 0, 8, 60, 35, 18, 20, 24, 29, 4, 1…
#> $ tackles_mid_3rd                 <dbl> 18, 0, 19, 45, 29, 21, 6, 8, 27, 14, 6…
#> $ tackles_att_3rd                 <dbl> 5, 0, 3, 9, 2, 5, 0, 0, 2, 4, 0, 2, 2,…
#> $ dribblers_tackled               <dbl> 35, 0, 13, 48, 28, 14, 14, 17, 23, 6, …
#> $ dribbles_challenged             <dbl> 50, 1, 25, 92, 45, 31, 22, 25, 39, 16,…
#> $ dribblers_tackled_pct           <dbl> 70.0, 0.0, 52.0, 52.2, 62.2, 45.2, 63.…
#> $ challenges_lost                 <dbl> 15, 1, 12, 44, 17, 17, 8, 8, 16, 10, 6…
#> $ blocks                          <dbl> 33, 0, 22, 56, 21, 21, 30, 28, 17, 9, …
#> $ shots_blocked                   <dbl> 6, 0, 1, 14, 4, 2, 20, 17, 5, 1, 11, 2…
#> $ passes_blocked                  <dbl> 27, 0, 21, 42, 17, 19, 10, 11, 12, 8, …
#> $ interceptions                   <dbl> 38, 0, 13, 32, 40, 11, 21, 28, 15, 6, …
#> $ tackles_plus_interceptions      <dbl> 96, 0, 43, 146, 106, 55, 47, 60, 73, 2…
#> $ clearances                      <dbl> 126, 5, 31, 62, 95, 7, 137, 183, 39, 2…
#> $ errors_leading_to_shot          <dbl> 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 2, 0, 0,…
#> $ touches                         <dbl> 1680, 824, 1467, 1471, 1403, 1065, 949…
#> $ touches_def_pen                 <dbl> 86, 710, 25, 65, 82, 8, 147, 198, 48, …
#> $ touches_def_3rd                 <dbl> 640, 821, 238, 412, 472, 132, 551, 731…
#> $ touches_mid_3rd                 <dbl> 743, 3, 796, 927, 674, 500, 367, 352, …
#> $ touches_att_3rd                 <dbl> 318, 0, 448, 144, 282, 460, 34, 22, 67…
#> $ touches_att_pen                 <dbl> 28, 0, 39, 10, 20, 53, 24, 16, 5, 58, …
#> $ touches_live                    <dbl> 1680, 824, 1463, 1471, 1403, 1065, 949…
#> $ take_ons_att                    <dbl> 31, 0, 25, 31, 42, 105, 5, 2, 12, 17, …
#> $ take_ons_succ                   <dbl> 12, 0, 6, 18, 21, 42, 4, 1, 6, 8, 0, 3…
#> $ take_ons_succ_pct               <dbl> 38.7, NA, 24.0, 58.1, 50.0, 40.0, 80.0…
#> $ take_ons_tackled                <dbl> 17, 0, 19, 12, 19, 58, 1, 1, 6, 9, 2, …
#> $ take_ons_tackled_pct            <dbl> 54.8, NA, 76.0, 38.7, 45.2, 55.2, 20.0…
#> $ carries                         <dbl> 733, 362, 808, 648, 592, 656, 448, 402…
#> $ carry_total_dist                <dbl> 4112, 2151, 4327, 3227, 3014, 4202, 19…
#> $ carry_prg_dist                  <dbl> 2445, 1397, 2351, 1616, 1594, 2107, 12…
#> $ carry_prg_c                     <dbl> 58, 0, 48, 21, 42, 78, 3, 9, 12, 15, 5…
#> $ carries_final_third             <dbl> 32, 0, 34, 12, 27, 51, 3, 2, 10, 11, 1…
#> $ carries_pen_area                <dbl> 6, 0, 7, 0, 7, 9, 0, 0, 0, 8, 0, 12, 3…
#> $ miscontrols                     <dbl> 32, 0, 42, 37, 27, 70, 11, 8, 20, 46, …
#> $ dispossessed                    <dbl> 20, 0, 32, 27, 21, 42, 1, 0, 9, 19, 1,…
#> $ passes_received                 <dbl> 841, 249, 974, 785, 617, 762, 538, 558…
#> $ prog_passes_received_possession <dbl> 79, 0, 114, 14, 71, 202, 3, 1, 4, 76, …
```
