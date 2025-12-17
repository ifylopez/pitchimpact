# Quick manual test for import_fbref_standard()
# Assumes you're inside the pitchimpact project

library(dplyr)

devtools::load_all()

path <- "C:/Users/matea/OneDrive/Documentos/Proyectos Personales/prueba3.csv"

rc <- import_fbref_standard(
  path = path,
  team = "Rosario Central",
  season = 2025,
  header_row = 2
)

# 1) Basic sanity
stopifnot(is.data.frame(rc))
stopifnot(nrow(rc) > 0)

# 2) Required columns exist
required_cols <- c(
  "team","season","player",
  "nation_raw","nation_code","nation",
  "pos_raw","pos_primary","pos_secondary","pos",
  "age","mp","starts","minutes","nineties",
  "goals","assists","goals_assists","non_pen_goals","pens_made","pens_att",
  "yellow","red","xg","npxg","xag","npxg_plus_xag",
  "prog_carries","prog_passes","prog_passes_received"
)
missing_cols <- setdiff(required_cols, names(rc))
stopifnot(length(missing_cols) == 0)

# 3) No per-90 duplicate columns should survive
stopifnot(!any(grepl("_2$", names(rc))))

# 4) Types: numeric fields should be numeric (NAs allowed)
numeric_cols <- c(
  "age","mp","starts","minutes","nineties",
  "goals","assists","goals_assists","non_pen_goals","pens_made","pens_att",
  "yellow","red","xg","npxg","xag","npxg_plus_xag",
  "prog_carries","prog_passes","prog_passes_received"
)
stopifnot(all(sapply(rc[numeric_cols], is.numeric)))

# 5) Nation parsing: nation_code should contain a 3-letter uppercase code when present
bad_nation <- rc %>%
  mutate(nation_code_clean = trimws(nation_code)) %>%
  filter(
    !is.na(nation_code_clean) &
      !grepl("[A-Z]{3}", nation_code_clean)
  )

stopifnot(nrow(bad_nation) == 0)


# 6) Position parsing:
# - pos_primary must exist when pos_raw exists
# - pos_secondary must be NA when there is no comma
bad_pos_primary <- rc %>%
  filter(!is.na(pos_raw) & pos_raw != "" & (is.na(pos_primary) | pos_primary == ""))
stopifnot(nrow(bad_pos_primary) == 0)

bad_pos_secondary <- rc %>%
  filter(!grepl(",", pos_raw) & !is.na(pos_secondary))
stopifnot(nrow(bad_pos_secondary) == 0)

# 7) Quick look
message("✅ import_fbref_standard() looks good.")
print(rc %>% select(player, nation_raw, nation_code, pos_raw, pos_primary, pos_secondary) %>% head(10))

