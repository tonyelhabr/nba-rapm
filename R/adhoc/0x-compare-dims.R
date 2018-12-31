
# path_coefs_old <-
#   "_project/nba-rapm/rapm_coefs.csv"
#
# coefs_old <-
#   path_coefs_old %>%
#   .import_data()
# coefs_old
#
# coefs_pretty_old <-
#   coefs_old %>%
#   mutate_at(vars(matches("rapm")), funs(rnk = row_number(desc(.)))) %>%
#   left_join(players, by = "id")
# coefs_pretty_old
#
# coefs_old %>% arrange(id)
# coefs_old %>% arrange(desc(id))
# players %>% arrange(id)
# players %>% arrange(desc(id))
# players %>% filter(str_detect(id, "01158"))
# players %>% filter(str_detect(id, "235"))

.SEASON <- 2017L

# play_by_play ----
path_play_by_play_old <- "_project/rapm_data.csv"

play_by_play_old <-
  path_play_by_play_old %>%
  .import_data()

play_by_play <-
  .import_data_from_path_format(
    path_format = args$path_play_by_play_format,
    season = .SEASON,
  )

play_by_play_old %>%
  count(game_id, sort = TRUE)

play_by_play %>%
  count(game_id, sort = TRUE)

play_by_play_old %>%
  count(game_id, sort = TRUE) %>%
  ggplot(aes(x = n)) +
  geom_density()

play_by_play %>%
  count(game_id, sort = TRUE) %>%
  ggplot(aes(x = n)) +
  geom_density()

# possession_data ----
.import_possession_data_side <-
  function(side = .SIDES, season, path_possession_data_side_format, ...) {
    side <- match.arg(side)

    .import_data_from_path_format(
      path_format = path_possession_data_side_format,
      season = season,
      ...
    )
  }

.import_possession_data_o <-
  purrr::partial(
    .import_possession_data_side,
    side = "o",
    season = .SEASON,
    path_possession_data_side_format = args$path_possession_data_o_format
  )
.import_possession_data_d <-
  purrr::partial(
    .import_possession_data_side,
    side = "d",
    season = .SEASON,
    path_possession_data_side_format = args$path_possession_data_d_format
  )
possession_data_o <- .import_possession_data_o()
possession_data_d <- .import_possession_data_d()

path_possession_data_o_old <- "_project/poss-data-wide-o.rds"
path_possession_data_d_old <- path_possession_data_o_old %>% str_replace("-o", "-d")

possession_data_o_old <-
  path_possession_data_o_old %>%
  .import_data()
possession_data_d_old <-
  path_possession_data_d_old %>%
  .import_data()

possession_data_o_old %>%
  arrange(desc(pts))
possession_data_o %>%
  arrange(desc(pts))
