
season <- 2017L

game_logs_player <-
  .try_import_game_logs_player(season = season)

players_summary_nbastatr <-
  game_logs_player %>%
  select_at(vars(matches("^id_(player|team)$|^f[a-z]+$"))) %>%
  group_by(id_player, id_team) %>%
  summarise_if(is.numeric, funs(mean(.))) %>%
  ungroup()
players_summary_nbastatr


game_logs_team <-
  .try_import_game_logs_team(season = season)
#
# game_ids <-
#   game_logs_team %>%
#   distinct(id_game) %>%
#   arrange(id_game)
#
# raw_play_by_play <-
#   game_ids %>%
#   pull() %>%
#   .[1:2] %>%
#   nbastatR::play_by_play(
#     game_ids = .,
#     nest_data = FALSE,
#     return_message = FALSE
#   )
# raw_play_by_play

