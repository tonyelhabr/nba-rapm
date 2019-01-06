
data_check <-
  config$path_data_check %>%
  teproj::import_path_cleanly()
data_check

game_log <-
  nbastatR::get_games_play_by_play(game_ids = config$game_id_check) %>%
  janitor::clean_names()
game_log

players <-
  config$path_players %>%
  teproj::import_path_cleanly()

# players %>%
#   filter(str_detect(name, "James")) %>%
#   arrange(name) %>%
#   pull(name) %>%
#   nbastatR::get_nba_players_ids()
data_check %>%
  mutate_at(
    vars(id),
    funs(str_remove(., "^[o|d]") %>% as.integer())
  ) %>%
  inner_join(
    players,
    by = "id"
  ) %>%
  select(-id) %>%
  spread(name, dummy) %>%
  arrange(game_id, period, poss_num)
