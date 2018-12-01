
players_raw <-
  nbastatR::get_nba_players()

players <-
  players_raw %>%
  janitor::clean_names() %>%
  filter(is_active) %>%
  select(
    id = id_player,
    name = name_player
  ) %>%
  arrange(id)
players
# players %>% arrange(desc(id))

teproj::export_path(
  players,
  path = config$path_players,
  export = config$export_data
)
