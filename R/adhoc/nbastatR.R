
nbastatR::get_players_tables_data()
players_active <-
  df_dict_nba_players %>%
  janitor::clean_names() %>%
  filter(is_active)
players_active

player_career_stats <-
  nbastatR::get_players_career_stats(player_ids = 200746)
player_career_stats %>%
  janitor::clean_names() %>%
  filter(mode_search == "Totals", str_detect(name_table, "SeasonTotalsRegularSeason")) %>%
  unnest(data_table) %>%
  select(minutes)
