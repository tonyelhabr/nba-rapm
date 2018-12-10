
season <- 2017L
players_raw <-
  nbastatR::get_nba_players()
players_raw

players <-
  players_raw %>%
  janitor::clean_names() %>%
  janitor::clean_names() %>%
  filter(
    year_season_first <= season,
    year_season_last >= season
  ) %>%
  select(
    id = id_player,
    name = name_player
  ) %>%
  arrange(id)
players
# players %>% arrange(desc(id))

# nbastatR::get_players_tables_data()
# players_active <-
#   df_dict_nba_players %>%
#   janitor::clean_names() %>%
#   filter(is_active)
# players_active
#
# player_career_stats <-
#   nbastatR::get_players_career_stats(player_ids = 200746)
# player_career_stats %>%
#   janitor::clean_names() %>%
#   filter(mode_search == "Totals", str_detect(name_table, "SeasonTotalsRegularSeason")) %>%
#   unnest(data_table) %>%
#   select(minutes)

tms_raw <-
  nbastatR::get_nba_teams()
tms_raw

tms <-
  tms_raw %>%
  janitor::clean_names() %>%
  filter(is_non_nba_team == 0L) %>%
  filter(year_played_last >= season %>%
  select(
    id = id_team,
    name = name_team,
    slug = slug_team
  )
tms
