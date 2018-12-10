
# Reference: http://eightthirtyfour.com/data
path_dl <- download_rda_file(season = 2017)
# path_dl <- "data-raw/PbP_2017.Rda"
# rio::import(path_dl)
load(path_dl)

# pbp %>% names()

play_by_play0 <-
  pbp %>%
  as_tibble() %>%
  mutate_if(is.factor, funs(as.character)) %>%
  janitor::clean_names() %>%
  arrange(game_id, period, eventnum)
# play_by_play %>% select_at(vars(matches("_type")))

cols_na <-
  play_by_play0 %>%
  names() %>%
  str_subset("^rebound|^jump_ball|^sub$|^technical|^double_(technical|personal)|^coach_technical|^timeout")
cols_na

play_by_play1 <-
  play_by_play0 %>%
  # select(one_of(cols_na)) %>%
  filter_at(vars(one_of(cols_na)), all_vars(is.na(.) | . == ""))
play_by_play1

# Note that all `type` and `sub_type` records are `NA`,
# so this my approximation of `play_type`.
play_by_play1 <-
  play_by_play1 %>%
  mutate(
    description = coalesce(homedescription, visitordescription, NA_character_),
    type = coalesce(shot_type, NA_character_) # turnover_type, timeout_type, foul_type)
  ) %>%
  select(-matches("(home|visitor|neutral)description|(shot|turnover|timeout|foul|sub)_type")) %>%
  rename(play_type = type)
play_by_play1

rgx_players_home <- "^(home)(_player_id_)([1-5])$"
rgx_players_away <- "^(away)(_player_id_)([1-5])$"
# rgx_players <- "^(home|away)(_player_id_)([1-5])$"
play_by_play2 <-
  play_by_play1 %>%
  select(
    game_id,
    period,
    # Rename as "compromise" between differently named column between sources.
    event_num = eventnum,
    # possession_id, # always `NA`
    time_run = pctimestring,
    pts_home = home_score,
    pts_away = away_score,
    tm_home = home_team,
    tm_away = away_team,
    sec = time, # running time elapsed
    play_type,
    player1_id,
    player1_tm_id = player1_team_id,
    player1_tm_nickname = player1_team_nickname,
    # team,  # always `NA`
    # points_scored, # always `NA`
    # period_start,  # either "True" or ""
    # period_end,  # either "True" or ""
    # matches(rgx_players)
    matches(rgx_players_home),
    matches(rgx_players_away),
    matches("description")
  ) %>%
  unite(lineup_home, matches(rgx_players_home), sep = "-") %>%
  unite(lineup_away, matches(rgx_players_away), sep = "-") %>%
  mutate(
    home_is_off = if_else(player1_tm_id == tm_home, 1L, 0L)
  ) %>%
  arrange(game_id, period, event_num) %>%
  select(-matches("^event_num$|^player1_tm_id$"))

# play_by_play2 %>% count(play_type, pts_home - lag(pts_home), sort = TRUE)
gms <-
  play_by_play2 %>%
  distinct(game_id, tm_home, tm_away) %>%
  arrange(game_id)

play_by_play2 %>% glimpse()

play_by_play2 <-
  play_by_play2 %>%
  rename_at(
    vars(matches(rgx_players)),
    # funs(paste0("x", .))
    funs(str_replace_all(., rgx_players, "tm_\\1_player\\3"))
  )
play_by_play2


