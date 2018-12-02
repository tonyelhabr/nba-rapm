
# helfRlein::clean_gc()

# https://drive.google.com/drive/folders/1GMiP-3Aoh2AKFCoGZ8f0teMYNlkm87dm
data_raw <-
  "data-raw/play_by_play_with_lineup/play_by_play_with_lineup_2017-18.csv" %>%
  # read_csv()
  teproj::import_path_cleanly()
data_raw

data <-
  data_raw %>%
  filter(!is.na(player1team_id)) %>%
  select(
    game_id,
    period,
    event_number,
    # season,
    # season_type,
    # matches("_time_string"),
    # dt,
    min_period_remain = pc_time_string,
    sec_elapsed = time_elapsed,
    matches("_score$"),
    matches("^team_id"),
    matches("team1player[1-5]id$"),
    matches("team2player[1-5]id$")
  ) %>%
  unite(lineup1, matches("team1player"), sep = "-") %>%
  unite(lineup2, matches("team2player"), sep = "-") %>%
  rename(pts_home = home_score, pts_away = away_score) %>%
  arrange(game_id, period, event_number) %>%
  select(-event_number)
data

data <-
  data %>%
  group_by(game_id) %>%
  mutate(poss_num = row_number()) %>%
  mutate(min_poss_diff = (sec_elapsed - lag(sec_elapsed, 1)) / 60) %>%
  fill(pts_home) %>%
  fill(pts_away) %>%
  ungroup() %>%
  mutate_at(vars(matches("^pts|min_poss_diff")), funs(coalesce(., 0))) %>%
  mutate_at(vars(matches("^pts")), funs(as.integer))
data
