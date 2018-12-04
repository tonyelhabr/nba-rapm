
# helfRlein::clean_gc()
# https://drive.google.com/drive/folders/1GMiP-3Aoh2AKFCoGZ8f0teMYNlkm87dm
tempfile <- tempfile(fileext = ".zip")
dir_game_summary_raw <-
  "https://drive.google.com/drive/folders/1GMiP-3Aoh2AKFCoGZ8f0teMYNlkm87dm&export=download" %>%
  # archive::archive_read()
  download.file(destfile = tempfile)
.tempfile_unzipped <- utils::unzip(.tempfile, exdir = tempdir())

# Check that it doesn't already exist when re-sourcing this script in order
# to reduce run-time.
if(!exists("data_raw")) {
  suppressMessages(
    data_raw <-
      "data-raw/play_by_play_with_lineup/play_by_play_with_lineup_2017-18.csv" %>%
      # read_csv()
      teproj::import_path_cleanly()
  )
  data_raw
}

data_select <-
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
    # min_period_remain = pc_time_string,
    sec_elapsed = time_elapsed,
    matches("_score$"),
    player1team_id,
    matches("^team_id"),
    matches("team1player[1-5]id$"),
    matches("team2player[1-5]id$"),
    play_type,
    matches("_description$")
  ) %>%
  unite(lineup1, matches("team1player"), sep = "-") %>%
  unite(lineup2, matches("team2player"), sep = "-") %>%
  mutate(
    is_off1 = if_else(player1team_id == team_id1, 1L, 0L)
  ) %>%
  rename(
    pts_home = home_score,
    pts_away = away_score,
    tm_id1 = team_id1,
    tm_id2 = team_id2
  ) %>%
  arrange(game_id, period, event_number) %>%
  select(-event_number, -player1team_id)
data_select
# data_select %>% filter(!is.na(pts_home) & !is.na(pts_away)) %>% count(play_type, sort = TRUE)
# data_select %>% filter(play_type == "FreeThrow" | lag(play_type == "FreeThrow"))
data_select <-
  data_select %>%
  filter(play_type %in% c("Make", "Miss", "FreeThrow")) %>%
  mutate_at(vars(play_type), funs(if_else(. == "FreeThrow", "Make", .)))
data_select
# data_select %>% group_by(game_id, period, sec_elapsed) %>% filter(n() > 1)
data_select <-
  data_select %>%
  group_by(game_id, period, sec_elapsed) %>%
  summarise_at(vars(matches("^pts_|^tm_|is_off1|^lineup")), funs(dplyr::last)) %>%
  ungroup()
data_select

data_calc <-
  data_select %>%
  group_by(game_id) %>%
  mutate(poss_num = row_number()) %>%
  mutate(min_poss = (sec_elapsed - lag(sec_elapsed, 1)) / 60) %>%
  fill(pts_home) %>%
  fill(pts_away) %>%
  ungroup() %>%
  select(-sec_elapsed) %>%
  mutate_at(vars(matches("^pts|min_poss")), funs(coalesce(., 0))) %>%
  mutate_at(vars(matches("^pts")), funs(as.integer))
data_calc

suppressMessages(
  game_summary_raw <-
    config$path_game_summary_raw %>%
    teproj::import_path_cleanly()
)
game_summary_raw

game_summary <-
  game_summary_raw %>%
  select(
    game_id,
    # season,
    # season_type,
    tm_id_home = home_team_id,
    tm_id_away = away_team_id
  )

data_join <-
  data_calc %>%
  inner_join(
    game_summary,
    by = "game_id"
  ) %>%
  mutate(
    is_home1 = if_else(tm_id1 == tm_id_home, TRUE, FALSE)
  ) %>%
  select(-matches("^tm_id_")) %>%
  mutate(
    pts_tm1 = if_else(is_home1, pts_home, pts_away),
    pts_tm2 = if_else(!is_home1, pts_home, pts_away)
  ) %>%
  select(-is_home1) %>%
  group_by(game_id) %>%
  mutate(
    pts1 = pts_tm1 - dplyr::lag(pts_tm1, default = 0L),
    pts2 = pts_tm2 - dplyr::lag(pts_tm2, default = 0L)
  ) %>%
  ungroup()
data_join

# data_join %>%
#   filter(game_id <= "0021700003") %>%
#   group_by(game_id) %>%
#   summarise_at(vars(matches("^pts[1|2]$")), funs(sum)) %>%
#   ungroup()

data <-
  data_join %>%
  mutate(pts = pts1 + pts2) %>%
  select(
    game_id,
    period,
    # poss_num,
    min_poss,
    is_off = is_off1,
    # tm_id1,
    # tm_id2,
    # pts1,
    # pts2,
    pts,
    lineup1,
    lineup2
  )
data

# data %>% filter(game_id == dplyr::first(game_id)) %>% count()

teproj::export_path(
  data,
  path = config$path_data_clean,
  export = config$export_data
)
