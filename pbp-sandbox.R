
library("tidyverse")
# Source: https://dblackrun.github.io/2018/04/17/nba-possession-data.html.
# url <- "https://s3.amazonaws.com/pbpstats/db_dumps/possession_details_00217.csv.zip"
data_raw <-
  "possession_details_00217.csv" %>%
  readr::read_csv()
data_raw

player_stats <- data_raw %>% pull(PlayerStats)
idx_slice <- 1:20
player_stats0 <- data_raw %>% slice(idx_slice) %>% pull(PlayerStats)
# player_stats0
# # player_stats0 %>% jsonlite::fromJSON()
# # player_stats0 %>% purrr::map(jsonlite::fromJSON)
# # jsonlite::stream_in(textConnection(gsub("\\n", "", player_stats0))) %>% as_tibble()
# player_stats0 %>%
#   str_extract_all("([-0-9a-zA-Z)]+)", simplify = TRUE)  %>%
#   .[,] %>%
#   as_tibble() %>%
#   # glimpse() %>%
#   mutate(rn = row_number()) %>%
#   gather(key, value, 5:(ncol(.)-1)) %>%
#   mutate_at(vars(key), funs(. %>% str_remove("^V") %>% as.integer())) %>%
#   arrange(rn, key) %>%
#   filter(lag(value, 1) == "PlusMinus") %>%
#   count(rn, value, sort = TRUE)
#
# player_stats0 %>%
#   str_extract_all("([-0-9a-zA-Z)]+)", simplify = TRUE)  %>%
#   .[,c(2, 3, 5, 7, 8)] %>%
#   as_tibble() %>%
#   bind_cols(
#     data_raw %>%
#       janitor::clean_names() %>%
#       select(
#         id_game = game_id,
#         period,
#         num_poss = possession_number,
#         off_id = offense_team_id,
#         def_id = defense_team_id,
#         time_start = start_time,
#         time_end = end_time,
#         pts = start_score_differential
#       ) %>%
#       slice(idx_slice)
#   )
#
# opt_old <- getOption("max.print")
# options(max.print = 500)
# player_stats0 %>%
#   str_extract_all("([a-zA-Z]+)", simplify = TRUE)  %>%
#   .[,2:10]
# player_stats0 %>%
#   str_extract_all("([0-9-]+)", simplify = TRUE) %>%
#   .[,2:10]
# options(max.print = opt_old)
player_stats <-
  data_raw %>%
  pull(PlayerStats) %>%
  str_extract_all("([-0-9a-zA-Z)]+)", simplify = TRUE) %>%
  .[,c(2, 3, 5)] %>%
  as_tibble() %>%
  purrr::set_names(c("lineup1", "lineup2", "poss")) %>%
  # filter(str_detect(poss, "^(Def|Off)Poss$")) %>%
  mutate(off = case_when(poss == "OffPoss" ~ 1L, poss == "DefPoss" ~ -1L, TRUE ~ 0L)) %>%
  select(-poss)
player_stats

data <-
  data_raw %>%
  janitor::clean_names() %>%
  # filter(game_id == game_id_filt) %>%
  select(-player_stats) %>%
  select(
    game_id,
    off_id = offense_team_id,
    def_id = defense_team_id,
    period,
    pts_diff_start = start_score_differential,
    num_poss_period = possession_number,
    min_start_period = start_time,
    min_end_period = end_time
  ) %>%
  arrange(game_id, period, num_poss_period) %>%
  mutate_at(vars(matches("^min.*period$")), funs(. / 60L)) %>%
  bind_cols(player_stats) # %>%
  # Other `lineup1`s correspond to `off_id`s that are repeated in the `player_stats`.
  # Don't filter `player_stats` beforehand in order to keep same number of rows
  # for binding.
  # filter(str_detect(lineup1, "[-]"))
data

game_id_filt <- "21700605"
data2 <- nbastatR::get_games_play_by_play(game_ids = game_id_filt)

data2_filt <-
  data2 %>%
  janitor::clean_names() %>%
  select(
    game_id = id_game,
    period = number_period,
    minsec_start_period = time_quarter,
    pts_home = score_home,
    pts_away = score_away
  ) %>%
  filter(!is.na(pts_home)) %>%
  mutate(pts_diff_start = pts_home - pts_away) %>%
  separate(minsec_start_period, into = c("min_start_period", "sec_start_period"), sep = ":") %>%
  mutate_all(funs(as.integer)) %>%
  mutate(min_start_period = min_start_period + (sec_start_period / 60L)) %>%
  select(-matches("sec_start_period"))
data2_filt

data_join_filt <-
  data %>%
  filter(game_id == game_id_filt) %>%
  mutate(
    pts_diff_start_abs = abs(pts_diff_start)
  ) %>%
  left_join(
    data2_filt,
    by = c("game_id", "period", "min_start_period", "pts_diff_start")
  ) %>%
  # filter(lineup2 == "") %>%
  distinct() %>%
  fill(pts_home) %>%
  fill(pts_away) %>%
  mutate_at(vars(matches("^pts_(home)|(away)$")), funs(coalesce(., 0L))) %>%
  # mutate(pts = (abs(pts_diff_start) - abs(lag(pts_diff_start, 1))) * -off) %>%
  select(-matches("abs$")) %>%
  select(-matches("lineup"), everything(), matches("lineup"))

data_join_filt %>%
  mutate_at(vars(matches("id$")), funs(str_replace_all(., "(^.*)([0-9]{3}$)", "\\2") %>% as.integer())) %>%
  mutate(tm1 = min(off_id, def_id), tm2 = max(off_id, def_id)) %>%
  # mutate(pts1 = if_else(off_id == tm1, -lead(pts_diff_start, 1), 0L)) %>%
  # mutate(pts2 = if_else(off_id != tm1, lead(pts_diff_start, 1) - pts1, 0L)) %>%
  # arrange(game_id, desc(period), desc(min_end_period)) %>%
  select(-matches("lineup"))


data %>%
  filter(game_id == game_id_filt) %>%
  group_by(game_id) %>%
  mutate(tm1 = min(off_id, def_id)) %>%
  # mutate(pts1 = if_else(off_id == tm1, -lead(pts_diff_start, 1), 0L)) %>%
  # mutate(pts2 = if_else(off_id != tm1, lead(pts_diff_start, 1) - pts1, 0L)) %>%
  # arrange(game_id, desc(period), desc(min_end_period)) %>%
  select(-matches("lineup"))

data_summ <-
  data %>%
  mutate(n_poss = 1L) %>%
  group_by(lineup1, lineup2, off) %>%
  # summarise(
  #   pts = sum(pts),
  #   n_poss = n()
  # ) %>%
  summarise_at(vars(pts, n_poss), funs(sum)) %>%
  ungroup()
data_summ

data_summ %>%
  # arrange(desc(pts))
  mutate(ppp = pts / n_poss) %>%
  # arrange(desc(ppp))
  arrange(ppp)

separate_lineup <-
  function(data, col, prefix = "x", suffix = 1:5, sep = "-") {
    data %>%
      separate(!!enquo(col), into = paste0(prefix, suffix), sep = sep)
  }

# # Note: player_ids are either 4, 6, or 7 characters in length
# pbp %>%
#   separate(lineup_id1, into = c(paste0("x", 1:5)), sep = "-") %>%
#   gather(key, value, matches("^x")) %>%
#   filter(value != off_id, value != def_id) %>%
#   mutate(n_char = nchar(value)) %>%
#   filter(n_char == max(n_char, na.rm = TRUE)) %>%
#   distinct(value)
#
# pbp %>%
#   separate(lineup_id1, into = c(paste0("x", 1:5)), sep = "-") %>%
#   gather(key, value, matches("^x")) %>%
#   filter(value != off_id, value != def_id) %>%
#   mutate(n_char = nchar(value)) %>%
#   # filter(n_char == 4) %>%
#   ggplot(aes(n_char)) +
#   geom_histogram()

data_sep <-
  data_summ %>%
  separate_lineup(lineup1, suffix = 1:5) %>%
  separate_lineup(lineup2, suffix = 6:10) %>%%
  mutate_at(vars(matches("^x")), funs(as.integer))
data_sep

# to add ----

players_raw <-
  nbastatR::get_nba_players()

players <-
  players_raw %>%
  janitor::clean_names() %>%
  filter(is_active) %>%
  select(
    id_player,
    name_player
  ) %>%
  arrange(id_player)
players
players %>% arrange(desc(id_player))
