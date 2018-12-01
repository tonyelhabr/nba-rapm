
# Source: https://dblackrun.github.io/2018/04/17/nba-possession-data.html.
# url <- "https://s3.amazonaws.com/datastats/db_dumps/possession_details_00217.csv.zip"
data_raw <-
  config$path_data_raw %>%
  # read_csv()
  teproj::import_path()
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
  .[, c(2, 3, 5)] %>%
  as_tibble() %>%
  purrr::set_names(c("lineup1", "lineup2", "poss")) %>%
  # filter(str_detect(poss, "^(Def|is_off)Poss$")) %>%
  mutate(is_off =
           case_when(
             poss == "OffPoss" ~ 1L,
             poss == "DefPoss" ~ -1L,
             TRUE ~ 0L
             )
         ) %>%
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
# game_id_filt <- "21700605"
# data2 <- nbastatR::get_games_play_by_play(game_ids = game_id_filt)
#
# data2_filt <-
#   data2 %>%
#   janitor::clean_names() %>%
#   select(
#     game_id = id_game,
#     period = number_period,
#     minsec_start_period = time_quarter,
#     pts_home = score_home,
#     pts_away = score_away
#   ) %>%
#   filter(!is.na(pts_home)) %>%
#   mutate(pts_diff_start = pts_home - pts_away) %>%
#   separate(minsec_start_period, into = c("min_start_period", "sec_start_period"), sep = ":") %>%
#   mutate_all(funs(as.integer)) %>%
#   mutate(min_start_period = min_start_period + (sec_start_period / 60L)) %>%
#   select(-matches("sec_start_period"))
# data2_filt
#
# data_filt <-
#   data %>%
#   filter(game_id == game_id_filt)
# data_filt
#
# data_join_filt <-
#   data_filt %>%
#   mutate(
#     pts_diff_start_abs = abs(pts_diff_start)
#   ) %>%
#   left_join(
#     data2_filt %>% rename(pts_diff_start_abs = pts_diff_start),
#     by = c("game_id", "period", "min_start_period", "pts_diff_start_abs")
#   ) %>%
#   # filter(lineup2 == "") %>%
#   distinct() %>%
#   fill(pts_home) %>%
#   fill(pts_away) %>%
#   mutate_at(vars(matches("^pts_(home)|(away)$")), funs(coalesce(., 0L))) %>%
#   # mutate(pts = (abs(pts_diff_start) - abs(lag(pts_diff_start, 1))) * -is_off) %>%
#   select(-matches("abs$")) %>%
#   mutate_at(
#     vars(matches("id$")),
#     funs(str_replace_all(., "(^.*)([0-9]{3}$)", "\\2") %>% as.integer())
#   ) %>%
#   select(-matches("lineup"), everything(), matches("lineup"))
#
# data_join_filt %>%
#   mutate(tm1 = min(off_id, def_id), tm2 = max(off_id, def_id)) %>%
#   # mutate(pts1 = if_else(off_id == tm1, -lead(pts_diff_start, 1), 0L)) %>%
#   # mutate(pts2 = if_else(off_id != tm1, lead(pts_diff_start, 1) - pts1, 0L)) %>%
#   # arrange(game_id, desc(period), desc(min_end_period)) %>%
#   mutate(
#     pts_diff1 = if_else(off_id == tm1, pts_diff_start, 0L),
#     pts_diff2 = if_else(off_id == tm2, pts_diff_start, 0L)
#   ) %>%
#   mutate(
#     pts1 =
#       if_else(off_id == tm1,
#               if_else((sign(-dplyr::lead(pts_diff2, 1)) == sign(pts_diff1)),
#                       (-dplyr::lead(pts_diff2, 1) - pts_diff1),
#                       # abs(-dplyr::lead(pts_diff2, 1) + pts_diff1)
#                       # -9999L
#                       abs(dplyr::lead(pts_diff2, 1)) + abs(pts_diff1)
#                       ),
#               0L
#               )
#   ) %>%
#   mutate(
#     pts2 =
#       if_else(off_id == tm2,
#               if_else((sign(-dplyr::lead(pts_diff1, 1)) == sign(pts_diff2)),
#                       (-dplyr::lead(pts_diff1, 1) - pts_diff2),
#                       abs(dplyr::lead(pts_diff1, 1)) + abs(pts_diff2)
#               ),
#               0L
#       )
#   ) %>%
#   mutate_at(vars(matches("^pts[12]$")), funs(coalesce(., 0L))) %>%
#   mutate(
#     pts_cumsum1 = cumsum(pts1)
#   ) %>%
#   mutate(
#     pts_cumsum2 = cumsum(pts2)
#   ) %>%
#   select(-matches("lineup")) -> z
#
# z %>%
#   filter((pts_cumsum1 != (pts_cumsum2 + abs(lead(pts_diff_start, 1)))) &
#            (pts_cumsum2 != (pts_cumsum1 + abs(lead(pts_diff_start, 1)))))
# z %>% filter(pts1 > 3 | pts2 > 3)
#
# data %>%
#   filter(game_id == game_id_filt) %>%
#   group_by(game_id) %>%
#   mutate(tm1 = min(off_id, def_id)) %>%
#   # mutate(pts1 = if_else(off_id == tm1, -lead(pts_diff_start, 1), 0L)) %>%
#   # mutate(pts2 = if_else(off_id != tm1, lead(pts_diff_start, 1) - pts1, 0L)) %>%
#   # arrange(game_id, desc(period), desc(min_end_period)) %>%
#   select(-matches("lineup"))

# Remove the dummy row with just a single string in `lineup1` and nothing for `lineup2`.
data <-
  data %>%
  filter(str_detect(lineup1, "[-]"))

# Really should write some functions for these.
data <-
  data %>%
  group_by(game_id) %>%
  mutate(tm1 = min(off_id, def_id), tm2 = max(off_id, def_id)) %>%
  mutate(
    pts_diff1 = if_else(off_id == tm1, pts_diff_start, 0L),
    pts_diff2 = if_else(off_id == tm2, pts_diff_start, 0L)
  ) %>%
  mutate(
    pts1 =
      if_else(off_id == tm1,
        if_else((sign(-dplyr::lead(pts_diff2, 1)) == sign(pts_diff1)),
          (-dplyr::lead(pts_diff2, 1) - pts_diff1),
          abs(dplyr::lead(pts_diff2, 1)) + abs(pts_diff1)
        ),
        0L
      )
  ) %>%
  mutate(
    pts2 =
      if_else(off_id == tm2,
        if_else((sign(-dplyr::lead(pts_diff1, 1)) == sign(pts_diff2)),
          (-dplyr::lead(pts_diff1, 1) - pts_diff2),
          abs(dplyr::lead(pts_diff1, 1)) + abs(pts_diff2)
        ),
        0L
      )
  ) %>%
  mutate(is_off = if_else(off_id == tm1, 1L, 0L)) %>%
  mutate_at(vars(matches("^pts[12]$")), funs(coalesce(., 0L))) %>%
  ungroup() %>%
  select(-matches("^off_id$|^def_id$|^pts_diff_start$|^num_poss_period$|^min_.*_period$"))
data

# There are still some to fix...
data %>%
  filter(is_off == dplyr::lag(is_off, 1) | is_off == dplyr::lead(is_off, 1))

data <-
  data %>%
  filter(
    !((pts1 > 4) | (pts2 > 4)) |
      !(
        ((pts1 > 4) &
           (
             pts1 == abs(pts_diff1)
           )) |
          ((pts2 > 4) &
             (
               pts2 == abs(pts_diff2)
             ))
      )
  )

# Looks like it's pretty good now.
data %>%
  filter(is_off == dplyr::lag(is_off, 1) | is_off == dplyr::lead(is_off, 1))

data <-
  data %>%
  mutate(pts = (pts1 + pts2)) %>%
  select(-matches("pts_diff[1|2]|tm[1|2]|pts[1|2]")) %>%
  # mutate(n_poss = 1L) %>%
  # group_by(lineup1, lineup2, is_off) %>%
  # summarise_at(vars(pts, n_poss), funs(sum)) %>%
  # ungroup() %>%
  # # filter(n_poss > 1) %>%
  # mutate(ppp = 100 * pts / n_poss) %>%
  # filter(ppp <= 300) %>%
  # arrange(desc(pts)) %>%
  arrange(game_id, period) %>%
  ungroup()
data

# # Note: player_ids are either 4, 6, or 7 characters in length
# data %>%
#   separate(lineup1, into = c(paste0("x", 1:5)), sep = "-") %>%
#   gather(key, value, matches("^x")) %>%
#   filter(value != off_id, value != def_id) %>%
#   mutate(n_char = nchar(value)) %>%
#   filter(n_char == max(n_char, na.rm = TRUE)) %>%
#   distinct(value)
#
# data %>%
#   separate(lineup1, into = c(paste0("x", 1:5)), sep = "-") %>%
#   gather(key, value, matches("^x")) %>%
#   filter(value != off_id, value != def_id) %>%
#   mutate(n_char = nchar(value)) %>%
#   # filter(n_char == 4) %>%
#   ggplot(aes(n_char)) +
#   geom_histogram()


teproj::export_path(
  data,
  path = config$path_data_clean,
  export = config$export_data
)

