
cl <- parallel::makeCluster(4)
doParallel::registerDoParallel(cl)


separate_lineup <-
  function(data, col, prefix = "x", suffix = 1:5, sep = "-") {
    data %>%
      separate(!!enquo(col), into = paste0(prefix, suffix), sep = sep)
  }

data <-
  config$path_data_clean %>%
  teproj::import_path_cleanly()
data


data <-
  data %>%
  separate_lineup(lineup1, suffix = 1:5) %>%
  separate_lineup(lineup2, suffix = 6:10) %>%
  mutate_at(vars(matches("^x")), funs(as.integer))
data

data <-
  data %>%
  mutate(poss_num = row_number()) %>%
  gather(dummy, id, matches("^x")) %>%
  select(-dummy) %>%
  mutate(side = if_else(is_off == 0L, "d", "o")) %>%
  mutate(xid = sprintf("%s%07d", side, as.integer(id))) %>%
  select(-is_off) %>%
  mutate(dummy = 1L)
data

data %>%
  filter(game_id == config$game_id_check) %>%
  # select(game_id, period, pts, id) %>%
  teproj::export_path(
    config$path_data_check,
    export = config$export_data
  )
.COLS_STATS <- c("n_poss", "min_poss", "pts")
.SUFFIX_COLS_STATS_ORDER1 <- c("o", "d")
.SUFFIX_COLS_STATS_ORDER2 <- c(.SUFFIX_COLS_STATS_ORDER1, "total")
# list(.COLS_STATS, .SUFFIX_COLS_STATS_ORDER) %>%
#   purrr::cross() %>%
#   purrr::map_chr(purrr::lift(paste, sep = "_"))
.COLS_SUMM_BYID_ORDER <-
  c("name",
    "id",
    "n_gm",
    paste0("n_poss_", .SUFFIX_COLS_STATS_ORDER2),
    paste0("min_poss_", .SUFFIX_COLS_STATS_ORDER2),
    paste0("pts_", .SUFFIX_COLS_STATS_ORDER1),
    "pm",
    "pts_o_per100",
    "pts_o_per36"
  )
summ_byid <-
  data %>%
  # mutate(
  #   side = id %>% str_remove_all("[0-9]")
  # ) %>%
  # mutate_at(vars(id), funs(. %>% str_remove("^[o|d]") %>% as.integer())) %>%
  mutate(n_poss = 1L) %>%
  group_by(id, game_id, side) %>%
  summarise(
    n_poss = n(),
    min_poss = sum(min_poss),
    pts = sum(pts)
  ) %>%
  ungroup() %>%
  group_by(id, side) %>%
  summarise(
    n_gm = n(),
    n_poss = sum(n_poss),
    min_poss = sum(min_poss),
    pts = sum(pts)
  ) %>%
  ungroup() %>%
  gather(metric, value, matches("^n_|_poss$|^pts")) %>%
  # group_by(id, metric) %>%
  # mutate_at(vars(value), funs(total = sum(., na.rm = TRUE))) %>%
  # ungroup() %>%
  gather(metric, value, matches("total")) %>%
  unite(metric, metric, side) %>%
  spread(metric, value) %>%
  mutate(
    n_poss_total = n_poss_o + n_poss_d,
    min_poss_total = min_poss_o + min_poss_d,
    pm = pts_o - pts_d
  ) %>%
  mutate(
    pts_o_per100 = 100 * pts_o / n_poss_total,
    pts_o_per36 = 36 * pts_o / min_poss_total
  ) %>%
  rename(n_gm = n_gm_o) %>%
  select(-n_gm_d) %>%
  arrange(desc(min_poss_total)) %>%
  left_join(
    config$path_players %>%
      teproj::import_path_cleanly(),
    by = "id"
  ) %>%
  select(one_of(.COLS_SUMM_BYID_ORDER))
summ_byid
summ_byid %>%
  arrange(desc(pm)) %>%
  select(name, pm, n_gm, min_poss_total)
teproj::export_path(
  summ_byid,
  config$path_players_summ,
  export = config$export_data
)

data <-
  data %>%
  semi_join(
    summ_byid %>%
      filter(min_poss_total > 250),
    by = "id"
  ) %>%
  select(xid, pts, poss_num, dummy)

create_data_wide <-
  function(data, prefix = c("o", "d"), path) {
    # prefix <- "o"
    # sprintf("Trying to export \"%s\" possession data to %s.", prefix, path)
    res <-
      data %>%
      filter(str_detect(xid, sprintf("^%s", prefix))) %>%
      spread(xid, dummy, fill = 0L) %>%
      mutate(n_poss = 1L) %>%
      group_by_at(vars(-pts, -n_poss)) %>%
      summarise_at(vars(pts, n_poss), funs(sum)) %>%
      ungroup() %>%
      select(pts, n_poss, everything()) %>%
      # filter(n_poss > 1) %>%
      # filter(pts > 2) %>%
      mutate(pts = 100 * pts / n_poss) %>%
      filter(abs(pts) <= 400) %>%
      # filter(pts <= 200) %>%
      select(-n_poss)

    # res %>% write_rds(path)
    res
  }

data_wide_o <-
  create_data_wide(
    data,
    prefix = "o",
    path = config$path_cache_o
  )

# teproj::export_path(
#   data_wide_o,
#   path = config$path_cache_o,
#   export = config$export_data
# )

write_rds(
  data_wide_o,
  path = config$path_cache_o
)

data_wide_d <-
  create_data_wide(
    data,
    prefix = "d",
    path = config$path_cache_d
  )

# teproj::export_path(
#   data_wide_d,
#   path = config$path_cache_d,
#   export = config$export_data
# )

write_rds(
  data_wide_d,
  path = config$path_cache_d
)

if(exists("cl")) {
  # closeAllConnections()
  # doParallel::stopImplicitCluster()
  parallel::stopCluster(cl)
}
