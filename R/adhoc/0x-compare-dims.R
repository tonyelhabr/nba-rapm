
# path_estimates_old <-
#   "_project/nba-rapm/rapm_estimates.csv"
#
# estimates_old <-
#   path_estimates_old %>%
#   teproj::import_path_cleanly()
# estimates_old
#
# estimates_pretty_old <-
#   estimates_old %>%
#   mutate_at(vars(matches("rapm")), funs(rnk = row_number(desc(.)))) %>%
#   left_join(players, by = "id")
# estimates_pretty_old
#
# estimates_old %>% arrange(id)
# estimates_old %>% arrange(desc(id))
# players %>% arrange(id)
# players %>% arrange(desc(id))
# players %>% filter(str_detect(id, "01158"))
# players %>% filter(str_detect(id, "235"))

# data-clean ----
path_data_clean_old <- "_project/rapm_data.csv"

data_clean_old <-
  path_data_clean_old %>%
  teproj::import_path_cleanly()
data_clean <-
  config$path_data_clean %>%
  teproj::import_path_cleanly()

data_clean_old %>%
  count(game_id, sort = TRUE)

data_clean %>%
  count(game_id, sort = TRUE)

data_clean_old %>%
  count(game_id, sort = TRUE) %>%
  ggplot(aes(x = n)) +
  geom_density()

data_clean %>%
  count(game_id, sort = TRUE) %>%
  ggplot(aes(x = n)) +
  geom_density()

# data_wide ----
path_cache_o_old <- "_project/poss-data-wide-o.rds"
path_cache_d_old <- path_cache_o_old %>% str_replace("-o", "-d")
data_wide_o_old <-
  path_cache_o_old %>%
  teproj::import_path_cleanly()
data_wide_d_old <-
  path_cache_d_old %>%
  teproj::import_path_cleanly()

data_wide_o <-
  config$path_cache_o %>%
  teproj::import_path_cleanly()
data_wide_d <-
  config$path_cache_d %>%
  teproj::import_path_cleanly()


data_wide_o_old %>%
  arrange(desc(pts))
data_wide_o %>%
  arrange(desc(pts))
