
path_estimates_old <-
  "_project/nba-rapm/rapm_estimates.csv"

estimates_old <-
  path_estimates_old %>%
  teproj::import_path_cleanly()
estimates_old

estimates_pretty_old <-
  estimates_old %>%
  mutate_at(vars(matches("rapm")), funs(rnk = row_number(desc(.)))) %>%
  left_join(players, by = "id")
estimates_pretty_old

estimates_old %>% arrange(id)
estimates_old %>% arrange(desc(id))
players %>% arrange(id)
players %>% arrange(desc(id))
players %>% filter(str_detect(id, "01158"))
players %>% filter(str_detect(id, "235"))
