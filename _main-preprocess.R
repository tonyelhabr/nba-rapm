
# # Do these once before.
# download_raw_play_by_play_files()

# # Do these once before/after.
# download_combine_rpm_espn()
# download_combine_rapm_basketballanalytics()

# Do these once after.
# combine_rapm_estimates()

rpm_espn <- .try_import_rpm_espn()
rapm_basektball_analytics <- .try_import_rapm_basketballanalytics()
rapm_estimates <- .try_import_rapm_estimates()
rapm_estimates
estimates_join <-
  left_join(
    rapm_estimates %>%
      rename_at(vars(matches("rank|rpm")), funs(paste0(., "_espn"))) %>%
      select(
        sseason,
        name = name_player,
        matches("rank|rapm")
      ),

    rpm_espn %>%
      rename_at(vars(matches("rank|rpm")), funs(paste0(., "_espn"))) %>%
      select(
        season,
        name,
        position,
        matches("rank|rpm")
      ),
    by = c("season", "name")
  ) %>%
  left_join(
    rapm_basektball_analytics %>%
      # rename_all(funs(paste0(., "_basketballanalytics"))),
      rename_at(vars(matches("rank|rapm")), funs(paste0(., "_basketballanalytics"))) %>%
      select(
        season,
        name,
        slug,
        matches("rank|rapm")
      ),
    by = c("season", "name")
  ) %>%
  select(
    season,
    name,
    slug,
    matches("rank"),
    matches("^r"),
    matches("^o"),
    matches("^d")
  )
estimates_join

estimates_tidy <-
  estimates_join %>%
  gather(metric_src, value, matches("espn|basketballanalytics")) %>%
  separate(metric_src, into = c("metric", "src"), sep = "_")
estimates_tidy
estimates_cors <-
  estimates_tidy %>%
  filter(metric == "rank") %>%
  filter(season == 2017) %>%
  # rename(item = name, feature = src) %>%
  rename(item = src, feature = name) %>%
  group_by(season) %>%
  widyr::pairwise_cor(item, feature, value) %>%
  ungroup()
estimates_cors

estimates_cors <-
  estimates_tidy %>%
  filter(metric == "rank") %>%
  # filter(season == 2017) %>%
  spread(src, value) %>%
  group_by(season) %>%
  nest() %>%
  mutate(cors =
           purrr::map(data,
                      ~select_at(.x, vars(matches("espn|basketballanalytics")))
                      %>% corrr::correlate()
           )
  ) %>%
  ungroup() %>%
  unnest(cors)
estimates_cors
