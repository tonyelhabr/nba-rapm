

rpm_espn <- .try_import_rpm_espn()
rapm_basektball_analytics <- .try_import_rapm_basketballanalytics()
rapm_estimates <- .try_import_rapm_estimates()
rapm_estimates
estimates_join <-
  left_join(
    rapm_estimates %>%
      select(-matches("[o|d]rapm_rank")) %>%
      rename_at(vars(matches("rank|rapm")), funs(paste0(., "_calc"))) %>%
      select(
        season,
        name,
        matches("rank|rapm")
      ),
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
  left_join(
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
# Try to figure out if there is a correlation between the `pm_diff` and errors with `rapm`
players_summary_compare <-
  .import_data_from_path(season = 2017, path = config$path_players_summary_compare) %>%
  mutate(pm_diff = pm_calc - pm_game_logs_nbastatr)
rapm_estimates_join <-
  left_join(
    rapm_estimates %>%
      select(-matches("[o|d]rapm_rank")) %>%
      rename_at(vars(matches("rank|rapm")), funs(paste0(., "_calc"))) %>%
      select(
        season,
        name,
        matches("rank|rapm")
      ),
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
  )

estimates_join %>%
  select_if(is.numeric) %>%
  lm(formula(rank_espn ~ .), data = .) %>%
  broom::tidy()

estimates_join_slim <-
  estimates_join %>%
  select(
    season,
    name,
    slug,
    matches("rank"),
    matches("^ra?pm")
  )

estimates_join_slim1 <-
  estimates_join_slim %>%
  select(-matches("espn"))

estimates_join_slim1 %>%
  print(n = 25)
estimates_join_slim2 <-
  estimates_join_slim %>%
  select(-matches("basketballanalytics"))

estimates_join_slim
estimates_join_slim %>%
  mutate(diff = (rank_calc - rank_basketballanalytics)) %>%
  arrange(abs(diff))

src_order <- c("calc", "basketballanalytics", "espn")
.create_rgx <-
  function(x) {
    paste0("(", paste(x, collapse = ")|(", sep = ""), ")") %>%
      str_replace_all("\\.", "[.]")
  }
rgx_src <- src_order %>% .create_rgx()
rgx_src
estimates_tidy <-
  estimates_join %>%
  gather(metric_src, value, matches(rgx_src)) %>%
  separate(metric_src, into = c("metric", "src"), sep = "_") %>%
  mutate_at(vars(src), funs(factor(., levels = src_order)))
estimates_tidy
estimates_tidy %>% pull(src) %>% levels()

estimates_h2h <-
  estimates_tidy %>%
  filter(metric == "rank") %>%
  spread(src, value) %>%
  select(-metric) %>%
  arrange(season, calc)
estimates_h2h

estimates_norm <-
  estimates_tidy %>%
  group_by(season, src, metric) %>%
  mutate_at(vars(value), funs(value_prank = dplyr::percent_rank(-.))) %>%
  ungroup()
estimates_norm

estimates_cors <-
  estimates_tidy %>%
  # estimates_norm %>%
  filter(metric == "rank") %>%
  spread(src, value) %>%
  group_by(season) %>%
  nest() %>%
  mutate(cors =
           purrr::map(data,
                      ~select_at(.x, vars(matches(rgx_src))) %>%
                        corrr::correlate(method = "spearman")
           )
  ) %>%
  ungroup() %>%
  unnest(cors) %>%
  gather(src2, value, matches(rgx_src)) %>%
  rename(src1 = rowname) %>%
  arrange(season, src1, src2) %>%
  filter(!is.na(value)) %>%
  filter(src1 < src2)
estimates_cors


library("teplot")
estimates_norm_viz <-
  estimates_norm %>%
  filter(metric == "rank") %>%
  spread(src, value) %>%
  # filter(espn > 0.75 | calc > 0.75) %>%
  mutate(diff = espn - calc) %>%
  mutate(
    diff_grp = diff %>% abs() %>% ggplot2::cut_width(0.25)
  )
estimates_norm_viz <-
  estimates_norm_viz %>%
  left_join(
    estimates_norm_viz %>%
      filter(!is.na(diff)) %>%
      group_by(diff_grp) %>%
      sample_frac(0.05) %>%
      ungroup() %>%
      mutate(name_diff = name)
  )
estimates_norm_viz
viz_estimates_norm1 <-
  estimates_norm_viz %>%
  filter(espn > 0.75 | calc > 0.75) %>%
  ggplot(aes(x = espn, y = calc, color = diff_grp)) +
  geom_point() +
  ggrepel::geom_label_repel(
    aes(label = name_diff)
  ) +
  teplot::theme_te()
viz_estimates_norm1

viz_estimates_norm2 <-
  estimates_norm_viz %>%
  # filter(espn > 0.75 | calc > 0.75) %>%
  ggplot(aes(x = diff, fill = diff_grp)) +
  geom_histogram() +
  teplot::theme_te()
viz_estimates_norm2


estimates_tidy %>%
  filter(src %in% c("calc", "espn"))
