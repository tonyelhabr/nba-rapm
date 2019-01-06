
.create_url_sz <-
  function(..., season) {
    .validate_season(season)
    season1 <- season
    season2 <- str_remove(season + 1, "^[0-9]{2}")
    glue::glue(
      "https://basketball-analytics.gitlab.io/rapm-data/data/{season1}-{season2}-rapm.json"
    )
  }

.download_rapm_sz <-
  function(..., path = config$path_rapm_sz) {
    url <- .create_url_sz(...)
    resp <-
      url %>%
      httr::GET()
    httr::warn_for_status(resp)
    cont_raw <-
      resp %>%
      httr::content()
    data_raw <-
      cont_raw %>%
      unlist() %>%
      tibble::enframe()
    data_sep <-
      data_raw %>%
      mutate_at(vars(name), funs(. %>% str_remove("data") %>% as.integer())) %>%
      mutate(idx = ((name - 1) %% 7) + 1) %>%
      select(idx, value)
    res <-
      left_join(
        data_sep,
        data_sep %>%
          filter(idx == 1) %>%
          mutate(idx_grp = row_number()),
        by = c("idx", "value")
      ) %>%
      fill(idx_grp) %>%
      # group_by(idx_grp) %>%
      spread(idx, value) %>%
      select(-idx_grp) %>%
      purrr::set_names(c("rank", "name", "slug", "poss", "orapm", "drapm", "rapm")) %>%
      mutate_at(vars(matches("rank|poss")), funs(as.integer)) %>%
      mutate_at(vars(matches("rapm")), funs(as.numeric))

    path_export <-
      .export_data_from_path(
        ...,
        data = res,
        path = path
      )
    res
  }
