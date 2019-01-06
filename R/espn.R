
# url <- "http://www.espn.com/nba/statistics/rpm/_/year/2018"

.get_grid_url_rpm_espn  <-
  function(season = as.integer(format(Sys.Date(), "%Y")), ...) {
    # .N_PLAYER_NBA <- (15L + 1L) * 30L
    # .N_ROW_PER_PAGE <- 40L
    # .N_PAGE_PER_SEASON_PFREF <- ceiling(.N_PLAYER_NBA / .N_ROW_PER_PAGE)
    .validate_season(season)
    tibble(season = rep(season, 12L)) %>%
      group_by(season) %>%
      mutate(idx_page = row_number()) %>%
      ungroup() %>%
      arrange(season, idx_page) %>%
      mutate(url = glue::glue(
        "http://www.espn.com/nba/statistics/rpm/_/year/{season}/page/{idx_page}"
      )) %>%
      select(-idx_page)
  }

.read_rpm_espn <-
  function(url, ..., col_names = NULL) {
    # browser()
    html <-
      url %>%
      xml2::read_html()
    html_table <-
      html %>%
      rvest::html_nodes("table") %>%
      pluck(1)
    html_data <-
      html_table %>%
      rvest::html_table(header = FALSE)
    header <-
      html_data %>%
      slice(1)
    if(is.null(col_names)) {
      col_names <- header %>% as.character()
    }

    html_data %>%
      slice(2:n()) %>%
      tibble::as_tibble() %>%
      filter(X1 != col_names[1]) %>%
      purrr::set_names(col_names)
  }

.clean_rpm_espn <-
  function(data, ...) {
    data %>%
      janitor::clean_names() %>%
      separate(name, into = c("name", "position"), sep = ",\\s+") %>%
      rename(rank = rk, slug_espn = team) %>%
      mutate_at(vars(rank, gp), funs(as.integer)) %>%
      mutate_at(vars(matches("mpg|rpm|wins")), funs(as.numeric))
  }

.download_rpm_espn <-
  function(..., path = config$path_rpm_espn) {
    grid <- .get_grid_url_rpm_espn(...)
    # grid <- .get_grid_url_rpm_espn(seasons = 2017)
    f_possibly <- purrr::possibly(.f = .read_rpm_espn, otherwise = NULL)
    res <- grid %>% mutate(data = purrr::map(url, f_possibly))
    # Could also do this after `unnest()`ing.
    res <- res %>% mutate(data = purrr::map(data, .clean_rpm_espn))
    res <-
      res %>%
      select(data) %>%
      unnest()
    # browser()
    .export_data_from_path(
      ...,
      data = res,
      path = path
    )
    res
  }

