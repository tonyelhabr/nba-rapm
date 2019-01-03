

.create_path_combined <-
  function(x) {
    x %>% str_replace_all("\\.", "_combined")
  }

.combine_data_from_paths <-
  function(...,
           path,
           dir = dirname(path),
           rgx = .create_path_season_rgx(path),
           path_out = .create_path_combined(path)) {

    # Use `list.files()` instead of `fs::dir_ls()` because the former raises an error
    # if extra arguments are passed through `...`
    paths <-
      list.files(
        ...,
        path = dir,
        pattern = rgx,
        full.names = TRUE
      )

    res <-
      paths %>%
      tibble(path = .) %>%
      mutate(season = path %>% str_replace_all("(^.*_)([0-9]{4})(.*$)", "\\2") %>% as.integer()) %>%
      mutate(data = purrr::map(path, ~.import_data_from_path(..., validate = FALSE, path = .x))) %>%
      select(season, data) %>%
      unnest(data)
    path_export <-
      .export_data_from_path(
        ...,
        validate = FALSE,
        data = res,
        path = path_out
      )
    invisible(res)
  }

# Don't think I really need to specify `path_out` with these, assuming that
# they all just substitute season suffix with `_combined`.
combine_rapm_coefs <-
  function(...) {
    .combine_data_from_paths(
      ...,
      path = config$path_rapm_coefs
    )
  }

combine_players_summary_compare <-
  function(...) {
    .combine_data_from_paths(
      ...,
      path = config$path_players_summary_compare
    )
  }

combine_rpm_espn <-
  function(...) {
    .combine_data_from_paths(
      ...,
      path = config$path_rpm_espn
    )
  }

combine_rapm_basketballanalytics <-
  function(...) {
    .combine_data_from_paths(
      ...,
      path = config$path_rapm_basketballanalytics
    )
  }

