
.combine_data_from_paths <-
  function(...,
           path,
           dir = dirname(path),
           rgx = str_replace_all(basename(path), "\\.", "_[0-9]{4}\\\\.")) {

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
        path = path
      )
    invisible(res)
  }

combine_rapm_estimates <-
  function(...) {
    .combine_data_from_paths(
      ...,
      path = config$path_rapm_estimates
    )
  }

combine_players_summary_compare <-
  function(...) {
    .combine_data_from_paths(
      ...,
      path = config$path_players_summary_compare
    )
  }

# Prefix these with `.` because `.try_import*()` (and `.download_combine*()`) function
# is "preferred".
.combine_rpm_espn <-
  function(...) {
    .combine_data_from_paths(
      ...,
      path = config$path_rpm_espn
    )
  }

.combine_rapm_basketballanalytics <-
  function(...) {
    .combine_data_from_paths(
      ...,
      path = config$path_rapm_basketballanalytics
    )
  }

