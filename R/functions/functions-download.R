
# raw_play_by_play ----
# Links derived from public google drive folders at
# https://drive.google.com/drive/folders/1GMiP-3Aoh2AKFCoGZ8f0teMYNlkm87dm.
# Id identified by clickiing on desired folder, copying share link,
# and extracting the text after "id" using the online tool at
# https://sites.google.com/site/gdocs2direct/.
.ID_GOOGLEDRIVE_RAW_PLAY_BY_PLAY <- "1BmBC0EQCsvyCwRHybxm--IMWWVqWEAOu"
.DIR_DL <- "data-raw"
.OVERWRITE <- FALSE
.download_googledrive_files <-
  function(...,
           id = .ID_GOOGLEDRIVE_RAW_PLAY_BY_PLAY,
           dir = .DIR_DL,
           overwrite = .OVERWRITE) {

    .create_dir_ifnecessary(..., dir = dir)
    temp_zip <- tempfile(fileext = ".zip")
    f_download <-
      function() {
        id %>%
          googledrive::as_id() %>%
          googledrive::drive_download(
            path = temp_zip,
            overwrite = overwrite,
            verbose = TRUE
          )
      }
    f_download_possibly <- purrr::possibly(f_download, otherwise = NULL)
    dribble_raw <- f_download_possibly()
    if (is.null(dribble_raw)) {
      .display_warning(
        glue::glue("Could not download files for `id = {id}`."),
        ...
      )
      return(invisible(NULL))
    }
    paths <- utils::unzip(temp_zip, exdir = dir)
    # TODO: Rename these more "drastically"?
    paths_renamed <-
      paths %>%
      purrr::walk(
        ~file.rename(from = .x, to = str_replace(.x, "-[01][0-9]", ""))
      )

    n_paths <- length(paths)
    .display_info(
      glue::glue(
        "{n_paths} files succesfully downloaded and unzipped to ",
        "{usethis::ui_path(dir)} for `id = {id}`."
        ),
      ...
    )
    invisible(unlink(temp_zip, recursive = TRUE))
    invisible(paths_renamed)
  }

download_raw_play_by_play_files <-
  function(...) {
    .download_googledrive_files(...)
  }

# nbastatr ----
# (Re-)generate data for a single `nbastatr` function.
# purrr::pmap(
#   list(.SEASONS),
#   ~.try_import_teams_nbastatr(
#     season = ..1
#   )
# )
# Or..., do it all at once.

download_nbastatr <-
  function(..., season = .SEASONS) {
    # Note that its erroneous to use `season = .SEASONS` (because the functions
    # will treat `season` as a vector instead of as a scalar), and it doesn't work to
    # call `purrr::invoke_map()` from `purrr::map()`, so use a for loop instead.
    for(s in season) {
      purrr::invoke_map(
        .f = list(
          .try_import_players_nbastatr,
          .try_import_teams_nbastatr,
          .try_import_players_game_logs_nbastatr,
          .try_import_teams_game_logs_nbastatr,
          .try_import_players_summary_nbastatr,
          .try_import_teams_summary_nbastatr
        ),
        .x = list(list(season = s))
      )
    }
  }

# espn ----
download_rpm_espn <-
  function(..., season = .SEASONS) {
    res <- purrr::map(season, ~.download_rpm_espn(..., season = .x))
    invisible(res)
  }

# basketballanalytics ----
.create_url_basketballanalytics <-
  function(..., season) {
    .validate_season(season)
    season1 <- season
    season2 <- str_remove(season + 1, "^[0-9]{2}")
    glue::glue(
      "https://basketball-analytics.gitlab.io/rapm-data/data/{season1}-{season2}-rapm.json"
    )
  }

.download_rapm_basektballanalytics <-
  function(..., path = config$path_rapm_basketballanalytics) {
    url <- .create_url_basketballanalytics(...)
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
    invisible(res)
  }

.download_combine_thing <-
  function(..., season = .SEASONS, f_download, path) {
    res0 <- purrr::map(season, ~f_download(..., season = .x))
    res <-
      .combine_data_from_paths(
        ...,
        path = path
      )
    invisible(res)
  }

# Note that `path` is made to be a parameter here so that it can be "overwritten"
# when called from the corresponding `.try_import*()` function.
.download_combine_rpm_espn <-
  function(..., path = config$path_rpm_espn) {
    memoise::memoise(
      .download_combine_thing(
        ...,
        f_download = .download_rpm_espn,
        path = path
      )
    )
  }

.download_combine_rapm_basketballanalytics <-
  function(..., path = config$path_rapm_basketballanalytics) {
    memoise::memoise(
      .download_combine_thing(
        ...,
        f_download = .download_rapm_basektballanalytics,
        path = path
      )
    )
  }

