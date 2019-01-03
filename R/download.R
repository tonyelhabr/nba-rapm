
# pbp_raw ----
# Links derived from public google drive folders at
# https://drive.google.com/drive/folders/1GMiP-3Aoh2AKFCoGZ8f0teMYNlkm87dm.
# Id identified by clickiing on desired folder, copying share link,
# and extracting the text after "id" using the online tool at
# https://sites.google.com/site/gdocs2direct/.
.ID_GOOGLEDRIVE_RAW_PLAY_BY_PLAY <- "1BmBC0EQCsvyCwRHybxm--IMWWVqWEAOu"
.DIR_DATA_RAW <- "data-raw"
.OVERWRITE_GOOGLEDRIVE <- FALSE
.download_googledrive_files <-
  function(...,
           id = .ID_GOOGLEDRIVE_RAW_PLAY_BY_PLAY,
           dir = .DIR_DATA_RAW,
           .overwrite = .OVERWRITE_GOOGLEDRIVE) {

    .create_dir_ifnecessary(..., dir = dir)
    temp_zip <- tempfile(fileext = ".zip")
    f_download <-
      function() {
        id %>%
          googledrive::as_id() %>%
          googledrive::drive_download(
            path = temp_zip,
            overwrite = .overwrite,
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

download_pbp_raw_files <-
  function(..., season = .SEASONS, path = config$path_pbp_raw) {
    paths_exist <-
      .check_dst_files_download(
        ...,
        season = season,
        path = path
      )
    if(paths_exist) {
      return(invisible(NULL))
    }
    .download_googledrive_files(...)
  }


# nbastatr ----
download_nbastatr <-
  function(..., season = .SEASONS) {
    paths_exist <-
      .check_dst_files_download(
        ...,
        season = season,
        path = path
      )
    if(paths_exist) {
      return(invisible(NULL))
    }
    # Note that its erroneous to use `season = .SEASONS` (because the functions
    # will treat `season` as a vector instead of as a scalar), and it doesn't work to
    # call `purrr::invoke_map()` from `purrr::map()`, so use a for loop instead.
    for(.season in season) {
      purrr::invoke_map(
        .f = list(
          .get_players_nbastatr,
          .get_teams_nbastatr,
          .get_players_game_logs_nbastatr,
          .get_teams_game_logs_nbastatr,
          .get_players_summary_nbastatr,
          .get_teams_summary_nbastatr
        ),
        .x = list(list(season = .season))
      )
    }
    invisible()
  }

download_rpm_espn <-
  function(..., season = .SEASONS, path = config$path_rpm_espn) {
    paths_exist <-
      .check_dst_files_download(
        ...,
        season = season,
        path = path
      )
    if(paths_exist) {
      return(invisible(NULL))
    }
    res <- purrr::map(season, ~.download_rpm_espn(..., season = .x))
    invisible(res)
  }

download_rapm_szou <-
  function(..., season = .SEASONS, path = config$path_rapm_szou) {
    paths_exist <-
      .check_dst_files_download(
        ...,
        season = season,
        path = path
      )
    if(paths_exist) {
      return(invisible(NULL))
    }
    res <- purrr::map(season, ~.download_rapm_szou(..., season = .x))
    invisible(res)
  }

# download-combine ----
# UPDATE: Decided that `download*()` and `combine*()` functions should be
# called explicitly.
# .download_combine_thing <-
#   function(..., season = .SEASONS, f_download, path) {
#     res_dummy <- purrr::map(season, ~f_download(..., season = .x))
#     res <-
#       .combine_data_from_paths(
#         ...,
#         path = path
#       )
#     invisible(res)
#   }
#
# # .download_combine_thing_memoise <- memoise::memoise(.download_combine_thing)
#
# # Note that `path` is made to be a parameter here so that it can be "overwritten"
# # when called from the corresponding `.try_import*()` function.
# .download_combine_rpm_espn <-
#   memoise::memoise(
#     function(..., path = config$path_rpm_espn_combined) {
#       .download_combine_thing(
#         ...,
#         f_download = .download_rpm_espn,
#         path = path
#       )
#     }
#   )
#
# .download_combine_rapm_szou <-
#   memoise::memoise(
#     function(..., path = config$path_rapm_szou_combined) {
#       .download_combine_thing(
#         ...,
#         f_download = .download_rapm_szou,
#         path = path
#       )
#     }
#   )

