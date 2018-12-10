
# Links derived from public google drive folders at
# https://drive.google.com/drive/folders/1GMiP-3Aoh2AKFCoGZ8f0teMYNlkm87dm.
.ID_GOOGLEDRIVE_RAW_PLAY_BY_PLAY <- "1iXxovZCf1QcHfiDXWBV3aWURkg1ymqh9"
.ID_GOOGLEDRIVE_RAW_GAME_SUMMARY <- "1_DvoCC-p5vCCOB4q379AFXJTszeFpKie"
.DIR_DL <- "data-raw"
.OVERWRITE <- TRUE
.download_googledrive_files <-
  function(id, dir = .DIR_DL, ..., verbose = .VERBOSE, overwrite = .OVERWRITE) {
    .create_dir_ifnecessary(dir = dir, verbose = verbose)
    temp_zip <- tempfile(fileext = ".zip")
    dribble_raw <-
      purrr::possibly(
      id %>%
      googledrive::as_id() %>%
      googledrive::drive_download(path = temp_zip, overwrite = overwrite, verbose = verbose),
      otherwise = NULL
      )
    if(is.null(dribble_raw)) {
      .display_error(
        sprintf("Could not download files for `id =  %s`.", id),
        verbose = verbose
      )
      return(invisible(NULL))
    }
    paths <- utils::unzip(temp_zip, exdir = dir)

    n_paths <- length(paths)
    .display_info(
      sprintf("%d files succesfully downloaded and unzipped to %s for `id = %s`.", n_paths, dir, id),
      verbose = verbose
    )
    invisible(unlink(temp_zip, recursive = TRUE))

    # TODO: Rename?
    invisible(dribble_raw)
  }


download_raw_game_summary_files <-
  function(id = .ID_GOOGLEDRIVE_RAW_GAME_SUMMARY, ..., verbose = .VERBOSE) {
    .download_googledrive_files(
      id = id,
      verbose = verbose,
      ...
    )
  }

download_raw_play_by_play_files <-
  function(id = .ID_GOOGLEDRIVE_RAW_PLAY_BY_PLAY, ..., verbose = .VERBOSE) {
    .download_googledrive_files(
      id = id,
      verbose = verbose,
      ...
    )
  }

download_rda_file <-
  function(season, dir = .DIR_DL, path = NULL, ..., verbose = .VERBOSE, overwrite = .OVERWRITE) {
    stopifnot(is.numeric(season))
    .create_dir_ifnecessary(dir = dir, verbose = verbose)

    season_suffix <- sprintf("%02d_%02d", (season) %% 2000, (season + 1) %% 2000)
    .URL_PREFIX_EIGHTYTHIRTYFOUR <- "http://eightthirtyfour.com/nba/pbp/"
    .BASENAME_PREFIX_EIGHTYTHIRTYFOUR <- "PbP_"
    .EXT_EIGHTYTHIRTYFOUR <- "Rda"
    basename <- sprintf("%s%s.%s", .BASENAME_PREFIX_EIGHTYTHIRTYFOUR, season_suffix, .EXT_EIGHTYTHIRTYFOUR)

    if(is.null(path)) {
    path <- file.path(dir, sprintf("%s%s.%s", .BASENAME_PREFIX_EIGHTYTHIRTYFOUR, season, .EXT_EIGHTYTHIRTYFOUR))
    }
    dl_success <-
      purrr::possibly(
        download.file(url = url, destfile = path, quiet = TRUE),
        otherwise = FALSE
      )
    if(!dl_success) {
      .display_warning(
        sprintf("Could not download file from %s to %s for `season = %s`.", url, path, season),
        verbose = verbose
      )
      return(invisible(NULL))
    }
    .display_info(
      sprintf("Succesfully downloaded file from %s to %s for `season = %s`.", url, path, id),
      verbose = verbose
    )
    invisible(path)
  }

download_raw_data <-
  function(raw_data_source = .RAW_DATA_SOURCES, ..., verbose = .VERBOSE) {
    raw_data_source <- .validate_raw_data_source(raw_data_source)
    if(raw_data_source == .RAW_DATA_SOURCES[1]) {
      download_rda_file(verbose = verbose, ...)
    } else if (raw_data_source == .RAW_DATA_SOURCES[2]) {
      download_raw_game_summary_files(...)
      download_raw_play_by_play_files(...)
    } else {
      return(invisible(NULL))
    }
  }
