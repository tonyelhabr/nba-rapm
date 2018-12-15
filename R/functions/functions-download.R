

# Links derived from public google drive folders at
# https://drive.google.com/drive/folders/1GMiP-3Aoh2AKFCoGZ8f0teMYNlkm87dm.
.ID_GOOGLEDRIVE_RAW_PLAY_BY_PLAY <-
  "1iXxovZCf1QcHfiDXWBV3aWURkg1ymqh9"
.DIR_DL <- "data-raw"
.OVERWRITE <- FALSE
.download_googledrive_files <-
  function(id,
           dir = .DIR_DL,
           ...,
           verbose = .VERBOSE,
           overwrite = .OVERWRITE) {
    stop(
      "You should download this manually instead (until better functionality is implemented",
      call. = FALSE
    )
    .create_dir_ifnecessary(dir = dir, verbose = verbose)
    temp_zip <- tempfile(fileext = ".zip")
    dribble_raw <-
      purrr::possibly(
        id %>%
          googledrive::as_id() %>%
          googledrive::drive_download(
            path = temp_zip,
            overwrite = overwrite,
            verbose = verbose
          ),
        otherwise = NULL
      )
    if (is.null(dribble_raw)) {
      .display_error(sprintf("Could not download files for `id =  %s`.", id),
                     verbose = verbose)
      return(invisible(NULL))
    }
    paths <- utils::unzip(temp_zip, exdir = dir)

    n_paths <- length(paths)
    .display_info(
      sprintf(
        "%d files succesfully downloaded and unzipped to %s for `id = %s`.",
        n_paths,
        dir,
        id
      ),
      verbose = verbose
    )
    invisible(unlink(temp_zip, recursive = TRUE))

    # TODO: Rename?
    invisible(dribble_raw)
  }

.download_raw_play_by_play_files <-
  function(id = .ID_GOOGLEDRIVE_RAW_PLAY_BY_PLAY, ..., verbose = .VERBOSE) {
    .download_googledrive_files(id = id,
                                verbose = verbose,
                                ...)
  }


.download_rda_file <-
  function(season,
           dir = .DIR_DL,
           url = NULL,
           path = NULL,
           ...,
           verbose = .VERBOSE,
           overwrite = .OVERWRITE) {
    .validate_season(season)

    .create_dir_ifnecessary(dir = dir, verbose = verbose)

    if (is.null(url)) {
      .URL_PREFIX_EIGHTYTHIRTYFOUR <-
        "http://eightthirtyfour.com/nba/pbp/"
      .BASENAME_PREFIX_EIGHTYTHIRTYFOUR <- "PbP_"
      .EXT_EIGHTYTHIRTYFOUR <- "Rda"
      season_suffix <-
        sprintf("%02d_%02d", (season) %% 2000, (season + 1) %% 2000)
      basename <-
        sprintf(
          "%s%s.%s",
          .BASENAME_PREFIX_EIGHTYTHIRTYFOUR,
          season_suffix,
          .EXT_EIGHTYTHIRTYFOUR
        )
      url <- sprintf("%s%s", .URL_PREFIX_EIGHTYTHIRTYFOUR, basename)
    }

    if (is.null(path)) {
      .get_path_from(
        path = file.path(
          dir,
          sprintf("%s.%s", "raw_play_by_play", .EXT_EIGHTYTHIRTYFOUR)
        ),
        season = season,
        # season_type = "Any",
        raw_data_source = "eightthirtyfour"
      )
    }
    if (file.exists(path)) {
      msg <- sprintf("%s already exists!")
      if (overwrite) {
        msg <-
          sprintf("%s Overwriting because `overwrite = %s`.", msg, overwrite)
        .display_info(msg, verbose = verbose)
      } else {
        msg <-
          sprintf("%s Not overwriting because `overwrite = FALSE`.",
                  msg,
                  overwrite)
        .display_info(msg, verbose = verbose)
        return(invisible(NULL))
      }
    }
    f_possibly <-
      purrr::possibly(~ download.file(
        url = url,
        destfile = path,
        mode = "wb",
        quiet = TRUE
      ),
      otherwise = -1)
    dl_success <- f_possibly()
    if (dl_success != 0) {
      .display_warning(
        sprintf(
          "Could not download file from %s to %s for `season = %s`.",
          url,
          path,
          season
        ),
        verbose = verbose
      )
      return(invisible(NULL))
    }
    .display_info(
      sprintf(
        "Succesfully downloaded file from %s to %s for `season = %s`.",
        url,
        path,
        season
      ),
      verbose = verbose
    )
    invisible(path)
  }

download_raw_data <-
  function(raw_data_source = .RAW_DATA_SOURCES, ...) {
    .validate_raw_data_source(raw_data_source)
    if (raw_data_source == .RAW_DATA_SOURCES[1]) {
      .download_rda_file(...)
    } else if (raw_data_source == .RAW_DATA_SOURCES[2]) {
      .download_raw_play_by_play_files(...)
    } else {
      return(invisible(NULL))
    }
  }
