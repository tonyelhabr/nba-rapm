
# Links derived from public google drive folders at
# https://drive.google.com/drive/folders/1GMiP-3Aoh2AKFCoGZ8f0teMYNlkm87dm.
.ID_GOOGLEDRIVE_RAW_PLAY_BY_PLAY <- "1iXxovZCf1QcHfiDXWBV3aWURkg1ymqh9"
.ID_GOOGLEDRIVE_RAW_GAME_SUMMARY <- "1_DvoCC-p5vCCOB4q379AFXJTszeFpKie"
.download_googledrive_files <-
  function(id, dir_local = tempdir(), ..., overwrite = TRUE, verbose = .VERBOSE) {
    stopifnot(dir.exists(dir))
    temp_zip <- tempfile(fileext = ".zip")
    dribble_raw <-
      purrr::possibly(
      id %>%
      googledrive::as_id() %>%
      googledrive::drive_download(path = temp_zip, overwrite = overwrite, verbose = verbose)
      , otherwise = NULL
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

