
# Links derived from public google drive folders at
# https://drive.google.com/drive/folders/1GMiP-3Aoh2AKFCoGZ8f0teMYNlkm87dm.
.ID_GOOGLEDRIVE_RAW_PLAY_BY_PLAY <- "1iXxovZCf1QcHfiDXWBV3aWURkg1ymqh9"
.DIR_DL <- "data-raw"
.OVERWRITE <- FALSE
.download_googledrive_files <-
  function(...,
           id = .ID_GOOGLEDRIVE_RAW_PLAY_BY_PLAY,
           dir = .DIR_DL,
           overwrite = .OVERWRITE) {
    .display_error(
      glue::glue(
        "You should download this manually instead ",
        "(until better functionality is implemented)."
      ),
      verbose = verbose
    )
    .create_dir_ifnecessary(dir = dir, verbose = verbose)
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
    paths_renamed <-
      paths %>%
      purrr::walk(
        ~file.rename(from = .x, to = str_replace(.x, "[01][0-9]-", ""))
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

.download_raw_play_by_play_files <-
  function(...) {
    .download_googledrive_files(...)
  }

