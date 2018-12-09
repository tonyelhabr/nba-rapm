

.import_possession_data_side <-
  function(side = .SIDES, season, path_possession_data_side_format, ...) {
    side <- match.arg(side)

    .import_data_from_path_format(
      path_format = path_possession_data_side_format,
      season = season,
      ...
    )
  }

.import_possession_data_o <-
  purrr::partial(
    .import_possession_data_side,
    side = "o",
    season = args$season,
    path_possession_data_side_format = args$path_possession_data_o_format
  )
possession_data_o <- .import_possession_data_o()


# side <- "o"
# x <- sprintf("args$path_possession_data_%s_format", side)
# path_format <- get(x, envir = .GlobalEnv)
