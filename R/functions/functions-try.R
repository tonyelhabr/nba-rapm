
.try_import_thing <-
  function(...,
           path,
           f_import = .import_data_from_path,
           f_get) {

    res <- attempt::try_catch(expr = f_import(..., path = path, return_type = "warning"), .e = NULL)

    if(!is.null(res)) {
      return(invisible(res))
    }

    .display_info(
      glue::glue("Could not get data with `f_import`. Trying `f_get`."),
      ...
    )
    res <- attempt::try_catch(expr = f_get(..., path = path, return_type = "warning"), .e = NULL)

    if(!is.null(res)) {
      return(invisible(res))
    }

    .display_error(
      glue::glue("Could not get data with `f_get` after failing with `f_import`."),
      ...
    )
  }

.try_import_players_nbastatr <-
  memoise::memoise(
    function(...) {
      .try_import_thing(
        ...,
        f_get = .get_players_nbastatr,
        path = config$path_players_nbastatr
      )
    }
  )

.try_import_teams_nbastatr <-
  memoise::memoise(
    function(...) {
      .try_import_thing(
        ...,
        f_get = .get_teams_nbastatr,
        path = config$path_teams_nbastatr
      )
    }
  )

.try_import_players_game_logs_nbastatr <-
  memoise::memoise(
    function(...) {
      .try_import_thing(
        ...,
        f_get = .get_players_game_logs_nbastatr,
        path = config$path_players_game_logs_nbastatr
      )
    }
  )

.try_import_teams_game_logs_nbastatr <-
  memoise::memoise(
    function(...) {
      .try_import_thing(
        ...,
        f_get = .get_teams_game_logs_nbastatr,
        path = config$path_teams_game_logs_nbastatr
      )
    }
  )

.try_import_players_summary_nbastatr <-
  memoise::memoise(
    function(...) {
      .try_import_thing(
        ...,
        f_get = .get_players_summary_nbastatr,
        path = config$path_players_summary_nbastatr
      )
    }
  )

.try_import_teams_summary_nbastatr <-
  memoise::memoise(
    function(...) {
      .try_import_thing(
        ...,
        f_get = .get_teams_summary_nbastatr,
        path = config$path_teams_summary_nbastatr
      )
    }
  )

# Note that `validate = FALSE` means that only the non-suffixed file is checked for
# existence for determination of whether to call the `f_get` function.
.try_import_rpm_espn <-
  memoise::memoise(
    function(...) {
      .try_import_thing(
        ...,
        validate = FALSE,
        f_get = .download_combine_rpm_espn,
        path = config$path_rpm_espn
      )
    }
  )

.try_import_rapm_basketballanalytics <-
  memoise::memoise(
    function(...) {
      .try_import_thing(
        ...,
        validate = FALSE,
        f_get = .download_combine_rapm_basketballanalytics,
        path = config$path_rapm_basketballanalytics
      )
    }
  )

.try_import_rapm_estimates <-
  memoise::memoise(
    function(...) {
      .try_import_thing(
        ...,
        validate = FALSE,
        f_get = combine_rapm_estimates,
        path = config$path_rapm_estimates
      )
    }
  )


