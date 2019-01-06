
# The purpose of this wrapper function is to make it so that `path` can be
# a generic argument name (and not something like `path_players_nbastatr`)
# without worrying about it being "caught" in `...`.
.try_import_thing <-
  function(...,
           path,
           f_import = .import_data_from_path,
           f_get) {

    res <- attempt::try_catch(expr = f_import(..., path = path, return_type = "warning"), .e = NULL)

    if(!is.null(res)) {
      return(res)
    }

    .display_info(
      glue::glue("Could not get data with `f_import`. Trying `f_get`."),
      ...
    )
    res <- attempt::try_catch(expr = f_get(..., path = path, return_type = "warning"), .e = NULL)

    if(!is.null(res)) {
      return(res)
    }

    .display_error(
      glue::glue("Could not get data with `f_get` after failing with `f_import`."),
      ...
    )
  }

# nbastatr ----
# Not sure if I should use this or just wrap each call with `memoise::memoise()`.
# .try_import_thing_memoise <- memoise::memoise(.try_import_thing)
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

.try_import_bpm_nbastatr <-
  memoise::memoise(
    function(...) {
      .try_import_thing(
        ...,
        f_get = .get_bpm_nbastatr,
        path = config$path_bpm_nbastatr
      )
    }
  )

# non-nbastatr, with `f_get` ----

.try_import_proj_profile <-
  function(...) {
    .try_import_thing(
      ...,
      validate = FALSE, # Because this is not associated with a season.
      f_get = .get_proj_profile,
      path = config$path_proj_profile
    )
  }

.try_import_metrics_cors_grid <-
  function(...) {
    .display_warning(
      glue::glue("You should probably just run `f_get` directly."),
      ...
    )
    .try_import_thing(
      ...,
      # f_get = .get_metrics_cors_grid,
      f_get = NULL,
      path = config$path_metrics_cors_grid
    )
  }

# Do I need this?
# .try_import_metrics_cors_grid_summary <-
#   function(...) {
#     .try_import_thing(
#       ...,
#       f_get = summarise_metrics_cors_grid,
#       path = config$path_metrics_cors_grid_summary
#     )
#   }


# non-nbastatr, no `f_get` ----
# + These should be used to import one season at a time.
# + These functions set `f_get = NULL` because their output is "non-trivial"
# to recreate, so they should be created by some other process (explicitly by the user).
# + DO NOT `memoise::memoise()` for `*rapm_coefs()` because otherwise the `.get_metrics_cors_grid()`
# function will not work properly!
# + Note that these functions depend on the default value of `f_import` in `.try_import_thing()`.
.try_import_rapm_coefs <-
    function(...) {
      .try_import_thing(
        ...,
        validate = FALSE,
        f_get = NULL,
        path = config$path_rapm_coefs
      )
    }

.try_import_rapm_sz <-
  memoise::memoise(
    function(...) {
      .try_import_thing(
        ...,
        validate = FALSE,
        f_get = NULL,
        path = config$path_rapm_sz
      )
    }
  )

.try_import_rpm_espn <-
  memoise::memoise(
    function(...) {
      .try_import_thing(
        ...,
        validate = FALSE,
        f_get = NULL,
        path = config$path_rpm_espn
      )
    }
  )


.try_import_players_summary_compare <-
  function(...) {
    .try_import_thing(
      ...,
      f_get = NULL,
      path = config$path_players_summary_compare
    )
  }


# non-nbastatr, combined (with `f_get`) ----
# # These `combined` functions are useful for comparing data across seasons.
# # Note that `validate = FALSE` means that only the non-suffixed file is checked for
# # existence for determination of whether to call the `f_get` function.
# .try_import_rpm_espn_combined <-
#   memoise::memoise(
#     function(...) {
#       .try_import_thing(
#         ...,
#         validate = FALSE,
#         f_get = .download_combine_rpm_espn,
#         path = .create_path_combined(config$path_rpm_espn)
#       )
#     }
#   )
#
# .try_import_rapm_sz_combined <-
#   memoise::memoise(
#     function(...) {
#       .try_import_thing(
#         ...,
#         validate = FALSE,
#         f_get = .download_combine_rapm_sz,
#         path = .create_path_combined(config$path_rapm_sz)
#       )
#     }
#   )
#
# .try_import_rapm_coefs_combined <-
#   memoise::memoise(
#     function(...) {
#       .try_import_thing(
#         ...,
#         validate = FALSE,
#         f_get = combine_rapm_coefs,
#         path = .create_path_combined(config$path_rapm_coefs)
#       )
#     }
#   )
