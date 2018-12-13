
# .get ----
# Note that paths are "hard-coded" here to allow for flexible "offline"
# execution of this function. This design choice should/could be reconsidered in the future.
.get_players_nbastatr <-
  function(..., season = .SEASON, path) {
    .validate_season(season)

    res <-
      nbastatR::nba_players() %>%
      janitor::clean_names()

    res <-
      res %>%
      filter(
        year_season_first <= season,
        year_season_last >= season
      )

    path_export <-
      .export_data_from_path(
        ...,
        season = season,
        data = res,
        path = path
      )
    invisible(res)
  }

.get_teams_nbastatr <-
  function(..., season = .SEASON, path) {
    .validate_season(season)

    res <-
      nbastatR::nba_teams() %>%
      janitor::clean_names() %>%
      filter(is_non_nba_team == 0L)

    res <-
      res %>%
      filter(year_played_last >= season)

    path_export <-
      .export_data_from_path(
        ...,
        season = season,
        data = res,
        path = path
      )
    invisible(res)
  }

.get_teams_game_logs_nbastatr <-
  function(...,
           season = .SEASON,
           season_type = .SEASON_TYPE,
           path) {
    .validate_season(season)
    .validate_season_type(season_type)
    season_type_nm <- .convert_season_type(season_type)

    # TODO: Better handle case where `season_type` is `NULL`?
    # Not exactly sure how this would be done, since `{nbastatR}`
    # has a default value for this parameter.
    res <-
      nbastatR::game_logs(
        seasons = season + 1,
        result_types = "team",
        season_types = season_type_nm,
        assign_to_environment = FALSE,
        return_message = FALSE
      ) %>%
      janitor::clean_names()

    # # Note that this is done soley to get `id_team_opponent`.
    res <-
      res %>%
      left_join(
        res %>%
          select(id_game, id_opponent = id_team, slug_opponent = slug_team),
        by = c("id_game", "slug_opponent")
      ) %>%
      arrange(id_game, id_team)

    path_export <-
      .export_data_from_path(
        ...,
        season = season,
        season_type = season_type,
        data = res,
        path = path
      )

    invisible(res)
  }

.get_players_game_logs_nbastatr <-
  function(...,
           season = .SEASON,
           season_type = .SEASON_TYPE,
           path) {
    .validate_season(season)
    .validate_season_type(season_type)
    season_type_nm <- .convert_season_type(season_type)

    res <-
      nbastatR::game_logs(
        seasons = season + 1,
        result_types = "player",
        season_types = season_type_nm,
        assign_to_environment = FALSE,
        return_message = FALSE
      ) %>%
      janitor::clean_names()

    path_export <-
      .export_data_from_path(
        ...,
        season = season,
        season_type = season_type,
        data = res,
        path = path
      )

    invisible(res)
  }


.get_teams_summary_nbastatr <-
  function(..., season = .SEASON, path) {

    # Not working for 2017 and beyond.
    # res <-
    #   nbastatR::bref_teams_stats(
    #     seasons = season + 1,
    #     assign_to_environment = FALSE,
    #     return_message = FALSE
    #   ) %>%
    #   unnest() %>%
    #   janitor::clean_names()

    teams <-
      .try_import_teams_nbastatr(season = season)
    id_teams <- teams %>% pull(id_team)

    res <-
      nbastatR::teams_annual_stats(
        team_ids = id_teams,
        all_active_teams = TRUE,
        season_types = c("Regular Season"),
        modes = c("Totals"),
        return_message = FALSE,
        nest_data = FALSE
      ) %>%
      janitor::clean_names() %>%
      mutate_at(
        vars(slug_season),
        funs(paste0(str_sub(., 1, 2), str_sub(., 6)) %>% as.integer())
      ) %>%
      filter(slug_season == season)

    .export_data_from_path(
      ...,
      season = season,
      data = res,
      path = path
    )
    invisible(res)
  }

.get_players_summary_nbastatr <-
  function(..., season = .SEASON, path) {

    res <-
      nbastatR::bref_players_stats(
        seasons = season + 1,
        assign_to_environment = FALSE,
        return_message = FALSE
      ) %>%
      unnest() %>%
      janitor::clean_names()

    .export_data_from_path(
      ...,
      season = season,
      data = res,
      path = path
    )
    invisible(res)
  }

# .try_import_thing ----
.try_import_thing <-
  function(...,
           path,
           f_import = .import_data_from_path,
           f_get) {

    res <- attempt::try_catch(expr = f_import(..., path = path), .e = NULL)

    if(!is.null(res)) {
      return(invisible(res))
    }

    .display_info(
      glue::glue("Could not get data with `f_import`. Trying `f_get`."),
      ...
    )

    res <- attempt::try_catch(expr = f_get(..., path = path), .e = NULL)

    if(!is.null(res)) {
      return(invisible(res))
    }

    .display_error(
      glue::glue("Could not get data with `f_get` after failing with `f_import`."),
      ...
    )
    stop(call. = FALSE)
  }

.try_import_players_nbastatr <-
  function(...) {
    .try_import_thing(
      ...,
      f_get = .get_players_nbastatr,
      path = config$path_players_nbastatr
    )
  }

.try_import_teams_nbastatr <-
  function(...) {
    .try_import_thing(
      ...,
      f_get = .get_teams_nbastatr,
      path = config$path_teams_nbastatr
    )
  }

.try_import_players_game_logs_nbastatr <-
  function(...) {
    .try_import_thing(
      ...,
      f_get = .get_players_game_logs_nbastatr,
      path = config$path_players_game_logs_nbastatr
    )
  }

.try_import_teams_game_logs_nbastatr <-
  function(...) {
    .try_import_thing(
      ...,
      f_get = .get_teams_game_logs_nbastatr,
      path = config$path_teams_game_logs_nbastatr
    )
  }

.try_import_players_summary_nbastatr <-
  function(...) {
    .try_import_thing(
      ...,
      f_get = .get_players_summary_nbastatr,
      path = config$path_players_summary_nbastatr
    )
  }

.try_import_teams_summary_nbastatr <-
  function(...) {
    .try_import_thing(
      ...,
      f_get = .get_teams_summary_nbastatr,
      path = config$path_teams_summary_nbastatr
    )
  }

