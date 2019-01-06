

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
    res
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
    res
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

    res
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

    res
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
    res
  }

# Use something like this to get player-slugs for a specific year?
.get_players_details_nbastatr <-
  function(..., season = .SEASON, path) {
    players <- nbastatR::dictionary_bref_players()
    url <-
      players %>%
      filter(namePlayerBREF == "Kevin Love") %>%
      pull(urlPlayerBioBREF)
    page_html <-
      url %>%
      xml2::read_html()
    table <-
      page_html %>%
      rvest::html_table() %>%
      pluck(1) %>%
      as_tibble()
  }

.get_players_summary_nbastatr <-
  function(..., season = .SEASON, path) {

    # Note that this will still assign `df_dict_nba_players` to the .GlobalEnv
    suppressWarnings(
      suppressMessages(
        res <-
          nbastatR::bref_players_stats(
            seasons = season + 1,
            # tables = c("advanced", "totals"), # default
            # include_all_nba = FALSE, # default
            # only_totals = TRUE, # default
            # nest_data = TRUE, # default
            nest_data = FALSE,
            # widen_data = TRUE, # default
            # join_data = TRUE, # default
            assign_to_environment = FALSE,
            return_message = FALSE
          ) %>%
          # unnest() %>%
          janitor::clean_names()
      )
    )

    .export_data_from_path(
      ...,
      season = season,
      data = res,
      path = path
    )
    res
  }

# Note that this is a bit different since it depends on `players_summary_nbastatr`
# already existing.
.get_players_bpm_nbastatr <-
  function(..., path) {
    players_summary_nbastatr <- .try_import_players_summary_nbastatr(...)
    res <-
      players_summary_nbastatr %>%
      select(id = id_player_nba, name = name_player, matches("ratio.*pm$")) %>%
      rename_at(vars(matches("ratio.*pm$")), funs(str_remove(., "ratio_")))
    .export_data_from_path(
      ...,
      season = season,
      data = res,
      path = path
    )
    res
  }

# Note that this is also different since it depends on `players_summary_compare`
# already existing.
.get_players_pm <-
  function(..., path) {
    players_summary_compare <- .try_import_players_summary_compare(...)
    res <-
      players_summary_compare
    .export_data_from_path(
      ...,
      season = season,
      data = res,
      path = path
    )
    res
  }

