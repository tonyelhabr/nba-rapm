
# .filter_season_type <-
#   function(play_by_play, season_type, ...) {
#     if(!any("season_type" %in% names(play_by_play))) {
#       .display_info(
#         "Not filtering for `season_type` since there is `season_type` column in `play_by_play`.",
#         ...
#       )
#       return(play_by_play)
#     }
#     # TODO: Implement this better (although this seems like it would work).
#     # .validate_season_type(season_type)
#     # browser()
#     # if (season_type == .SEASON_TYPES[3] |
#     #     (season_type != .SEASON_TYPES[1] &
#     #     season_type != .SEASON_TYPES[2])) {
#     #   return(play_by_play)
#     # }
#     # play_by_play %>% filter(season_type == !!season_type)
#     play_by_play %>% filter(season_type == "Regular Season")
#   }

# TODO: Check if this works.s
.clean_play_by_play <-
  function(raw_play_by_play, ...) {
    UseMethod(".clean_play_by_play")
  }

.clean_play_by_play.rd <-
  function(raw_play_by_play,
           ...,
           path_teams_game_logs_nbastatr = config$path_teams_game_logs_nbastatr,
           debug = TRUE) {

    # Use these if running interactively.
    if(FALSE) {
      raw_play_by_play <- .import_data_from_path(season = .SEASON, path = config$path_raw_play_by_play)
      teams_game_logs_nbastatr <- .try_import_teams_game_logs_nbastatr(season = .SEASON)
      teams_nbastatr <- .try_import_teams_nbastatr(season = .SEASON)
      players_nbastatr <- .try_import_players_nbastatr(season = .SEASON)
    } else {
      teams_game_logs_nbastatr <- .try_import_teams_game_logs_nbastatr(...)
      teams_nbastatr <- .try_import_teams_nbastatr(...)
      players_nbastatr <- .try_import_players_nbastatr(...)
    }

    if(FALSE) {
      # Check that final scores are correct.
      raw_game_final_scores <-
        raw_play_by_play %>%
        filter(season_type == "Regular Season") %>%
        filter(play_type == "EndOfPeriod") %>%
        arrange(game_id, period, desc(time_elapsed))  %>%
        group_by(game_id) %>%
        filter(period == max(period)) %>%
        ungroup() %>%
        select(
          id_game = game_id,
          id_team1 = team_id1,
          id_team2 = team_id2,
          # pts_home = home_score,
          # pts_away = away_score
          pts_home = away_score,
          pts_away = home_score
        ) %>%
        mutate_at(vars(matches("pts")), funs(as.integer))

      game_logs_final_scores <-
        left_join(
          teams_game_logs_nbastatr %>%
            group_by(id_game) %>%
            filter(location_game == "H") %>%
            ungroup() %>%
            select(id_game, id_team_home = id_team, slug_home = slug_team, pts_home = pts_team),
          teams_game_logs_nbastatr %>%
            group_by(id_game) %>%
            filter(location_game == "A") %>%
            ungroup() %>%
            select(id_game, id_team_away = id_team, slug_away = slug_team, pts_away = pts_team),
          by = c("id_game")
        )

      game_final_scores_debug <-
        raw_game_final_scores %>%
        left_join(
          game_logs_final_scores,
          by = c("id_game", "pts_home", "pts_away")
        )

      path_export <-
        .export_data_from_path(
          ...,
          data = game_final_scores_debug,
          path = glue::glue("data/debug/game_final_scores_debug.csv")
        )
    }

    if(FALSE) {
      raw_play_by_play <- .import_data_from_path(season = .SEASON, path = config$path_raw_play_by_play)
      raw_play_by_play <-
        raw_play_by_play %>%
        # filter(game_id == 21700007) %>%
        filter(game_id == .ID_GAME_DEBUG) %>%
        arrange(period, time_elapsed)
    }

    play_by_play <-
      raw_play_by_play %>%
      # .filter_season_type(...)
      filter(season_type == "Regular Season") %>%
      filter(event_action_type != 0) %>%
      filter(!is.na(player1team_id)) %>%
      mutate_at(vars(matches("description$")), funs(na_if(., ""))) %>%
      mutate(
        description = coalesce(home_description, away_description)
      ) %>%
      select(
        id_game = game_id,
        period,
        # Rename as "compromise" between differently named column between sources.
        event_num = event_number,
        pc_time_string,
        sec_elapsed = time_elapsed,
        # matches("_score$"),
        # pts_home = home_score,
        # pts_away = away_score,
        pts_home = away_score,
        pts_away = home_score,
        player1_id = player1id,
        player1_id_team = player1team_id,
        # matches("^team_id"),
        id_team1 = team_id1,
        id_team2 = team_id2,
        play_type,
        description,
        matches("team1player[1-5]id$"),
        matches("team2player[1-5]id$")
      ) %>%
      unite(lineup1, matches("team1player"), sep = "-") %>%
      unite(lineup2, matches("team2player"), sep = "-") %>%
      arrange(id_game, period, sec_elapsed, event_num)

    # Do all this for better readability.
    play_by_play <-
      play_by_play %>%
      left_join(
        teams_game_logs_nbastatr %>%
          group_by(id_game) %>%
          filter(id_team == min(id_team)) %>%
          ungroup() %>%
          select(
            id_game,
            id_team1 = id_team,
            id_team2 = id_opponent,
            slug_team1 = slug_team,
            slug_team2 = slug_opponent,
            location_game1 = location_game
          ),
        by = c("id_game", "id_team1", "id_team2")
      ) %>%
      left_join(
        players_nbastatr %>%
          select(player1_id = id_player, name_player1 = name_player),
        by = c("player1_id")
      ) %>%
      left_join(
        teams_nbastatr %>%
          select(player1_id_team = id_team, slug_team_player1 = slug_team),
        by = c("player1_id_team")
      ) %>%
      select(
        id_game,
        slug_team1,
        slug_team2,
        name_player1,
        slug_team_player1,
        play_type,
        description,
        everything()
      )

    play_by_play <-
      play_by_play %>%
      # filter(play_type %in% c("Make", "Miss", "FreeThrow")) %>%
      filter(!play_type %in% c("Foul", "Rebound")) %>%
      # This isn't really necessary since `play_type` isn't explicitly used
      # to distinguish makes and misses.
      # mutate_at(
      #   vars(play_type),
      #   funs(case_when(
      #     . == "FreeThrow" ~ "Make",
      #     . == "Turnover" ~ "Miss",
      #     TRUE ~ .
      #   )
      #   )
      # ) %>%
      # Aggregate free throws.
      group_by(id_game, player1_id, play_type, sec_elapsed) %>%
      filter(row_number() == n()) %>%
      ungroup() %>%
      mutate(rn = row_number()) %>%
      select(rn, everything())

    if(FALSE) {
      # Checking time intervals.
      play_by_play %>%
        filter(
          id_game == dplyr::lead(id_game) &
            period == dplyr::lead(period),
          (sec_elapsed > dplyr::lead(sec_elapsed))
          # (sec_elapsed < dplyr::lag(sec_elapsed))
        )
      play_by_play %>%
        filter(
          id_game == dplyr::lag(id_game) &
            period == dplyr::lag(period) &
            # (sec_elapsed > dplyr::lead(sec_elapsed))
            (sec_elapsed < dplyr::lag(sec_elapsed))
        )
    }

    # This is the key step/assumption (but it SHOULD be correct due to the pre-processing).
    play_by_play <-
      play_by_play %>%
      mutate(
        is_off1 = if_else(player1_id_team == id_team1, 1L, 0L),
        is_home1 = if_else(location_game1 == "H", TRUE, FALSE)
      ) %>%
      mutate(
        pts_team1 = if_else(is_home1, pts_home, pts_away),
        pts_team2 = if_else(is_home1, pts_away, pts_home)
      ) %>%
      select(matches("^pts"), everything())

    # play_by_play <-
    #   play_by_play %>%
    #   select(-matches("^is_home1$|^pts_home[12]$"))
    if(debug) {
      .export_data_from_path(
        ...,
        data = play_by_play %>% filter(id_game == .ID_GAME_DEBUG),
        path = glue::glue("data/debug/play_by_play_clean1_filt.csv")
      )
    }

    play_by_play <-
      play_by_play %>%
      group_by(id_game) %>%
      mutate(poss_num = row_number()) %>%
      mutate(mp = (sec_elapsed - dplyr::lag(sec_elapsed, n = 1L, default = 0L)) / 60) %>%
      # fill(pts_home) %>%
      # fill(pts_away) %>%
      fill(pts_team1) %>%
      fill(pts_team2) %>%
      ungroup() %>%
      select(-sec_elapsed) %>%
      mutate_at(vars(matches("^pts_team|mp")), funs(coalesce(., 0))) %>%
      mutate_at(vars(matches("^pts")), funs(as.integer))

    play_by_play <-
      play_by_play %>%
      group_by(id_game) %>%
      mutate(
        pts1 = pts_team1 - dplyr::lag(pts_team1, default = 0L),
        pts2 = pts_team2 - dplyr::lag(pts_team2, default = 0L)
      ) %>%
      ungroup() %>%
      mutate(pts12 = pts1 + pts2) %>%
      select(matches("^pts"), everything())

    nms <- play_by_play %>% names()
    nms_last <-
      c(
        nms %>% str_subset("^lineup[12]$"),
        nms %>% str_subset("^id_team[12]$"),
        nms %>% str_subset("^player1_id")
      )
    nms_first <- nms %>% str_subset("^pts|is_off")
    nms_mid <- nms %>% setdiff(c(nms_first, nms_last))

    play_by_play <-
      play_by_play %>%
      select(one_of(c(nms_first, nms_mid, nms_last)))

    if(debug) {
      .export_data_from_path(
        ...,
        data = play_by_play %>% filter(id_game == .ID_GAME_DEBUG),
        path = glue::glue("data/debug/play_by_play_clean2_filt.csv")
      )
    }

    play_by_play
  }

clean_play_by_play <-
  function(path_raw_play_by_play,
           path_play_by_play = config$path_play_by_play,
           raw_data_source = .RAW_DATA_SOURCE,
           ...) {
    will_skip <-
      .try_skip(
        ...,
        path_reqs =
          c(path_raw_play_by_play),
        path_deps =
          c(path_play_by_play)
      )

    if(will_skip) {
      return(invisible(NULL))
    }
    path_from <- .get_path_from(path = path_raw_play_by_play, ...)

    raw_play_by_play <-
      .import_data_from_path(
        ...,
        path = path_raw_play_by_play
      )

    .validate_raw_data_source(raw_data_source)

    class(raw_play_by_play) <- append(class(raw_play_by_play), raw_data_source)
    play_by_play <-
      .clean_play_by_play(
        raw_play_by_play = raw_play_by_play,
        path_raw_teams_game_logs_nbastatr = path_raw_teams_game_logs_nbastatr,
        ...
      )

    path_export <-
      .export_data_from_path(
        ...,
        data = play_by_play,
        path = path_play_by_play
      )

    invisible(play_by_play)
  }

auto_clean_play_by_play <-
  purrr::partial(
    clean_play_by_play,
    path_raw_play_by_play = config$path_raw_play_by_play,
    season = config$season,
    season_type = config$season_type,
    # raw_data_source = config$raw_data_source,
    skip = config$skip_clean,
    verbose = config$verbose,
    export = config$export,
    backup = config$backup,
    clean = config$clean,
    n_keep = config$n_keep
  )

