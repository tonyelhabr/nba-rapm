
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

clean_play_by_play <-
  function(...,
           # Some of these are included here exclusively for the `.try_skip()` function.
           # This is true with some of the other functions as well.
           path_raw_play_by_play = config$path_raw_play_by_play,
           path_play_by_play = config$path_play_by_play,
           path_game_final_scores_compare = config$path_game_final_scores_compare,
           path_play_by_play_error = config$path_play_by_play_error) {

    will_skip <-
      .try_skip(
        ...,
        path_reqs =
          c(
            .get_path_from(..., path = path_raw_play_by_play)
          ),
        path_deps =
          c(
            .get_path_from(..., path = path_play_by_play)
          )
      )

    if(will_skip) {
      return(invisible(NULL))
    }

    .display_progress(
      glue::glue("Step 1: Cleaning play-by-play data."),
      ...
    )

    raw_play_by_play <-
      .import_data_from_path(
        ...,
        path = path_raw_play_by_play
      )

    teams_game_logs_nbastatr <- .try_import_teams_game_logs_nbastatr(...)
    teams_nbastatr <- .try_import_teams_nbastatr(...)
    players_nbastatr <- .try_import_players_nbastatr(...)

    # Check that final scores are correct.
    raw_game_final_scores <-
      raw_play_by_play %>%
      # .filter_season_type(...) %>%
      filter(season_type == "Regular Season") %>%
      filter(play_type == "EndOfPeriod") %>%
      arrange(game_id, period, desc(time_elapsed))  %>%
      group_by(game_id) %>%
      filter(period == max(period)) %>%
      ungroup() %>%
      # Note that `score` columns are flipped.
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
          select(
            id_game,
            id_team_home = id_team,
            slug_home = slug_team,
            pts_home = pts_team
          ),
        teams_game_logs_nbastatr %>%
          group_by(id_game) %>%
          filter(location_game == "A") %>%
          ungroup() %>%
          select(
            id_game,
            id_team_away = id_team,
            slug_away = slug_team,
            pts_away = pts_team
          ),
        by = c("id_game")
      )

    game_final_scores_compare <-
      raw_game_final_scores %>%
      anti_join(
        game_logs_final_scores,
        by = c("id_game", "pts_home", "pts_away")
      )

    id_game_bad <-
      game_final_scores_compare %>%
      pull(id_game)

    path_export <-
      .export_data_from_path(
        ...,
        data = game_final_scores_compare,
        path = path_game_final_scores_compare
      )

    # if(FALSE) {
    #   raw_play_by_play <-
    #     .import_data_from_path(
    #       season = .SEASON,
    #       path = config$path_raw_play_by_play
    #     )
    #   raw_play_by_play <-
    #     raw_play_by_play %>%
    #     filter(game_id == .ID_GAME_DEBUG) %>%
    #     arrange(period, time_elapsed)
    # }

    play_by_play <-
      raw_play_by_play %>%
      # .filter_season_type(...)
      filter(season_type == "Regular Season") %>%
      # Throw out completely mis-labeled points (e.g. 21600236).
      filter(!game_id %in% id_game_bad) %>%
      # Some of these "relevant" `play_type`s are mis-labeled with `event_action_type = 0`.
      filter(event_action_type != 0 |
               c(play_type %in% c("Make", "Miss", "Turnover", "Timeout", "Ejection", "Foul"))) %>%
      filter(!is.na(player1team_id)) %>%
      mutate_at(vars(matches("description$")), funs(na_if(., ""))) %>%
      mutate(
        description = coalesce(home_description, away_description)
      ) %>%
      select(
        id_game = game_id,
        period,
        event_num = event_number,
        pc_time_string,
        sec_elapsed = time_elapsed,
        pts_home = away_score,
        pts_away = home_score,
        player1_id = player1id,
        player1_id_team = player1team_id,
        id_team1 = team_id1,
        id_team2 = team_id2,
        play_type,
        description,
        matches("team1player[1-5]id$"),
        matches("team2player[1-5]id$")
      ) %>%
      unite(lineup1, matches("team1player"), sep = "-") %>%
      unite(lineup2, matches("team2player"), sep = "-") %>%
      # Do this because `event_num` is not completely reliable for sorting.
      mutate(pts_total = pts_home + pts_away) %>%
      arrange(id_game, period, sec_elapsed, pts_total, event_num) %>%
      group_by(id_game) %>%
      fill(pts_home) %>%
      fill(pts_away) %>%
      ungroup()

    # Do this for better readability, as well as to get the all important
    # `location_game`, which cannot be derived directly from the raw data.
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
        pts_home,
        pts_away,
        id_game,
        slug_team1,
        slug_team2,
        name_player1,
        slug_team_player1,
        play_type,
        description,
        everything()
      ) %>%
      # There is something weird where id_team[1|2] becomes wrong mid-game. (e.g. 21601080)
      fill(slug_team1) %>%
      fill(slug_team2) %>%
      fill(location_game1) %>%
      # Note that somehow things may become unsorted a bit.
      arrange(id_game, period, sec_elapsed, pts_total, event_num)

    play_by_play <-
      play_by_play %>%
      filter(!play_type %in% c("Foul", "Rebound")) %>%
      # Aggregate free throws.
      group_by(id_game, player1_id, play_type, sec_elapsed) %>%
      # Be careful with missed second/third free-throws!
      # (use `fill()` before this).
      filter(row_number() == n()) %>%
      ungroup() %>%
      mutate(rn = row_number()) %>%
      select(rn, everything())

    # This is the key step/assumption (but it SHOULD be correct due to the pre-processing).
    play_by_play <-
      play_by_play %>%
      mutate(
        is_off1 = if_else(player1_id_team == id_team1, TRUE, FALSE),
        is_home1 = if_else(location_game1 == "H", TRUE, FALSE)
      ) %>%
      mutate(
        pts_team1 = if_else(is_home1, pts_home, pts_away),
        pts_team2 = if_else(is_home1, pts_away, pts_home)
      ) %>%
      # Re-order the columns just to make things easier to debug.
      # (This is done in other places that follow as well.)
      select(matches("^is_"), everything()) %>%
      select(matches("^pts"), everything())

    play_by_play <-
      play_by_play %>%
      group_by(id_game) %>%
      mutate(poss_num = row_number()) %>%
      mutate(
        mp = (sec_elapsed - dplyr::lag(sec_elapsed, n = 1L, default = 0L)) / 60
      ) %>%
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
      select(matches("pts"), everything())

    # Not sure if there is really a good way to fix this.
    # Checked one game on bref and it seemed to be a record error.
    path_export <-
      .export_data_from_path(
        ...,
        data = play_by_play %>% filter(pts1 > 0, pts2 > 0),
        path = path_play_by_play_error
      )

    suffix12 <- as.character(1:2)
    cols_keep <-
      c(
        "rn",
        paste0(c("is_off", "is_home"), "1"),
        paste0("pts", suffix12),
        "mp",
        "id_game",
        "period",
        paste0("id_team", suffix12),
        paste0("slug_team", suffix12),
        paste0("lineup", suffix12)
      )

    # # Other possibly useful columns?
    # cols_order <- c(cols_order, "description", "pc_time_string", "poss_num")
    play_by_play <-
      play_by_play %>%
      select(one_of(cols_keep))

    path_export <-
      .export_data_from_path(
        ...,
        data = play_by_play,
        path = path_play_by_play
      )

    invisible(play_by_play)
  }

clean_play_by_play_auto <-
  function(...,
           season = config$season,
           season_type = config$season_type,
           skip = config$skip,
           verbose = config$verbose,
           export = config$export,
           backup = config$backup,
           clean = config$clean,
           n_keep = config$n_keep) {
    clean_play_by_play(
      # ...,
      season = season,
      season_type = season_type,
      skip = skip,
      verbose = verbose,
      export = export,
      backup = backup,
      clean = clean,
      n_keep = n_keep
    )
  }
