
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
.clean_raw_play_by_play <-
  function(raw_play_by_play, ...) {
    UseMethod(".clean_raw_play_by_play")
  }

.clean_raw_play_by_play.rd <-
  function(raw_play_by_play,
           ...,
           path_teams_game_logs_nbastatr = config$path_teams_game_logs_nbastatr,
           debug = config$debug) {

    # TODO: Implement some kind of checking of column names/types for this function?
    play_by_play <-
      raw_play_by_play %>%
      # .filter_season_type(...)
      filter(season_type == "Regular Season")

    play_by_play <-
      play_by_play %>%
      filter(!is.na(player1team_id)) %>%
      select(
        game_id,
        period,
        # Rename as "compromise" between differently named column between sources.
        event_num = event_number,
        sec_elapsed = time_elapsed,
        # matches("_score$"),
        pts_home = home_score,
        pts_away = away_score,
        player1_team_id = player1team_id,
        # matches("^team_id"),
        team_id1 = team_id1,
        team_id2 = team_id2,
        matches("team1player[1-5]id$"),
        matches("team2player[1-5]id$"),
        play_type,
        matches("_description$")
      ) %>%
      unite(lineup1, matches("team1player"), sep = "-") %>%
      unite(lineup2, matches("team2player"), sep = "-") %>%
      mutate(
        is_off1 = if_else(player1_team_id == team_id1, 1L, 0L)
      ) %>%
      arrange(game_id, period, event_num) %>%
      select(-matches("^event_num$|^player1_team_id$"))

    play_by_play <-
      play_by_play %>%
      filter(play_type %in% c("Make", "Miss", "FreeThrow")) %>%
      mutate_at(vars(play_type), funs(if_else(. == "FreeThrow", "Make", .)))

    play_by_play <-
      play_by_play %>%
      group_by(game_id, period, sec_elapsed) %>%
      summarise_at(vars(matches("^pts_|^team_|is_off1|^lineup")), funs(dplyr::last)) %>%
      ungroup()

    play_by_play <-
      play_by_play %>%
      group_by(game_id) %>%
      mutate(poss_num = row_number()) %>%
      mutate(mp = (sec_elapsed - lag(sec_elapsed, 1)) / 60) %>%
      fill(pts_home) %>%
      fill(pts_away) %>%
      ungroup() %>%
      select(-sec_elapsed) %>%
      mutate_at(vars(matches("^pts|mp")), funs(coalesce(., 0))) %>%
      mutate_at(vars(matches("^pts")), funs(as.integer))

    teams_game_logs_nbastatr <-
      .try_import_teams_game_logs_nbastatr(...)

    # Need to join just to get the home/away data.
    if(debug) {
      n_row_before <- nrow(play_by_play)
    }

    # Do this just to `location_game`.
    play_by_play <-
      play_by_play %>%
      inner_join(
        teams_game_logs_nbastatr %>% select(id_game, id_team, location_game),
        by = c("game_id" = "id_game", "team_id1" = "id_team")
      )

    if(debug) {
      n_row_after <- nrow(play_by_play)
      .compare_n_row_eq(
        n1 = n_row_before,
        n2 = n_row_after
      )
    }

    play_by_play <-
      play_by_play %>%
      mutate(
        is_home1 = if_else(location_game == "H", TRUE, FALSE)
      ) %>%
      select(-matches("^team_id_")) %>%
      mutate(
        pts_team1 = if_else(is_home1, pts_home, pts_away),
        pts_team2 = if_else(!is_home1, pts_home, pts_away)
      ) %>%
      select(-is_home1) %>%
      group_by(game_id) %>%
      mutate(
        pts1 = pts_team1 - dplyr::lag(pts_team1, default = 0L),
        pts2 = pts_team2 - dplyr::lag(pts_team2, default = 0L)
      ) %>%
      ungroup()

    play_by_play <-
      play_by_play %>%
      mutate(pts = pts1 + pts2) %>%
      select(
        game_id,
        period,
        # poss_num,
        mp,
        is_off = is_off1,
        # team_id1,
        # team_id2,
        # pts1,
        # pts2,
        pts,
        lineup1,
        lineup2
      )
    play_by_play
  }

clean_raw_play_by_play <-
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
      .clean_raw_play_by_play(
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

auto_clean_raw_play_by_play <-
  purrr::partial(
    clean_raw_play_by_play,
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

