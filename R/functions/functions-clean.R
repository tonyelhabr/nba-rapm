
.filter_season_type <-
  function(play_by_play, season_type = .SEASON_TYPES, ...) {
    season_type = match.arg(season_type)
    if (season_type == .SEASON_TYPES[3] |
        (season_type != .SEASON_TYPES[1] &
        season_type != .SEASON_TYPES[2])) {
      return(play_by_play)
    }
    play_by_play %>% filter(season_type %in% !!season_type)
  }

clean_raw_play_by_play <-
  function(season,
           path_raw_play_by_play_format,
           path_raw_game_summary_format,
           ...,
           skip = .SKIP,
           verbose = .VERBOSE,
           export = .EXPORT,
           path_play_by_play_format) {

    will_skip <-
      .try_skip(
        skip = skip,
        season = season,
        path_format_reqs =
          c(
            path_raw_play_by_play_format,
            path_raw_game_summary_format
          ),
        path_format_deps =
          c(
            path_play_by_play_format
          ),
        verbose = verbose,
        # call_name = rlang::call_name(), # Haven't really tested this.
        ...
      )
    if(will_skip) {
      return(invisible(NULL))
    }

    raw_play_by_play <-
      .import_data_from_path_format(
        path_format = path_raw_play_by_play_format,
        season = season,
        verbose = verbose
      )

    raw_game_summary <-
      .import_data_from_path_format(
        path_format = path_raw_game_summary_format,
        season = season,
        verbose = verbose
      )

    # TODO: Implement some kind of checking of column names/types for this function?
    play_by_play <-
      raw_play_by_play %>%
      .filter_season_type() %>%
      filter(!is.na(player1team_id))

    play_by_play <-
      play_by_play %>%
      select(
        game_id,
        period,
        event_number,
        sec_elapsed = time_elapsed,
        matches("_score$"),
        player1team_id,
        matches("^team_id"),
        matches("team1player[1-5]id$"),
        matches("team2player[1-5]id$"),
        play_type,
        matches("_description$")
      ) %>%
      unite(lineup1, matches("team1player"), sep = "-") %>%
      unite(lineup2, matches("team2player"), sep = "-") %>%
      mutate(
        is_off1 = if_else(player1team_id == team_id1, 1L, 0L)
      ) %>%
      rename(
        pts_home = home_score,
        pts_away = away_score,
        tm_id1 = team_id1,
        tm_id2 = team_id2
      ) %>%
      arrange(game_id, period, event_number) %>%
      select(-event_number, -player1team_id)

    play_by_play <-
      play_by_play %>%
      filter(play_type %in% c("Make", "Miss", "FreeThrow")) %>%
      mutate_at(vars(play_type), funs(if_else(. == "FreeThrow", "Make", .)))

    play_by_play <-
      play_by_play %>%
      group_by(game_id, period, sec_elapsed) %>%
      summarise_at(vars(matches("^pts_|^tm_|is_off1|^lineup")), funs(dplyr::last)) %>%
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

    game_summary <-
      raw_game_summary %>%
      .filter_season_type()

    game_summary <-
      game_summary %>%
      select(
        game_id,
        # season,
        # season_type,
        tm_id_home = home_team_id,
        tm_id_away = away_team_id
      )


    play_by_play <-
      play_by_play %>%
      inner_join(
        game_summary,
        by = "game_id"
      ) %>%
      mutate(
        is_home1 = if_else(tm_id1 == tm_id_home, TRUE, FALSE)
      ) %>%
      select(-matches("^tm_id_")) %>%
      mutate(
        pts_tm1 = if_else(is_home1, pts_home, pts_away),
        pts_tm2 = if_else(!is_home1, pts_home, pts_away)
      ) %>%
      select(-is_home1) %>%
      group_by(game_id) %>%
      mutate(
        pts1 = pts_tm1 - dplyr::lag(pts_tm1, default = 0L),
        pts2 = pts_tm2 - dplyr::lag(pts_tm2, default = 0L)
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
        # tm_id1,
        # tm_id2,
        # pts1,
        # pts2,
        pts,
        lineup1,
        lineup2
      )

    path_export <-
      .export_data_from_path_format(
        data = play_by_play,
        path_format = path_play_by_play_format,
        season = season,
        verbose = verbose,
        export = export,
        ...
      )

    invisible(play_by_play)
  }

auto_clean_raw_play_by_play <-
  purrr::partial(
    clean_raw_play_by_play,
    season = args$season,
    path_raw_play_by_play_format = args$path_raw_play_by_play_format,
    path_raw_game_summary_format = args$path_raw_game_summary_format,
    path_play_by_play_format = args$path_play_by_play_format,
    season_type = args$season_type,
    skip = args$skip_clean,
    verbose = args$verbose,
    export = args$export_clean
  )

