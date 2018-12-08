
clean_raw_data <-
  function(season,
           path_data_raw_format,
           path_game_summary_raw_format,
           ...,
           skip = .SKIP,
           verbose = .VERBOSE,
           export = .EXPORT,
           path_data_clean_format) {

    if(skip) {
      return(invisible(NULL))
    }

    data_raw <-
      .import_data_from_path_format(
        path_format = path_data_raw_format,
        season = season,
        verbose = verbose
      )

    game_summary_raw <-
      .import_data_from_path_format(
        path_format = path_game_summary_raw_format,
        season = season,
        verbose = verbose
      )

    data_select <-
      data_raw %>%
      filter(!is.na(player1team_id)) %>%
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
    data_select

    data_select <-
      data_select %>%
      filter(play_type %in% c("Make", "Miss", "FreeThrow")) %>%
      mutate_at(vars(play_type), funs(if_else(. == "FreeThrow", "Make", .)))
    data_select

    data_select <-
      data_select %>%
      group_by(game_id, period, sec_elapsed) %>%
      summarise_at(vars(matches("^pts_|^tm_|is_off1|^lineup")), funs(dplyr::last)) %>%
      ungroup()
    data_select

    data_calc <-
      data_select %>%
      group_by(game_id) %>%
      mutate(poss_num = row_number()) %>%
      mutate(min_poss = (sec_elapsed - lag(sec_elapsed, 1)) / 60) %>%
      fill(pts_home) %>%
      fill(pts_away) %>%
      ungroup() %>%
      select(-sec_elapsed) %>%
      mutate_at(vars(matches("^pts|min_poss")), funs(coalesce(., 0))) %>%
      mutate_at(vars(matches("^pts")), funs(as.integer))
    data_calc


    game_summary <-
      game_summary_raw %>%
      select(
        game_id,
        # season,
        # season_type,
        tm_id_home = home_team_id,
        tm_id_away = away_team_id
      )

    data_join <-
      data_calc %>%
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
    data_join

    data <-
      data_join %>%
      mutate(pts = pts1 + pts2) %>%
      select(
        game_id,
        period,
        # poss_num,
        min_poss,
        is_off = is_off1,
        # tm_id1,
        # tm_id2,
        # pts1,
        # pts2,
        pts,
        lineup1,
        lineup2
      )

    if (export) {
      path_data_clean <-
        .export_data_from_path_format(
          data = data,s
          path_format = path_data_clean_format,
          season = season,
          verbose = verbose
        )
    }

    data
  }

do_clean_raw_data <-
  purrr::partial(
    clean_raw_data,
    season = args$season,
    path_data_raw_format = args$path_data_raw_format,
    path_game_summary_raw_format = args$path_game_summary_raw_format,
    skip = args$skip_clean,
    verbose = args$verbose,
    export = args$export_clean,
    path_data_clean_format = args$path_data_clean_format
  )

