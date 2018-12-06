

clean_play_by_play_data <-
  function(
    path_play_by_play_raw = NULL,
    path_game_summary_raw = NULL,
    season = NULL,
    path_play_by_play_raw_format = NULL,
    path_game_summary_raw_format = NULL,
    ...,
    verbose = TRUE,
    export = TRUE,
    path_data_clean = NULL,
    path_data_clean_format = NULL
  ) {

    path_play_by_play_raw <-
      .get_path_ifnull(
        path_play_by_play_raw,
        path_play_by_play_raw_format,
        season
      )
    path_game_summary_raw <-
      .get_path_ifnull(
        path_game_summary,
        path_game_summary_raw_format,
        season
      )
    if(export) {
      path_data_clean <-
        .get_path_ifnull(
          path_data_clean,
          path_data_clean_format,
          season
        )
    }

    data_raw <- path_play_by_play_raw %>% .import_path(verbose = verbose)
    game_summary_raw <- path_game_summary_raw <- .import_path(verbose = verbose)

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
    if(export) {
      .export_path(
        data,
        path = path_data_clean,
        verbose = verbose
      )
    }

    data
  }


.separate_lineup <-
  function(data, col, prefix = "x", suffix = 1:5, sep = "-") {
    data %>%
      separate(!!enquo(col), into = paste0(prefix, suffix), sep = sep)
  }

widen_play_by_play_data <-
  function(data = NULL,
           path_format = NULL,
           season = NULL,
           ...,
           verbose = TRUE,
           path = NULL,
           export = TRUE,
           path_export = NULL,
           path_export_format = NULL) {


    data <-
      .import_data_ifnull(
      data = data,
      path_format = path_format,
      season = season,
      verbose = verbose
    )

    data <-
      data %>%
      .separate_lineup(lineup1, suffix = 1:5) %>%
      .separate_lineup(lineup2, suffix = 6:10) %>%
      mutate_at(vars(matches("^x")), funs(as.integer))
    data

    data <-
      data %>%
      mutate(poss_num = row_number()) %>%
      gather(dummy, id, matches("^x")) %>%
      select(-dummy) %>%
      mutate(side = if_else(is_off == 0L, "d", "o")) %>%
      mutate(xid = sprintf("%s%07d", side, as.integer(id))) %>%
      select(-is_off) %>%
      mutate(dummy = 1L)
    data
  }

compute_rapm_estimate <-
  function(data = NULL,
           path_format = NULL,
           season = NULL,
           ...,
           verbose = TRUE,
           path = NULL,
           export = TRUE,
           path_export = NULL,
           path_export_format = NULL) {

  }

combine_rapm_estimates <-
  function(data = NULL,
           path_format = NULL,
           season = NULL,
           ...,
           verbose = TRUE,
           path = NULL,
           export = TRUE,
           path_export = NULL,
           path_export_format = NULL) {

  }


