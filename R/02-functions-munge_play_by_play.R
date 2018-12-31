
.summarise_lineup <-
  function(...,
           play_by_play,
           path_lineup_summary_calc = config$path_lineup_summary_calc) {
    players_nbastatr <- .try_import_players_nbastatr(...)

    play_by_play_aug <-
      play_by_play %>%
      # filter(id_game == .ID_GAME_compare) %>%
      # filter(id_game >= 21700001, id_game <= 21700004) %>%
      # filter(slug_team1 == "SAS" | slug_team2 == "SAS") %>%
      left_join(
        # To get `name_player`.
        players_nbastatr %>%
          select(id_player, name_player) %>%
          mutate_at(
            vars(name_player),
            funs(paste0(str_sub(., 1, 1), ". ", str_replace_all(., "^.*\\s+", "")))
          ),
        by = c("id_player")
      ) %>%
      arrange(id_game, poss_num)

    play_by_play_aug <-
      play_by_play_aug %>%
      group_by(id_game, poss_num) %>%
      mutate(player_num = row_number()) %>%
      ungroup() %>%
      mutate_at(vars(player_num), funs(sprintf("x%02d", .)))

    .convert_lineup_to_suffix <-
      function(x) {
        x <- rlang::enquo(x)
        x_chr <- rlang::quo_text(x)
        str_sub(x_chr, start = nchar(x_chr))
      }
    .unite_lineup <-
      function(.data,
               col,
               ...,
               prefix_rgx = "x",
               sep = " - ",
               remove = TRUE) {
        col_quo <- rlang::enquo(col)
        suffix <- .convert_lineup_to_suffix(!!col_quo)
        suffix_rgx <-
          case_when(suffix == "1" ~ "0[1-5]",
                    suffix == "2" ~ "[01][06-9]")
        rgx <- glue::glue("{prefix_rgx}{suffix_rgx}")
        .data %>%
          unite(!!col_quo, matches(rgx), sep = sep, remove = remove, ...)
      }

    play_by_play_compare_wide <-
      play_by_play_aug %>%
      select(id_game, poss_num, pts, player_num, name_player) %>%
      spread(player_num, name_player) %>%
      .unite_lineup(lineup1) %>%
      .unite_lineup(lineup2)

    play_by_play_aug <-
      play_by_play_aug %>%
      select(-matches("^(id|name)_player$|^id_team$|^player_num$")) %>%
      group_by(id_game, poss_num) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      inner_join(
        play_by_play_compare_wide,
        by = c("id_game", "poss_num", "pts")
      )


    .summarise_lineup_byside <-
      function(data, col, side = c("o", "d")) {

        col_quo <- enquo(col)
        suffix <- .convert_lineup_to_suffix(!!col_quo)
        data %>%
          rename(lineup = !!col_quo) %>%
          select(id_game, lineup, poss_num, mp, pts) %>%
          group_by(id_game, lineup) %>%
          summarise(
            poss_calc = n(),
            mp_calc = sum(mp),
            pts_calc = sum(pts)
          ) %>%
          ungroup() %>%
          rename_at(vars(matches("_calc$")), funs(paste0(., "_", side)))
      }

    lineup_summary_calc <-
      left_join(
        play_by_play_aug %>% .summarise_lineup_byside(lineup1, side = "o"),
        play_by_play_aug %>% .summarise_lineup_byside(lineup2, side = "d"),
        by = c("id_game", "lineup")
      ) %>%
      mutate(
        poss_calc_total = poss_calc_o + poss_calc_d,
        mp_calc_total = mp_calc_o + mp_calc_d,
        pm_calc = pts_calc_o - pts_calc_d
      ) %>%
      arrange(id_game, desc(mp_calc_total))

    path_export <-
      .export_data_from_path(
        ...,
        data = lineup_summary_calc,
        path = path_lineup_summary_calc
      )
    invisible(lineup_summary_calc)
  }

.summarise_players <-
  function(...,
           play_by_play,
           path_players_summary_calc = config$path_players_summary_calc,
           path_players_game_logs_compare = config$path_players_game_logs_compare,
           path_players_summary_compare = config$path_players_summary_compare) {

    .summarise_players_stats <-
      function(.data) {
        .data %>%
          group_by(id_game, id_player, side) %>%
          summarise(
            poss_calc = n(),
            mp_calc = sum(mp),
            pts_calc = sum(pts)
          ) %>%
          ungroup()
      }
    players_game_logs_calc <-
      bind_rows(
        play_by_play %>%
          filter(side == "o") %>%
          .summarise_players_stats(),
        play_by_play %>%
          filter(side == "d") %>%
          .summarise_players_stats()
      ) %>%
      arrange(id_game, id_player)

    players_game_logs_calc_tidy <-
      players_game_logs_calc %>%
      gather(metric, value, matches("_calc$"))

    players_game_logs_calc_compare <-
      left_join(
        players_game_logs_calc_tidy %>%
          unite(metric_side, metric, side, sep = "_") %>%
          spread(metric_side, value),
        players_game_logs_calc_tidy %>%
          group_by(id_game, id_player, metric) %>%
          summarise_at(vars(value), funs(sum)) %>%
          ungroup() %>%
          spread(metric, value),
        by = c("id_game", "id_player")
      ) %>%
      mutate(pm_calc = pts_calc_o - pts_calc_d) %>%
      arrange(id_game, id_player)

     players_game_logs_nbastatr <-
       .try_import_players_game_logs_nbastatr(...)

    players_game_logs_nbastatr_slim <-
      players_game_logs_nbastatr %>%
      select(
        id_game,
        date_game,
        id_team,
        # name_team,
        slug_team,
        slug_opponent,
        id_player,
        name_player,
        mp_game_logs_nbastatr = minutes,
        pm_game_logs_nbastatr = plusminus
      )

    players_game_logs_compare <-
      players_game_logs_calc_compare %>%
      left_join(
        players_game_logs_nbastatr_slim,
        by = c("id_player", "id_game")
      ) %>%
      arrange(id_game, id_team, id_player) %>%
      mutate(
        mp_diff = mp_calc - mp_game_logs_nbastatr,
        pm_diff = pm_calc - pm_game_logs_nbastatr
      ) %>%
      select(
        id_game,
        id_player,
        slug_team,
        slug_opponent,
        name_player,
        poss_calc,
        mp_calc,
        mp_game_logs_nbastatr,
        mp_diff,
        pts_calc_o,
        pts_calc_d,
        pm_calc,
        pm_game_logs_nbastatr,
        pm_diff
      )

    # TODO: Should probably exclude the inaccurate `pm` results identified
    # with `players_game_logs_compare`.

    path_export <-
      .export_data_from_path(
        ...,
        data = players_game_logs_compare,
        path = path_players_game_logs_compare
      )

    players_summary_calc_base <-
      players_game_logs_calc %>%
      group_by(id_player, side) %>%
      summarise(
        gp_calc = n(),
        poss_calc = sum(poss_calc),
        mp_calc = sum(mp_calc),
        pts_calc = sum(pts_calc)
      ) %>%
      ungroup() %>%
      gather(metric, value, matches("_calc$"))

    players_nbastatr <-
      .try_import_players_nbastatr(...)

    cols_players_summary_calc <-
      c("id_player",
        "name_player",
        paste0("gp", "_calc_total"),
        paste0("poss", c("_calc_o", "_calc_d", "_calc_total")),
        paste0("mp", c("_calc_o", "_calc_d", "_calc_total")),
        paste0("pts", c("_calc_o", "_calc_d", "_o_per100_calc")),
        "pm_calc"
      )
    players_summary_calc <-
      players_summary_calc_base %>%
      unite(metric, metric, side) %>%
      spread(metric, value) %>%
      mutate(
        poss_calc_total = poss_calc_o + poss_calc_d,
        mp_calc_total = mp_calc_o + mp_calc_d,
        pm_calc = pts_calc_o - pts_calc_d
      ) %>%
      mutate(
        pts_o_per100_calc = 100 * pts_calc_o / poss_calc_o
      ) %>%
      rename(gp_calc_total = gp_calc_o) %>%
      select(-gp_calc_d) %>%
      left_join(
        players_nbastatr %>% select(id_player, name_player),
        by = "id_player"
      ) %>%
      arrange(desc(pm_calc)) %>%
      select(one_of(cols_players_summary_calc))

    path_export <-
      .export_data_from_path(
        ...,
        data = players_summary_calc,
        path = path_players_summary_calc
      )

    players_summary_nbastatr <-
      .try_import_players_summary_nbastatr(...)

    players_summary_nbastatr_slim <-
      players_summary_nbastatr %>%
      select(
        id_player = id_player_nba,
        name_player_summary_nbastatr = name_player_bref,
        gp_summary_nbastatr = count_games,
        mp_summary1_nbastatr = minutes,
        mp_summary2_nbastatr = minutes_totals
      )

    players_game_logs_nbastatr_slim_summ <-
      players_game_logs_nbastatr_slim %>%
      group_by(id_player, name_player) %>%
      summarise(
        gp_game_logs_nbastatr = n(),
        mp_game_logs_nbastatr = sum(mp_game_logs_nbastatr),
        pm_game_logs_nbastatr = sum(pm_game_logs_nbastatr)
      ) %>%
      ungroup() %>%
      arrange(id_player)

    cols_players_summary_calc_compare <-
      c("id_player",
        paste0("name_player", c("", "_summary_nbastatr")),
        paste0("gp", c("_calc_total", "_summary_nbastatr")),
        paste0("poss", c("_calc_o", "_calc_d", "_calc_total")),
        paste0("mp",
               c(c("_calc_o", "_calc_d", "_calc_total"),
                 "_game_logs_nbastatr",
                 "_summary1_nbastatr",
                 "_summary2_nbastatr")),
        paste0("pts", c("_calc_o", "_calc_d", "_o_per100_calc")),
        paste0("pm", c("_calc", "_game_logs_nbastatr"))
      )

    players_summary_compare <-
      players_summary_calc %>%
      left_join(
        players_game_logs_nbastatr_slim_summ,
        by = c("id_player", "name_player")
      ) %>%
      left_join(
        players_summary_nbastatr_slim,
        by = c("id_player")
      ) %>%
      arrange(desc(pm_calc)) %>%
      select(cols_players_summary_calc_compare)

    path_export <-
      .export_data_from_path(
        ...,
        data = players_summary_compare,
        path = path_players_summary_compare
      )
    invisible(players_summary_calc)
  }


.summarise_teams <-
  function(...,
           players_summary_calc,
           path_teams_summary_calc = config$path_teams_summary_calc,
           path_teams_summary_compare = config$path_teams_summary_compare) {

    players_nbastatr <- .try_import_players_nbastatr(...)

    players_summary_calc_aug <-
      players_summary_calc %>%
      left_join(
        players_nbastatr %>% select(id_player, id_team, team_name),
        by = c("id_player")
      )
    teams_summary_calc <-
      players_summary_calc_aug %>%
      group_by(id_team, team_name) %>%
      summarise_at(vars(matches("_calc")), funs(sum)) %>%
      ungroup() %>%
      arrange(id_team, team_name)

    teams_summary_nbastatr <- .try_import_teams_summary_nbastatr(...)

    teams_summary_nbastatr_slim <-
      teams_summary_nbastatr %>%
      select(
        id_team,
        gp_summary_nbastatr = gp,
        pts_summary_nbastatr = pts
      )

    # TODO: Re-arrange the columns to closer to those of similar names.
    teams_summary_compare <-
      teams_summary_calc %>%
      left_join(
        teams_summary_nbastatr_slim,
        by = c("id_team")
      ) %>%
      arrange(desc(pm_calc))

    .export_data_from_path(
        ...,
        data = teams_summary_compare,
        path = path_teams_summary_compare
      )
    invisible(players_summary_calc)
  }

# This function is mostly useful so that the `*min` paremters
# can be "abstracted" away (with the ellipses).
.filter_play_by_play <-
  function(...,
           play_by_play,
           players_summary_calc,
           poss_min = 0,
           gp_min = 0,
           mp_min = 0) {

    n_row_before <- play_by_play %>% nrow()
    res <-
      play_by_play %>%
      semi_join(
        players_summary_calc %>%
          filter(
            poss_calc_total >= poss_min,
            gp_calc_total >= gp_min,
            mp_calc_total >= mp_min
          ),
        by = "id_player"
      )
    n_row_after <- res %>% nrow()
    .display_info(
      glue::glue(
        "{usethis::ui_value(n_row_before)} - {usethis::ui_value(n_row_after)} = ",
        "{usethis::ui_value(n_row_before - n_row_after)} rows removed by filtering."
        ),
      ...
    )
    res
  }

.widen_data_byside <-
  function(...,
           play_by_play,
           # I believe this is the one time I need to explicilty include `side` due to the filtering.
           side,
           path_possession_data_side = config$path_possession_data_side,
           path_possession_data_error_side = config$path_possession_data_error_side) {

    # browser()
    play_by_play_side <-
      play_by_play %>%
      filter(side == !!side) %>%
      mutate(xid_player = sprintf("%s%07d", side, as.integer(id_player)))

    dups_n <-
      play_by_play_side %>%
      count(rn, xid_player, sort = TRUE) %>%
      filter(n > 1L)

    n_dups <- nrow(dups_n)
      if(n_dups > 0L) {
        .display_info(
          glue::glue(
            "There are {usethis::ui_value(n_dups)} rows with more than one ",
            "player-side-possession combination."
          ),
          ...
        )

        players_nbastatr <- .try_import_players_nbastatr(...)

        dups_n_aug <-
          dups_n %>%
          mutate(
            id_player = xid_player %>% str_replace("^[od]", "") %>% as.integer()
          ) %>%
          left_join(
            players_nbastatr %>% select(id_player, name_player),
            by = c("id_player")
          )

        path_export <-
          .export_data_from_path(
            ...,
            data = dups_n_aug,
            side = side,
            path = path_possession_data_error_side
          )
    }

    if(FALSE) {
      xid_players <-
        play_by_play_side %>%
        anti_join(dups_n, by = c("rn", "xid_player")) %>%
        arrange(xid_player) %>%
        count(xid_player)
      xid_players_filt <-
        xid_players %>%
        slice(1:50)
    }

    possession_data_side <-
      play_by_play_side %>%
      anti_join(dups_n, by = c("rn", "xid_player")) %>%
      arrange(rn, xid_player) %>%
      mutate(dummy = 1L) %>%
      select(rn, xid_player, pts, dummy) %>%
      # semi_join(xid_players_filt) %>%
      spread(xid_player, dummy, fill = 0L) %>%
      select(-rn) %>%
      mutate(n = 1L) %>%
      group_by_at(vars(-pts, -n)) %>%
      summarise_at(vars(pts, n), funs(sum)) %>%
      ungroup() %>%
      select(pts, n, everything()) %>%
      mutate(pts = 100 * pts / n)

    if(FALSE) {
      possession_data_side %>% arrange(desc(pts))
      possession_data_side %>% count(n) %>% arrange(n)
      possession_data_side %>% count(n) %>% arrange(desc(n))

      possession_data_side %>%
        # filter(n > 1) %>%
        # filter(pts > 2) %>%
        filter(abs(pts) <= 500)
    }

    possession_data_side <-
      possession_data_side %>%
      select(-n)
    path_export <-
      .export_data_from_path(
        ...,
        data = possession_data_side,
        side = side,
        path = path_possession_data_side
      )
    invisible(possession_data_side)
  }

.separate_lineup <-
  function(play_by_play, col, prefix = "x", suffix = 1:5, sep = "-") {
    col <- enquo(col)
    play_by_play %>%
      separate(!!col, into = glue::glue("{prefix}{suffix}"), sep = sep)
  }

munge_play_by_play <-
  function(...,
           path_play_by_play = config$path_play_by_play,
           path_possession_data_side = config$path_possession_data_side,
           path_players_summary_calc = config$path_players_summary_calc) {

    will_skip <-
      .try_skip(
        ...,
        path_reqs =
          c(
            .get_path_from(..., path = path_play_by_play)
          ),
        path_deps =
          c(
            .get_path_from(..., path = path_players_summary_calc),
            .get_path_from(..., path = path_possession_data_side, side = "o"),
            .get_path_from(..., path = path_possession_data_side, side = "d")
          )
      )
    if(will_skip) {
      return(invisible(NULL))
    }

    .display_info(
      glue::glue("Step 2: Munging play-by-play data."),
      ...
    )

    play_by_play <-
      .import_data_from_path(
        ...,
        path = path_play_by_play
      )

    suppressMessages(
      play_by_play <-
        full_join(
          play_by_play %>%
            filter(is_off1) %>%
            select(
              rn1 = rn,
              id_game,
              is_home = is_home1,
              # is_off = is_off1,
              pts = pts1,
              mp,
              id_team_o = id_team1,
              id_team_d = id_team2,
              slug_o = slug_team1,
              slug_d = slug_team2,
              lineup_o = lineup1,
              lineup_d = lineup2
            ),
          play_by_play %>%
            mutate(is_home2 = !is_home1, is_off2 = !is_off1) %>%
            filter(is_off2) %>%
            select(
              rn2 = rn,
              id_game,
              is_home = is_home2,
              # is_off = is_off2,
              pts = pts2,
              mp,
              id_team_o = id_team2,
              id_team_d = id_team1,
              slug_o = slug_team2,
              slug_d = slug_team1,
              lineup_o = lineup2,
              lineup_d = lineup1
            )
        ) %>%
        mutate(rn = coalesce(rn1, rn2)) %>%
        select(-matches("^rn[12]$")) %>%
        select(rn, everything()) %>%
        arrange(rn)
    )

    play_by_play <-
      play_by_play %>%
      .separate_lineup(lineup_o, suffix = 1:5) %>%
      .separate_lineup(lineup_d, suffix = 6:10) %>%
      mutate_at(vars(matches("^x")), funs(as.integer))

    play_by_play <-
      play_by_play %>%
      group_by(id_game) %>%
      mutate(poss_num = row_number()) %>%
      ungroup() %>%
      gather(dummy, id_player, matches("^x")) %>%
      select(-dummy) %>%
      group_by(id_game, poss_num) %>%
      mutate(player_num = row_number()) %>%
      ungroup() %>%
      mutate(side = if_else(player_num <= 5L, "o", "d")) %>%
      arrange(rn, player_num)

    # Note that the `players_summary_calc` is the only `_calc` object
    # that that matters
    # (because the subsequent filtering depends on it).
    lineup_summary_calc <-
      .summarise_lineup(
        ...,
        play_by_play = play_by_play
      )

    players_summary_calc <-
      .summarise_players(
        ...,
        play_by_play = play_by_play
      )

    teams_summary_calc <-
      .summarise_teams(
        ...,
        players_summary_calc = players_summary_calc
      )

    play_by_play <-
      .filter_play_by_play(
        ...,
        play_by_play = play_by_play,
        players_summary_calc = players_summary_calc
      )

    possession_data_o <-
      .widen_data_byside(
        ...,
        play_by_play = play_by_play,
        side = "o"
      )
    possession_data_d <-
      .widen_data_byside(
        ...,
        play_by_play = play_by_play,
        side = "d"
      )

    invisible(
      list(
        possession_data_o = possession_data_o,
        possession_data_d = possession_data_d
      )
    )
  }

munge_play_by_play_auto <-
  function(...,
           season = config$season,
           poss_min = config$poss_min,
           gp_min = config$gp_min,
           mp_min = config$mp_min,
           skip = config$skip,
           verbose = config$verbose,
           export = config$export,
           backup = config$backup,
           clean = config$clean,
           n_keep = config$n_keep) {
    munge_play_by_play(
      # ...,
      season = season,
      poss_min = poss_min,
      gp_min = gp_min,
      mp_min = mp_min,
      skip = skip,
      verbose = verbose,
      export = export,
      backup = backup,
      clean = clean,
      n_keep = n_keep
    )
  }
