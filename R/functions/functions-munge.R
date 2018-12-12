
.separate_lineup <-
  function(play_by_play, col, prefix = "x", suffix = 1:5, sep = "-") {
    play_by_play %>%
      separate(!!enquo(col), into = paste0(prefix, suffix), sep = sep)
  }

.get_cols_players_summary_calc <-
  function(...) {
    .COLS_STATS <- c("poss", "mp", "pts")
    .SUFFIX_COLS_STATS_ORDER1 <- .SIDES
    .SUFFIX_COLS_STATS_ORDER2 <- c(.SUFFIX_COLS_STATS_ORDER1, "total")
    .COLS_SUMM_BYID_ORDER <-
      c("name",
        "id",
        "gp_total",
        paste0("poss_", .SUFFIX_COLS_STATS_ORDER2),
        paste0("mp_", .SUFFIX_COLS_STATS_ORDER2),
        paste0("pts_", .SUFFIX_COLS_STATS_ORDER1),
        "pm",
        "pts_o_per100",
        "pts_o_per36"
      )
    .COLS_SUMM_BYID_ORDER
  }


.summarise_players <-
  function(...,
           play_by_play,
           path_players_summary_calc = config$path_players_summary_calc,
           path_players = config$path_players,
           debug = .DEBUG) {
    # TODO
    game_logs_player_calc <-
      play_by_play %>%
      mutate(poss = 1L) %>%
      group_by(id, game_id, side) %>%
      summarise(
        poss = n(),
        mp = sum(mp),
        pts = sum(pts)
      ) %>%
      ungroup()


    if(debug) {
      game_logs_player_nbastatr <-
        .try_import_game_logs_player_nbastatr(...)

      game_logs_player_debug <-
        game_logs_player_calc %>%
        left_join(
          game_logs_player_nbastatr,
          by = c("id" = "id_player", "game_id" = "id_game")
        ) %>%
        arrange(game_id, id)

      .export_data_from_path(
        ...,
        data = player_summary_debug,
        path = glue::glue("data/game_logs_player_debug.csv")
      )

    }

    players_summary_calc <-
      game_logs_player_nbastatr_calc %>%
      group_by(id, side) %>%
      summarise(
        gp = n(),
        poss = sum(poss),
        mp = sum(mp),
        pts = sum(pts)
      ) %>%
      ungroup() %>%
      gather(metric, value, matches("^[g|m]p$|^poss$|^pts")) %>%
      # group_by(id, metric) %>%
      # mutate_at(vars(value), funs(total = sum(., na.rm = TRUE))) %>%
      # ungroup() %>%
      gather(metric, value, matches("total")) %>%
      unite(metric, metric, side) %>%
      spread(metric, value) %>%
      mutate(
        poss_total = poss_o + poss_d,
        mp_total = mp_o + mp_d,
        pm = pts_o - pts_d
      ) %>%
      mutate(
        pts_o_per100 = 100 * pts_o / poss_total,
        pts_o_per36 = 36 * pts_o / mp_total
      ) %>%
      rename(gp_total = gp_o) %>%
      select(-matches("^gp_d$")) %>%
      arrange(desc(mp_total))

    # TODO: Include `players_summary_calc` as an input and validate that all specified columns exist?
    cols_players_summary_calc <-
      .get_cols_players_summary_calc()

    players_summary_calc <-
      players_summary_calc %>%
      select(one_of(cols_players_summary_calc))

    if(debug) {
      players_summary_nbastatr <-
        .try_import_players_summary_nbastatr(...)

      players_summary_debug <-
        players_summary_calc %>%
        left_join(
          players_summary_nbastatr,
          by = c("id" = "id_player")
        ) %>%
        arrange(id)

      .export_data_from_path(
        ...,
        data = player_summary_debug,
        path = glue::glue("data/players_summary_debug.csv")
      )

    }

    path_export <-
      .export_data_from_path(
        ...,
        data = players_summary_calc,
        path = path_players_summary_calc,
      )

    invisible(players_summary_calc)
  }

.filter_and_trim_play_by_play <-
  function(..., play_by_play, players_summary_calc, poss_min, gp_min, mp_min) {
    play_by_play %>%
      semi_join(
        players_summary_calc %>%
          filter(
            poss_total >= poss_min,
            gp_total >= gp_min,
            mp_total >= mp_min
          ),
        by = "id"
      ) %>%
      select(xid, pts, poss_num, dummy)
  }

.widen_data_byside <-
  function(...,
           play_by_play,
           side,
           path_possession_data_side,
           debug = .DEBUG) {

    .validate_side(side)

    if(debug) {
      duplicates_n <-
        play_by_play %>%
        filter(str_detect(xid, sprintf("^%s", side))) %>%
        count(xid, poss_num, sort = TRUE) %>%
        filter(n > 1L)

      n_duplicates <- nrow(duplicates_n)
      if(n_duplicates > 0L) {
        .display_warning(
          sprintf(
            paste0(
              "There are %d rows with more than one `xid`-`poss_num` combination ",
              "(i.e. a player appears in a lineup more than once on a single possession).",
              "`dplyr::distinct()` is being used to remove make records uniques."
            ),
            n_duplicates),
          ...
        )

        players_nbastatr <-
          .try_import_players_nbastatr(...)

        duplicates_n_aug <-
          duplicates_n %>%
          mutate(
            id = xid %>% str_replace("^%s") %>% as.integer()
          ) %>%
          left_join(
            players_nbastatr %>% select(id_player, player_name),
            by = c("id" = "id_player")
          )

        path_export <-
          .export_data_from_path(
            ...,
            data = duplicates_n_aug,
            path = glue::glue("data/n_duplicates.csv"),
          )
      }

    }

    possession_data <-
      play_by_play %>%
      filter(str_detect(xid, sprintf("^%s", side))) %>%
      # Not sure why, but there are still duplicates.
      distinct(xid, poss_num, .keep_all = TRUE) %>%
      arrange(xid, poss_num) %>%
      # select(-poss_num) %>%
      spread(xid, dummy, fill = 0L) %>%
      mutate(poss = 1L) %>%
      group_by_at(vars(-pts, -poss)) %>%
      summarise_at(vars(pts, poss), funs(sum)) %>%
      ungroup() %>%
      select(pts, poss, everything()) %>%
      # filter(poss > 1) %>%
      # filter(pts > 2) %>%
      mutate(pts = 100 * pts / poss) %>%
      # Note that this is an extra precaution.
      filter(abs(pts) <= 500) %>%
      select(-poss)

    path_export <-
      .export_data_from_path(
        data = possession_data,
        path = path_possession_data_side,
        season = season,
        ...
      )

    invisible(possession_data)
  }

munge_cleaned_play_by_play <-
  function(path_play_by_play = config$path_play_by_play,
           path_possession_data_o = config$path_possession_data_o,
           path_possession_data_d = config$path_possession_data_d,
           # TODO: Maybe figure out a way to abstract these paramters aways.
           # (Possibly just create a function that does the filtering,
           # taking these named parameters.)
           # Update: Done.
           ...) {

    will_skip <-
      .try_skip(
        ...,
        path_reqs =
          c(
            path_play_by_play
          ),
        path_deps =
          c(
            path_possession_data_o,
            path_possession_data_d
          )
      )
    if(will_skip) {
      return(invisible(NULL))
    }

    play_by_play <-
      .import_data_from_path(
        ...,
        path = path_play_by_play
      )

    play_by_play <-
      play_by_play %>%
      .separate_lineup(lineup1, suffix = 1:5) %>%
      .separate_lineup(lineup2, suffix = 6:10) %>%
      mutate_at(vars(matches("^x")), funs(as.integer))

    play_by_play <-
      play_by_play %>%
      mutate(poss_num = row_number()) %>%
      gather(dummy, id, matches("^x")) %>%
      select(-dummy)

    play_by_play <-
      play_by_play %>%
      mutate(side = if_else(is_off == 0L, "d", "o")) %>%
      mutate(xid = sprintf("%s%07d", side, as.integer(id))) %>%
      select(-is_off) %>%
      mutate(dummy = 1L)

    players_summary_calc <-
      .summarise_players(
        ...,
        play_by_play = play_by_play
      )

    teams_summary_calc <-
      .summarise_teams(
        ...,
        play_by_play = play_by_play
      )

    play_by_play <-
      .filter_and_trim_play_by_play(
        ...,
        play_by_play = play_by_play,
        players_summary_calc = players_summary_calc,
      )

    .widen_data_byside_partially <-
      purrr::partial(
        .widen_data_byside,
        ...,
        play_by_play = play_by_play,
        season = season
      )

    possession_data_o <-
      .widen_data_byside_partially(
        path_possession_data_side = path_possession_data_o,
        side = "o"
      )

    possession_data_d <-
      .widen_data_byside_partially(
        path_possession_data_side = path_possession_data_d,
        side = "d"
      )

    invisible(
      list(
        possession_data_o = possession_data_o,
        possession_data_d = possession_data_d
      )
    )
  }

auto_munge_cleaned_play_by_play <-
  purrr::partial(
    munge_cleaned_play_by_play,
    season = config$season,
    poss_min = config$poss_min,
    gp_min = config$gp_min,
    mp_min = config$mp_min,
    skip = config$skip_munge,
    debug = config$debug,
    verbose = config$verbose,
    export = config$export,
    backup = config$backup,
    clean = config$clean,
    n_keep = config$n_keep
  )
