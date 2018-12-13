
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
      c("id_player",
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
           path_teams_summary_calc = config$path_teams_summary_calc,
           debug = config$debug) {

    players_game_logs_calc <-
      play_by_play %>%
      mutate(poss = 1L) %>%
      group_by(id_player, id_game, side) %>%
      summarise(
        poss = n(),
        mp = sum(mp),
        pts = sum(pts)
      ) %>%
      ungroup()


    if(debug) {

      players_game_logs_nbastatr <-
        .try_import_players_game_logs_nbastatr(...)
      players_game_logs_nbastatr_slim <-
        players_game_logs_nbastatr %>%
        select(
          id_game,
          date_game,
          id_team,
          name_team,
          slug_team,
          slug_opponent,
          id_player,
          name_player,
          mp_nbastatr = minutes,
          pm_nbastatr = plusminus
        )

      .RGX_PLAYERS_GAME_LOGS_CALC <- "^mp$|^poss$|^pm"
      players_game_logs_debug <-
        players_game_logs_calc %>%
        mutate_at(vars(pts), funs(if_else(side == "d", -., .))) %>%
        rename(pm = pts) %>%
        gather(metric, value, matches(.RGX_PLAYERS_GAME_LOGS_CALC)) %>%
        group_by(id_game, id_player, metric) %>%
        summarise_at(vars(value), funs(sum)) %>%
        ungroup() %>%
        spread(metric, value) %>%
        rename_at(vars(matches(.RGX_PLAYERS_GAME_LOGS_CALC)), funs(paste0(., "_calc"))) %>%
        left_join(
          players_game_logs_nbastatr_slim,
          by = c("id_player", "id_game")
        ) %>%
        arrange(id_game, id_team, id_player)

      .export_data_from_path(
        ...,
        data = players_game_logs_debug,
        path = glue::glue("data/debug/players_game_logs_debug.csv")
      )

    }

    players_summary_calc <-
      players_game_logs_calc %>%
      group_by(id_player, side) %>%
      summarise(
        gp = n(),
        poss = sum(poss),
        mp = sum(mp),
        pts = sum(pts)
      ) %>%
      ungroup() %>%
      gather(metric, value, matches("^[g|m]p$|^poss$|^pts"))

    # players_summary_calc <-
    #   players_summary_calc %>%
    #   group_by(id_player, metric) %>%
    #   mutate_at(vars(value), funs(total = sum(., na.rm = TRUE))) %>%
    #   ungroup()

    players_summary_calc <-
      players_summary_calc %>%
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

    cols_players_summary_calc <-
      .get_cols_players_summary_calc(...)

    players_summary_calc <-
      players_summary_calc %>%
      select(one_of(cols_players_summary_calc))

    if(debug) {

      players_summary_nbastatr <-
        .try_import_players_summary_nbastatr(...)
      browser()
      players_summary_nbastatr_slim <-
        players_summary_nbastatr %>%
        select(
          id_player = id_player_nba,
          name_player_nbastatr_bref = name_player_bref,
          gp_nbastatr_bref = count_games,
          minutes_nbastatr1_bref = minutes,
          minutes_nbastatr2_bref = minutes_totals
        )

      players_game_logs_nbastatr_slim_summ <-
        players_game_logs_nbastatr_slim %>%
        group_by(id_player, name_player) %>%
        summarise(
          gp = n(),
          mp_nbastatr = sum(mp_nbastatr),
          pm_nbastatr = sum(pm_nbastatr)
        ) %>%
        ungroup() %>%
        arrange(id_player)

      players_summary_debug <-
        players_summary_calc %>%
        left_join(
          players_game_logs_nbastatr_slim_summ,
          by = c("id_player")
        ) %>%
        left_join(
          players_summary_nbastatr_slim,
          by = c("id_player")
        ) %>%
        arrange(id_player)

      .export_data_from_path(
        ...,
        data = players_summary_debug,
        path = glue::glue("data/debug/players_summary_debug.csv")
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

.summarise_teams <-
  function(...,
           # play_by_play,
           players_summary_calc,
           path_teams_summary_calc = config$path_teams_summary_calc,
           debug = config$debug) {

    if(debug) {
      players <- .try_import_players_nbastatr(...)
      players_summary_calc_aug <-
        players_summary_calc %>%
        left_join(
          players %>% select(id_player, id_team, team_name),
          by = c("id_player" = "id_player")
        )

      teams_summary_calc <-
        players_summary_calc_aug %>%
        group_by(id_team, team_name) %>%
        summarise_if(is.numeric, funs(sum)) %>%
        ungroup() %>%
        arrange(id_team, team_name)

      teams_summary_nbastatr <-
        .try_import_teams_summary_nbastatr(...)

      teams_summary_debug <-
        teams_summary_calc %>%
        left_join(
          teams_summary_nbastatr,
          by = c("id_team", "team_name")
        )

      .export_data_from_path(
        ...,
        data = teams_summary_debug,
        path = glue::glue("data/debug/teams_summary_debug.csv")
      )
    }
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
        by = "id_player"
      ) %>%
      select(xid_player, pts, poss_num, dummy)
  }

.widen_data_byside <-
  function(...,
           play_by_play,
           side,
           path_possession_data_side,
           debug = config$debug) {

    .validate_side(side)

    if(debug) {
      duplicates_n <-
        play_by_play %>%
        filter(str_detect(xid_player, sprintf("^%s", side))) %>%
        count(xid_player, poss_num, sort = TRUE) %>%
        filter(n > 1L)

      n_duplicates <- nrow(duplicates_n)
      if(n_duplicates > 0L) {
        .display_warning(
          sprintf(
            paste0(
              "There are %d rows with more than one `xid_player`-`poss_num` combination ",
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
            id_player = xid_player %>% str_replace("^%s") %>% as.integer()
          ) %>%
          left_join(
            players_nbastatr %>% select(id_player, player_name),
            by = c("id_player" = "id_player")
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
      filter(str_detect(xid_player, sprintf("^%s", side))) %>%
      # Not sure why, but there are still duplicates.
      distinct(xid_player, poss_num, .keep_all = TRUE) %>%
      arrange(xid_player, poss_num) %>%
      # select(-poss_num) %>%
      spread(xid_player, dummy, fill = 0L) %>%
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
  function(...,
           path_play_by_play = config$path_play_by_play,
           path_possession_data_o = config$path_possession_data_o,
           path_possession_data_d = config$path_possession_data_d,
           debug = config$debug) {

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


    # play_by_play <- .import_data_from_path(season = .SEASON, path = config$path_play_by_play)

    play_by_play <-
      play_by_play %>%
      .separate_lineup(lineup1, suffix = 1:5) %>%
      .separate_lineup(lineup2, suffix = 6:10) %>%
      mutate_at(vars(matches("^x")), funs(as.integer))

    play_by_play <-
      play_by_play %>%
      mutate(poss_num = row_number()) %>%
      gather(dummy, id_player, matches("^x")) %>%
      select(-dummy)

    # if(debug) {
    if(FALSE) {

      players_nbastatr <- .try_import_players_nbastatr(season = .SEASON)
      # players_nbastatr <- .try_import_players_nbastatr(...)

      play_by_play_debug <-
        play_by_play %>%
        mutate_at(vars(pts), funs(if_else(is_off == 1, -., .))) %>%
        rename(pts_calc = pts) %>%
        left_join(
          # To get `name_player`. Keep `id_team` to use for sorting.
          players_nbastatr %>% select(id_player, name_player, id_team),
          by = c("id_player")
        ) %>%
        arrange(id_game, poss_num, id_team, id_player) %>%
        group_by(id_game, poss_num) %>%
        mutate(player_num = row_number()) %>%
        ungroup() %>%
        mutate_at(vars(player_num), funs(sprintf("x%02d", .))) %>%
        select(id_game, poss_num, pts_calc, player_num, name_player)

      play_by_play_debug <-
        play_by_play_debug %>%
        # filter(id_game == dplyr::first(id_game)) %>%
        spread(player_num, name_player)

      # .unite_lineup <-
      #   function(play_by_play, col, prefix = "x", which = c("1", "2"), sep = "-") {
      #     col <- enquo(col)
      #     # which <- enquo(col)
      #     # which_chr <- rlang::quo_name(!!which)
      #     # suffix <- str_sub(which_chr, start = nchar(which_chr))
      #     which <- match.arg(which)
      #     suffix <-
      #       case_when(
      #         which == "1" ~ "0[1-5]",
      #         which == "2" ~ "[01][06-9]"
      #       )
      #
      #     play_by_play %>%
      #       unite(!!col, matches(glue::glue("{prefix}[{suffix}")), sep = sep)
      #   }

      play_by_play_debug <-
        play_by_play_debug %>%
        # .unite_lineup(lineup1, "1") %>%
        # .unite_lineup(lineup2, "2")
        unite(lineup1, matches("x0[1-5]"), sep = "-") %>%
        unite(lineup2, matches("x[0-1][06-9]"), sep = "-")


      .export_data_from_path(
        ...,
        data = play_by_play_debug,
        path = glue::glue("data/debug/play_by_play_debug.csv")
      )
    }

    play_by_play <-
      play_by_play %>%
      mutate(side = if_else(is_off == 0L, "d", "o")) %>%
      mutate(xid_player = sprintf("%s%07d", side, as.integer(id_player))) %>%
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
        players_summary_calc = players_summary_calc
      )

    play_by_play <-
      .filter_and_trim_play_by_play(
        ...,
        play_by_play = play_by_play,
        players_summary_calc = players_summary_calc
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
    verbose = config$verbose,
    export = config$export,
    backup = config$backup,
    clean = config$clean,
    n_keep = config$n_keep
  )
