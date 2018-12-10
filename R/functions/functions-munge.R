

.separate_lineup <-
  function(play_by_play, col, prefix = "x", suffix = 1:5, sep = "-") {
    play_by_play %>%
      separate(!!enquo(col), into = paste0(prefix, suffix), sep = sep)
  }


.get_players <-
  function(path_players_format, season = NULL, ...) {
    if(!is.null(season)) {
      season <- .validate_season(season)
    }
    players_raw <-
      nbastatR::get_nba_players()
    players <-
      players_raw %>%
      janitor::clean_names()
    if(!is.null(season)) {
      players <-
        players %>%
        filter(year_season_first <= season,
               year_season_last >= season)
    }
    players <-
      players %>%
      select(id = id_player,
             name = name_player) %>%
      arrange(id)

    # Note that `players` and `teams` are special cases where it is not required
    # that `path*` be non-`NULL`. (This is a design choice since these are
    # retrieved from `{nbastatR}`.
    if(!is.null(path_players_format)) {
      path_export <-
        .export_data_from_path_format(
          data = players,
          path_format = path_players_format,
          ...
        )
    }

    invisible(players)
  }

.get_teams <-
  function(path_players_format, season = NULL, ...) {
    if(!is.null(season)) {
      season <- .validate_season(season)
    }
    teams_raw <-
      nbastatR::get_nba_teams()

    teams <-
      teams_raw %>%
      janitor::clean_names() %>%
      filter(is_non_nba_team == 0L)

    if(!is.null(season)) {
      teams <-
        teams %>%
        filter(year_played_last >= season)
    }
    # .COLS_TEAMS_ORDER <- c("id_team", "name_team", "slug_team")
    teams <-
      teams %>%
      select(
        id = id_team,
        name = name_team,
        slug = slug_team
      )

    if(!is.null(path_teams_format)) {
      path_export <-
        .export_data_from_path_format(
          data = teams,
          path_format = path_teams_format,
          ...
        )
    }

    invisible(teams)
  }


# Putting this in all caps to indicate that this is sort of "hard-coded".
.get_cols_players_summary <-
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

.import_players_possibly <-
  function(path_players_format,
           ...) {
    .import_thing_possibly(
      path_import_format_clean = path_players_format,
      path_import_format_raw = path_players_format,
      f_clean2raw = .get_players,
      ...
    )
  }

.summarise_res <-
  function(play_by_play,
           path_players_summary_format,
           path_players_format,
           season,
           ...) {

    players <-
      .import_players_possibly(...)

    players_summary <-
      play_by_play %>%
      mutate(poss = 1L) %>%
      group_by(id, game_id, side) %>%
      summarise(
        poss = n(),
        mp = sum(mp),
        pts = sum(pts)
      ) %>%
      ungroup() %>%
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

    players_summary <-
      players_summary %>%
      left_join(players, by = "id")

    # TODO: Include `players_summary` as an input and validate that all specified columns exist?
    cols_players_summary <-
      .get_cols_players_summary()

    players_summary <-
      players_summary %>%
      select(one_of(cols_players_summary))

    path_export <-
      .export_data_from_path_format(
        data = players_summary,
        path_format = path_players_summary_format,
        season = season,
        ...
      )

    invisible(players_summary)
  }

.filter_and_trim_play_by_play <-
  function(play_by_play, players_summary, poss_min, gp_min, mp_min, ...) {
    play_by_play %>%
      semi_join(
        players_summary %>%
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
  function(play_by_play,
           side,
           path_possession_data_side_format,
           season,
           debug = .DEBUG,
           ...) {

    side <- .validate_side(side)

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


        players <-
          .import_players_possibly(...)

        duplicates_n_aug <-
          duplicates_n %>%
          mutate(
            id = xid %>% str_replace("^%s") %>% as.integer()
          ) %>%
          left_join(
            players,
            by = "id"
          )

        path_format <- sprintf("%s%s.csv", sprintf("data/debug/n_duplicates-%s", Sys.time()))
        path_export <-
          .export_data_from_path_format(
            data = duplicates_n_aug,
            path_format = path_format,
            season = season,
            export = debug,
            ...
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
      .export_data_from_path_format(
        data = possession_data,
        path_format = path_possession_data_side_format,
        season = season,
        ...
      )

    invisible(possession_data)
  }

munge_cleaned_play_by_play <-
  function(path_play_by_play_format,
           path_possession_data_o_format,
           path_possession_data_d_format,
           path_players_format,
           path_players_summary_format,
           # season = .SEASON,
           # TODO: Maybe figure out a way to abstract these paramters aways.
           # (Possibly just create a function that does the filtering,
           # taking these named parameters.)
           # Update: Done.
           ...) {

    will_skip <-
      .try_skip(
        path_format_reqs =
          c(
            path_play_by_play_format
          ),
        path_format_deps =
          c(
            path_possession_data_o_format,
            path_possession_data_o_format,
            path_players_format,
            path_players_summary_format
          ),
        ...
      )
    if(will_skip) {
      return(invisible(NULL))
    }

    play_by_play <-
      .import_data_from_path_format(
        path_format = path_play_by_play_format,
        ...
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
    # browser()
    # play_by_play %>%
    #   count(game_id, poss_num, sort = TRUE) %>%
    #   filter(n != 10)
    # play_by_play %>%
    #   filter(xid == "o0002772") %>%
    #   # count(game_id, poss_num, sort = TRUE)
    #   filter(poss_num == 15112)

    # ALWAYS regenerate `player_summary`.
    players_summary <-
      play_by_play %>%
      .summarise_players(
        path_players_summary_format = path_players_summary_format,
        path_players_format = path_players_format,
        ...
      )

    play_by_play <-
      .filter_and_trim_play_by_play(
        play_by_play = play_by_play,
        players_summary = players_summary,
        ...
      )

    .widen_data_byside_partially <-
      purrr::partial(
        .widen_data_byside,
        play_by_play = play_by_play,
        season = season,
        ...
      )

    possession_data_o <-
      .widen_data_byside_partially(
        path_possession_data_side_format = path_possession_data_o_format,
        side = "o"
      )

    possession_data_d <-
      .widen_data_byside_partially(
        path_possession_data_side_format = path_possession_data_d_format,
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
    path_play_by_play_format = args$path_play_by_play_format,
    path_possession_data_o_format = args$path_possession_data_o_format,
    path_possession_data_d_format = args$path_possession_data_d_format,
    path_players_summary_format = args$path_players_summary_format,
    path_players_format = args$path_players_format,
    season = args$season,
    poss_min = args$poss_min,
    gp_min = args$gp_min,
    mp_min = args$mp_min,
    skip = args$skip_munge,
    debug = args$debug,
    verbose = args$verbose,
    export = args$export,
    backup = args$backup,
    clean = args$clean,
    n_keep = args$n_keep
  )
