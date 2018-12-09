
.separate_lineup <-
  function(data, col, prefix = "x", suffix = 1:5, sep = "-") {
    data %>%
      separate(!!enquo(col), into = paste0(prefix, suffix), sep = sep)
  }


.get_players <-
  function(season, path_players_format, ..., verbose = .VERBOSE, export = .EXPORT) {
    players_raw <-
      nbastatR::get_nba_players()
    players <-
      players_raw %>%
      janitor::clean_names() %>%
      filter(
        year_season_first <= season,
        year_season_last >= season
      ) %>%
      select(
        id = id_player,
        name = name_player
      ) %>%
      arrange(id)

    path_export <-
      .export_data_from_path_format(
        data = players,
        path_format = path_players_format,
        season = season,
        export = export,
        verbose = verbose,
        ...
      )

    players
  }

.summarise_players <-
  function(data,
           season,
           path_players_summary_format,
           path_players_format,
           ...,
           verbose = .VERBOSE,
           export = .EXPORT) {

    # Ignore the following. ALWAYS regenerate `players_summary`.
    # .import_players_summary_possibly <-
    #   purrr::possibly(
    #     .import_data_from_path_format(
    #       path_format = path_players_summary_format,
    #       season = season,
    #       verbose = verbose,
    #       ...
    #     ),
    #     otherwise = NULL
    #   )
    #
    # players_summary <-
    #   .import_players_summary_possibly()
    # if(!is.null(players_summary)) {
    #   return(players_summary)
    # }

    .import_players_partially <-
      purrr::partial(
      .import_data_from_path_format,
        path_format = path_players_format,
        season = season,
        verbose = verbose,
        ...
      )

    .import_players_possibly <-
      purrr::possibly(
        .import_players_partially,
        otherwise = NULL
      )
    players <- .import_players_possibly()
    if(is.null(players)) {
      players <-
        .get_players(
          season = season,
          path_players_format = path_players_format,
          verbose = verbose,
          export = export,
          ...
        )
    }

    # TODO: Make a function for this?
    .COLS_STATS <- c("poss", "mp", "pts")
    .SUFFIX_COLS_STATS_ORDER1 <- c("o", "d")
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

    # browser()
    players_summary <-
      data %>%
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
      left_join(players, by = "id") %>%
      select(one_of(.COLS_SUMM_BYID_ORDER))

    path_export <-
      .export_data_from_path_format(
        data = players_summary,
        path_format = path_players_summary_format,
        season = season,
        export = export,
        verbose = verbose,
        ...
      )

    players_summary
  }

.widen_data_byside <-
  function(data, side = c("o", "d"), season, path_data_wide_side_format, ..., verbose = .VERBOSE, export = .EXPORT) {

    side <- match.arg(side)
    # display_info(
    #   sprintf("Trying to export \"%s\" possession data.", side),
    #   verbose = verbose
    # )

    # browser()
    # data %>%
    #   filter(str_detect(xid, sprintf("^%s", side))) %>%
    #   count(xid, poss_num, sort = TRUE) %>%
    #   filter(n > 1)

    data_wide <-
      data %>%
      filter(str_detect(xid, sprintf("^%s", side))) %>%
      # Not sure why, but there are still duplicates.
      distinct(xid, poss_num, .keep_all = TRUE) %>%
      spread(xid, dummy, fill = 0L) %>%
      mutate(poss = 1L) %>%
      group_by_at(vars(-pts, -poss)) %>%
      summarise_at(vars(pts, poss), funs(sum)) %>%
      ungroup() %>%
      select(pts, poss, everything()) %>%
      # filter(poss > 1) %>%
      # filter(pts > 2) %>%
      mutate(pts = 100 * pts / poss) %>%
      # filter(abs(pts) <= 400) %>%
      select(-poss)

    path_export <-
      .export_data_from_path_format(
        data = data_wide,
        path_format = path_data_wide_side_format,
        season = season,
        export = export,
        verbose = verbose,
        ...
      )

    data_wide
  }

process_cleaned_data <-
  function(season,
           path_data_clean_format,
           poss_min,
           gp_min,
           mp_min,
           ...,
           skip = .SKIP,
           verbose = .VERBOSE,
           export = .EXPORT,
           path_data_wide_o_format,
           path_data_wide_d_format,
           path_players_format,
           path_players_summary_format) {

    if(skip) {
      return(invisible(NULL))
    }

    data <-
      .import_data_from_path_format(
        path_format = path_data_clean_format,
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

    players_summary <-
      data %>%
      .summarise_players(
        season = season,
        path_players_summary_format = path_players_summary_format,
        path_players_format = path_players_format,
        verbose = verbose,
        export = export,
        ...
      )

    data <-
      data %>%
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

    .widen_data_byside_partially <-
      purrr::partial(
        .widen_data_byside,
        data = data,
        season = season,
        verbose = verbose,
        export = export,
        ...
      )

    data_wide_o <-
      .widen_data_byside_partially(
        side = "o",
        path_data_wide_side_format = path_data_wide_o_format
      )

    data_wide_d <-
      .widen_data_byside_partially(
        side = "d",
        path_data_wide_side_format = path_data_wide_d_format
      )

    data
  }

do_process_cleaned_data <-
  purrr::partial(
    process_cleaned_data,
    season = args$season,
    path_data_clean_format = args$path_data_clean_format,
    poss_min = args$poss_min,
    gp_min = args$gp_min,
    mp_min = args$mp_min,
    path_data_wide_o_format = args$path_data_wide_o_format,
    path_data_wide_d_format = args$path_data_wide_d_format,
    path_players_summary_format = args$path_players_summary_format,
    path_players_format = args$path_players_format,
    skip = args$skip_intermediate,
    verbose = args$verbose,
    export = args$export_intermediate

  )
