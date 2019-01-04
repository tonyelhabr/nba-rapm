
.parse_pbp <-
  function(...,
           pbp_raw,
           path_game_final_scores_error = config$path_game_final_scores_error,
           path_pbp_parse_error = config$path_pbp_parse_error,
           path_pbp_parse = config$path_pbp_parse) {
    teams_game_logs_nbastatr <- .try_import_teams_game_logs_nbastatr(...)
    teams_nbastatr <- .try_import_teams_nbastatr(...)
    players_nbastatr <- .try_import_players_nbastatr(...)

    # TODO: Separate this into its own function (like the `dups` function).
    # Check that final scores are correct.
    raw_game_final_scores <-
      pbp_raw %>%
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

    game_final_scores_error <-
      raw_game_final_scores %>%
      anti_join(
        game_logs_final_scores,
        by = c("id_game", "pts_home", "pts_away")
      )

    id_game_bad <-
      game_final_scores_error %>%
      pull(id_game)

    path_export <-
      .export_data_from_path(
        ...,
        data = game_final_scores_error,
        path = path_game_final_scores_error
      )

    play_types_valid <-
      c("Make", "Miss", "Turnover", "Timeout", "Ejection", "Foul")

    pbp_parse <-
      pbp_raw %>%
      # .filter_season_type(...)
      filter(season_type == "Regular Season") %>%
      # Throw out completely mis-labeled points (e.g. 21600236).
      anti_join(game_final_scores_error, by = c("game_id" = "id_game")) %>%
      # Some of these "relevant" `play_type`s are mis-labeled with `event_action_type = 0`.
      filter(event_action_type != 0 | c(play_type %in% play_types_valid)) %>%
      filter(!is.na(player1team_id)) %>%
      mutate_at(vars(matches("description$")), funs(na_if(., ""))) %>%
      mutate(
        description = coalesce(home_description, away_description)
      ) %>%
      select(
        pk = primary_key,
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
    pbp_parse <-
      pbp_parse %>%
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
        id_game,
        pts_home,
        pts_away,
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

    pbp_parse <-
      pbp_parse %>%
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
    pbp_parse <-
      pbp_parse %>%
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

    pbp_parse <-
      pbp_parse %>%
      group_by(id_game) %>%
      mutate(poss_num = row_number()) %>%
      mutate(
        mp = (sec_elapsed - dplyr::lag(sec_elapsed, n = 1L, default = 0L)) / 60
      ) %>%
      ungroup() %>%
      select(-sec_elapsed) %>%
      mutate_at(vars(matches("^pts_team|mp")), funs(coalesce(., 0))) %>%
      mutate_at(vars(matches("^pts")), funs(as.integer))

    pbp_parse <-
      pbp_parse %>%
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
        data = pbp_parse %>% filter(pts1 > 0, pts2 > 0),
        path = path_pbp_parse_error
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
        "pk",
        paste0("id_team", suffix12),
        paste0("slug_team", suffix12),
        paste0("lineup", suffix12)
      )

    # # Other possibly useful columns?
    # cols_order <- c(cols_order, "description", "pc_time_string", "poss_num")
    pbp_parse <-
      pbp_parse %>%
      select(one_of(cols_keep))

    path_export <-
      .export_data_from_path(
        ...,
        data = pbp_parse,
        path = path_pbp_parse
      )
    pbp_parse
  }

.separate_lineup <-
  function(pbp, col, prefix = "x", suffix = 1:5, sep = "-") {
    col <- enquo(col)
    pbp %>%
      separate(!!col, into = glue::glue("{prefix}{suffix}"), sep = sep)
  }

.reshape_pbp <-
  function(...,
           pbp_parse,
           path_pbp = config$path_pbp) {
    suppressMessages(
      pbp <-
        full_join(
          pbp_parse %>%
            filter(is_off1) %>%
            select(
              rn1 = rn,
              pk1 = pk,
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
          pbp_parse %>%
            mutate(is_home2 = !is_home1, is_off2 = !is_off1) %>%
            filter(is_off2) %>%
            select(
              rn2 = rn,
              pk2 = pk,
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
        mutate(rn = coalesce(rn1, rn2), pk = coalesce(pk1, pk2)) %>%
        select(-matches("^(rn|pk)[12]$")) %>%
        select(rn, pk, everything()) %>%
        arrange(rn, pk)
    )

    pbp <-
      pbp %>%
      .separate_lineup(lineup_o, suffix = 1:5) %>%
      .separate_lineup(lineup_d, suffix = 6:10) %>%
      mutate_at(vars(matches("^x")), funs(as.integer))

    pbp <-
      pbp %>%
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

    path_export <-
      .export_data_from_path(
        ...,
        data = pbp,
        path = path_pbp
      )
    pbp
  }

.convert_lineup_to_suffix <-
  function(x) {
    x <- rlang::enquo(x)
    x_chr <- rlang::quo_text(x)
    str_sub(x_chr, start = nchar(x_chr))
  }

.unite_lineup <-
  function(data,
           col,
           prefix_rgx = "x",
           sep = " - ",
           remove = TRUE) {
    col_quo <- rlang::enquo(col)
    suffix <- .convert_lineup_to_suffix(!!col_quo)
    suffix_rgx <-
      case_when(suffix == "1" ~ "0[1-5]",
                suffix == "2" ~ "[01][06-9]")
    rgx <- glue::glue("{prefix_rgx}{suffix_rgx}")
    data %>%
      unite(!!col_quo, matches(rgx), sep = sep, remove = remove)
  }

.summarise_lineup_byside <-
  function(data, col, side) {

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

.summarise_lineup <-
  function(...,
           pbp,
           path_lineup_summary_calc = config$path_lineup_summary_calc) {
    players_nbastatr <- .try_import_players_nbastatr(...)

    pbp_aug <-
      pbp %>%
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

    pbp_aug <-
      pbp_aug %>%
      group_by(id_game, poss_num) %>%
      mutate(player_num = row_number()) %>%
      ungroup() %>%
      mutate_at(vars(player_num), funs(sprintf("x%02d", .)))

    pbp_compare_wide <-
      pbp_aug %>%
      select(id_game, poss_num, pts, player_num, name_player) %>%
      spread(player_num, name_player) %>%
      .unite_lineup(lineup1) %>%
      .unite_lineup(lineup2)

    pbp_aug <-
      pbp_aug %>%
      select(-matches("^(id|name)_player$|^id_team$|^player_num$")) %>%
      group_by(id_game, poss_num) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      inner_join(
        pbp_compare_wide,
        by = c("id_game", "poss_num", "pts")
      )

    lineup_summary_calc <-
      left_join(
        pbp_aug %>% .summarise_lineup_byside(lineup1, side = "o"),
        pbp_aug %>% .summarise_lineup_byside(lineup2, side = "d"),
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
    lineup_summary_calc
  }

.summarise_players_stats <-
  function(data) {
    data %>%
      group_by(id_game, id_player, side) %>%
      summarise(
        poss_calc = n(),
        mp_calc = sum(mp),
        pts_calc = sum(pts)
      ) %>%
      ungroup()
  }

.summarise_players <-
  function(...,
           pbp,
           path_players_summary_calc = config$path_players_summary_calc,
           path_players_game_logs_compare = config$path_players_game_logs_compare,
           path_players_summary_compare = config$path_players_summary_compare) {


    players_game_logs_calc <-
      bind_rows(
        pbp %>%
          filter(side == "o") %>%
          .summarise_players_stats(),
        pbp %>%
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
    players_summary_calc
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

    path_export <-
      .export_data_from_path(
        ...,
        data = teams_summary_calc,
        path = path_teams_summary_calc
      )

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

    path_export <-
      .export_data_from_path(
        ...,
        data = teams_summary_compare,
        path = path_teams_summary_compare
      )
    teams_summary_calc
  }


# .filter_season_type <-
#   function(pbp, season_type, ...) {
#     if(!any("season_type" %in% names(pbp))) {
#       .display_info(
#         "Not filtering for `season_type` since there is `season_type` column in `pbp`.",
#         ...
#       )
#       return(pbp)
#     }
#     # TODO: Implement this better (although this seems like it would work).
#     # .validate_season_type(season_type)
#     # if (season_type == .SEASON_TYPES[3] |
#     #     (season_type != .SEASON_TYPES[1] &
#     #     season_type != .SEASON_TYPES[2])) {
#     #   return(pbp)
#     # }
#     # pbp %>% filter(season_type == !!season_type)
#     pbp %>% filter(season_type == "Regular Season")
#   }

.clean_pbp <-
  function(...,
           # This boolean is a convenience for development. It probably shouldn't
           # be allowed in the final version of this function.
           skip_calc = FALSE,
           # Some of these are included here exclusively for the `.try_skip()` function.
           # This is true with some of the other functions as well.
           path_pbp_raw = config$path_pbp_raw,
           path_pbp = config$path_pbp,
           path_players_summary_calc = config$path_players_summary_calc) {

    # TODO: Keep this `skip` logic? (Mayve only for this function...)
    will_skip <-
      .try_skip(
        ...,
        path_reqs =
          c(
            .get_path_from(..., path = path_pbp_raw)
          ),
        path_deps =
          c(
            .get_path_from(..., path = path_pbp),
            .get_path_from(..., path = path_players_summary_calc)
          )
      )

    if(will_skip) {
      return(invisible(NULL))
    }

    .display_auto_step(
      glue::glue("Cleaning play-by-play data."),
      ...
    )

    pbp_raw <-
      .import_data_from_path(
        ...,
        path = path_pbp_raw
      )

    pbp_parse <-
      .parse_pbp(
        ...,
        pbp_raw = pbp_raw
      )

    pbp <-
      .reshape_pbp(
        ...,
        pbp_parse = pbp_parse
      )

    # Note that the `players_summary_calc` is the only `_calc` object
    # that matters (because the subsequent filtering depends on it).
    if(!skip_calc) {
      lineup_summary_calc <-
        .summarise_lineup(
          ...,
          pbp = pbp
        )

      players_summary_calc <-
        .summarise_players(
          ...,
          pbp = pbp
        )

      teams_summary_calc <-
        .summarise_teams(
          ...,
          players_summary_calc = players_summary_calc
        )
    } else {
      # TODO: There should probably be some logic to make sure that this
      # data was generated from a "recent" version of `pbp`.
      players_summary_calc <-
        .import_data_from_path(
          ...,
          path = path_players_summary_calc
        )
    }

    invisible(
      list(
        pbp = pbp,
        players_summary_calc = players_summary_calc
      )
    )
  }

auto_clean_pbp <-
  function(...,
           season = config$season,
           # season_type = config$season_type,
           skip = config$skip,
           verbose = config$verbose,
           export = config$export,
           backup = config$backup,
           clean = config$clean,
           n_keep = config$n_keep) {
    .clean_pbp(
      ...,
      season = season,
      # season_type = season_type,
      skip = skip,
      verbose = verbose,
      export = export,
      backup = backup,
      clean = clean,
      n_keep = n_keep
    )
  }
