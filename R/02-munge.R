

.separate_lineup <-
  function(pbp, col, prefix = "x", suffix = 1:5, sep = "-") {
    col <- enquo(col)
    pbp %>%
      separate(!!col, into = glue::glue("{prefix}{suffix}"), sep = sep)
  }


.reformat_pbp <-
  function(...,
           pbp,
           path_pbp_reformatted = config$path_pbp_reformatted) {
    suppressMessages(
      pbp <-
        full_join(
          pbp %>%
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
          pbp %>%
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
        path = path_pbp_reformatted
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
  function(.data,
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
    .data %>%
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


# Filtering out entire "stints" where a single player does not meet the criteria.
.POSS_MIN <- 0
.GP_MIN <- .POSS_MIN
.MP_MIN <- .POSS_MIN
..filter_pbp1 <-
  function(...,
           pbp,
           players_summary_calc,
           poss_min = .POSS_MIN,
           gp_min = .GP_MIN,
           mp_min = .MP_MIN) {
    players_summary_calc_filt <-
      players_summary_calc %>%
      filter(
        poss_calc_total < poss_min |
          gp_calc_total < gp_min |
          mp_calc_total < mp_min
      )
    pbp %>%
      anti_join(players_summary_calc_filt, by = "id_player") %>%
      group_by(rn) %>%
      filter(n() == 10L) %>%
      ungroup()
  }

# The "original" implementation (where individual players are filtererd out,
# meaning that the stint may have less than 9 players (i.e. indicator variables)
# when spread into "wide" form.
..filter_pbp2 <-
  function(...,
           pbp,
           players_summary_calc,
           poss_min = .POSS_MIN,
           gp_min = .GP_MIN,
           mp_min = .MP_MIN) {
    players_summary_calc_filt <-
      players_summary_calc %>%
      filter(
        poss_calc_total >= poss_min,
        gp_calc_total >= gp_min,
        mp_calc_total >= mp_min
      )
    pbp %>%
      semi_join(players_summary_calc_filt, by = "id_player")
  }

# + It is worth separating this into its own function so that the `*min` parameters
# can be "abstracted" away (with the ellipses).
# + NOTE: I don't think the filtering is done correclty here! (Entire stints involving
# the filtered out players should be removed!)
# UPDATE: This is now implemented in one of the `..filter*()` functions,
# but it doesn't produce better results(?). A seperate `..filter()` function
# has been created to have the "original" filtering implementation.
.filter_pbp <-
  function(...,
           pbp,
           # Do this so that this function can be used more "dynamically"
           # (i.e. outside the parent `munge*()` function, where `players_summary_calc`
           # may not be calculated immediately before hand).
           players_summary_calc = NULL,
           path_players_summary_calc = config$path_players_summary_calc){

    if(is.null(players_summary_calc)) {
      players_summary_calc <-
        .import_data_from_path(
          ...,
          path = path_players_summary_calc
        )
    }
    n_row_before <- pbp %>% nrow()
    res <-
      ..filter_pbp2(
        ...,
        pbp = pbp,
        players_summary_calc = players_summary_calc
      )

    n_row_after <- res %>% nrow()
    n_poss_rem <- (n_row_before - n_row_after) / 10
    .display_info(
      glue::glue(
        "{scales::comma(n_row_before)} records before filtering; ",
        "{scales::comma(n_row_after)} records after_filter; ",
        "({scales::comma(n_poss_rem)} posss removed by filtering)."
        ),
      ...
    )
    res
  }

# .convert_to_poss_side, start ----
.check_poss_long_side_dups <-
  function(...,
           side,
           poss_long_side,
           path_poss_long_error_side = config$path_poss_long_error_side) {

    poss_long_side_dups <-
      poss_long_side %>%
      count(rn, xid_player, sort = TRUE) %>%
      filter(n > 1L)

    n_dups <- nrow(poss_long_side_dups)
    if(n_dups > 0L) {
      .display_info(
        glue::glue(
          "There are {scales::comma(n_dups)} rows with more than 1 ",
          "player-side-possession combination."
        ),
        ...
      )

      players_nbastatr <- .try_import_players_nbastatr(...)

      poss_long_side_dups <-
        poss_long_side_dups %>%
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
          side = side,
          data = poss_long_side_dups,
          path = path_poss_long_error_side
        )
    }
    poss_long_side_dups
  }


.convert_to_poss_long_side <-
  function(...,
           side,
           pbp,
           path_poss_long_side = config$path_poss_long_side) {
    poss_long_side <-
      pbp %>%
      filter(side == !!side) %>%
      mutate(xid_player = sprintf("%s%07d", side, as.integer(id_player)))

    poss_long_side_dups <-
      poss_long_side %>%
      .check_poss_long_side_dups(
        ...,
        side = side,
        poss_long_side = poss_long_side
      )

    poss_long_side <-
      poss_long_side %>%
      anti_join(poss_long_side_dups, by = c("rn", "xid_player")) %>%
      arrange(rn, xid_player) %>%
      mutate(dummy = 1L) %>%
      select(rn, pts, pk, xid_player, dummy)

    path_export <-
      .export_data_from_path(
        ...,
        data = poss_long_side,
        path = path_poss_long_side
      )
    poss_long_side
  }

# This is useful just to separate functionality.
# Maybe change this so that it doesn't accept `...`, since it's not intended
# to "catch" anything (e.g. `side`). This is how `.summarise_poss_wide_side()`
# is written. (The downside is that the calling function has to know that
# it can't pass extra arguments.)
# .widen_poss_long_side <- function(...)

# cols_summ <- c("cyl", "carb")
# cols_grp <- mtcars %>% names() %>% setdiff(cols_summ)
# cols_summ <- syms(cols_summ)
# mtcars %>% group_by_at(vars(!!!cols_summ))

# This is useful to "abstract away" `collapse`.
.collapse_poss_wide_side <-
  function(...,
           poss_wide_side,
           scale = .SCALE,
           collapse = .COLLAPSE) {
    # TODO: Make a switch statement to tell the user about any combination
    # of `scale` and `collapse`?
    if(scale & !collapse) {
      .display_warning(
        glue::glue(
          "Setting {usethis::ui_field('scale')} = {scale} and ",
          "usethis::ui_field('collapse')} = {collapse}` is probably a bad idea."
        ),
        ...
      )
    }
    if(collapse) {
      # cols_grp <- c("pts", "n_poss")
      poss_wide_side <-
        poss_wide_side %>%
        # mutate(n_poss = 1) %>%
        # select(pts, n_poss, everything()) %>%
        group_by_at(vars(-pts, -n_poss)) %>%
        summarise_at(vars(pts, n_poss), funs(sum)) %>%
        ungroup()
    }
    poss_wide_side %>%
      # select(pts, n_poss, everything()) %>%
      mutate(pp100poss = 100 * pts / n_poss) %>%
      select(pp100poss, pts, n_poss, everything())
  }

# .pull_max <- function(data, col) {
#   col <- enquo(col)
#   data %>%
#     summarise_at(vars(!!col), funs(max)) %>%
#     pull(!!col)
# }

.convert_to_poss_wide_side <-
  function(...,
           scale = .SCALE,
           poss_long_side,
           path_poss_wide_side = config$path_poss_wide_side) {

    if(scale) {

      # Note that this action was created as a shortcut to `spread()`
      # and `summarise()` the "long" data, which is done more "formally"
      # in the `.collapse*()` function.
      n_poss_max <-
        poss_long_side %>%
        group_by(pk) %>%
        summarise_at(
          vars(xid_player),
          funs(paste0(., collapse = "-", sep = ""))
        ) %>%
        ungroup() %>%
        select(-pk) %>%
        count(xid_player, sort = TRUE) %>%
        slice(1) %>%
        pull(n)

      poss_long_side <-
        poss_long_side %>%
        mutate_at(vars(dummy), funs(. / n_poss_max))

    }

    poss_wide_side <-
      poss_long_side %>%
      spread(xid_player, dummy, fill = 0L) %>%
      select(-rn, -pk) %>%
      mutate(n_poss = 1) %>%
      select(pts, n_poss, everything())

    poss_wide_side <-
      .collapse_poss_wide_side(
        ...,
        poss_wide_side = poss_wide_side
      )

    path_export <-
      .export_data_from_path(
        ...,
        data = poss_wide_side,
        path = path_poss_wide_side
      )
    poss_wide_side
  }

.convert_to_poss_side <-
  function(..., pbp) {

    poss_long_side <-
      .convert_to_poss_long_side(
        ...,
        pbp = pbp
      )

    poss_wide_side <-
      .convert_to_poss_wide_side(
        ...,
        poss_long_side = poss_long_side
      )
    poss_wide_side
  }
# .convert_to_poss_side, end ----

# ..munge_pbp.tibble <-
#   function(...) {
#
#   }

.munge_pbp <-
  function(...,
           # Note: Should edit out this `skip_` stuff later. However, it's useful for
           # experimenting with some stuff right now.s
           skip_reformat = TRUE,
           # skip_reformat = FALSE,
           path_pbp_reformatted = config$path_pbp_reformatted,
           skip_compare = TRUE,
           # skip_compare = FALSE,
           path_pbp = config$path_pbp,
           path_poss_wide_side = config$path_poss_wide_side,
           path_players_summary_calc = config$path_players_summary_calc) {

    will_skip <-
      .try_skip(
        ...,
        path_reqs =
          c(
            .get_path_from(..., path = path_pbp)
          ),
        path_deps =
          c(
            .get_path_from(..., path = path_players_summary_calc),
            .get_path_from(..., path = path_poss_wide_side, side = "o"),
            .get_path_from(..., path = path_poss_wide_side, side = "d")
          )
      )
    if(will_skip) {
      return(invisible(NULL))
    }

    .display_auto_step(
      glue::glue("Step 2: Munging play-by-play data."),
      ...
    )

    if(!skip_reformat) {
      pbp <-
        .import_data_from_path(
          ...,
          path = path_pbp
        )

      # UseMethod("..munge_pbp")

      pbp <-
        .reformat_pbp(
          ...,
          pbp = pbp
        )
    } else {
      pbp <-
        .import_data_from_path(
          ...,
          path = path_pbp_reformatted
        )
    }

    # Note that the `players_summary_calc` is the only `_calc` object
    # that that matters
    # (because the subsequent filtering depends on it).
    if(!skip_compare) {
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
      players_summary_calc <-
        .import_data_from_path(
          ...,
          path = path_players_summary_calc
        )
    }
    # Skip this for now...
    # pbp <-
    #   .filter_pbp(
    #     ...,
    #     pbp = pbp,
    #     players_summary_calc = players_summary_calc
    #   )

    poss_o <-
      .convert_to_poss_side(
        ...,
        pbp = pbp,
        side = "o"
      )

    poss_d <-
      .convert_to_poss_side(
        ...,
        pbp = pbp,
        side = "d"
      )

    invisible(
      list(
        poss_o = poss_o,
        poss_d = poss_d
      )
    )
  }

munge_pbp_auto <-
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
    .munge_pbp(
      ...,
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

