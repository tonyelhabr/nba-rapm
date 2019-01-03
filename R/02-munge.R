

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
    # pbp <-
    #   ..filter_pbp1(
    #     ...,
    #     pbp = pbp,
    #     players_summary_calc = players_summary_calc
    #   )
    pbp <-
      ..filter_pbp2(
        ...,
        pbp = pbp,
        players_summary_calc = players_summary_calc
      )

    n_row_after <- pbp %>% nrow()
    n_row_rem <- n_row_before - n_row_after
    .display_info(
      glue::glue(
        "{scales::comma(0.1 * n_row_before)}/{scales::comma(0.1 * n_row_after)} ",
        "possessions before/after filtering; ",
        "(i.e. {scales::comma(0.1 * n_row_rem)} removed by filtering)."
      ),
      ...
    )
    pbp
  }

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

    # .dummy <-  switch(side, o = 1L, d = -1L)
    .dummy <- 1L
    poss_long_side <-
      poss_long_side %>%
      anti_join(poss_long_side_dups, by = c("rn", "xid_player")) %>%
      arrange(rn, xid_player) %>%
      mutate(dummy = .dummy) %>%
      select(rn, pts, pk, xid_player, dummy)

    path_export <-
      .export_data_from_path(
        ...,
        side = side,
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
           side,
           poss_wide_side,
           scale,
           collapse = .COLLAPSE) {
    # TODO: Make a switch statement to tell the user about any combination
    # of `scale` and `collapse`?
    if(scale & !collapse) {
      .display_warning(
        glue::glue(
          "Setting {usethis::ui_field('scale')} = {scale} and ",
          "{usethis::ui_field('collapse')} = {collapse}` is probably a bad idea."
        ),
        ...
      )
    }
    if(collapse) {
      # Do something less "hard-coded" here?
      poss_wide_side <-
        poss_wide_side %>%
        # mutate(n_poss = 1) %>%
        # select(pts, n_poss, everything()) %>%
        group_by_at(vars(-pts, -n_poss)) %>%
        summarise_at(vars(pts, n_poss), funs(sum)) %>%
        ungroup()
    }
    if(side == "d") {
      poss_wide_side <-
        poss_wide_side %>%
        mutate_at(vars(pts), funs(-.))
    }
    poss_wide_side %>%
      # select(pts, n_poss, everything()) %>%
      mutate(pp100poss = 100 * pts / n_poss) %>%
      # mutate(pp100poss = pts / n_poss) %>%
      select(pp100poss, pts, n_poss, everything())
  }

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
        # side = side,
        scale = scale,
        poss_wide_side = poss_wide_side
      )

    path_export <-
      .export_data_from_path(
        ...,
        # side = side,
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


# ..munge_pbp.tibble <-
#   function(...) {
#
#   }

.munge_pbp <-
  function(...,
           skip_compare = TRUE,
           # skip_compare = FALSE,
           path_pbp = config$path_pbp,
           path_players_summary_calc = config$path_players_summary_calc,
           path_poss_wide_side = config$path_poss_wide_side) {

    will_skip <-
      .try_skip(
        ...,
        path_reqs =
          c(
            .get_path_from(..., path = path_pbp),
            .get_path_from(..., path = path_players_summary_calc)
          ),
        path_deps =
          c(
            .get_path_from(..., path = path_poss_wide_side, side = "o"),
            .get_path_from(..., path = path_poss_wide_side, side = "d")
          )
      )
    if(will_skip) {
      return(invisible(NULL))
    }

    .display_auto_step_step(
      glue::glue("Step 2: Munging play-by-play data."),
      ...
    )

    pbp <-
      .import_data_from_path(
        ...,
        path = path_pbp
      )

    players_summary_calc <-
      .import_data_from_path(
        ...,
        path = path_players_summary_calc
      )

    pbp <-
      .filter_pbp(
        ...,
        pbp = pbp,
        players_summary_calc = players_summary_calc
      )

    poss_o <-
      .convert_to_poss_side(
        ...,
        side = "o",
        pbp = pbp
      )

    poss_d <-
      .convert_to_poss_side(
        ...,
        side = "d",
        pbp = pbp
      )

    invisible(
      list(
        poss_o = poss_o,
        poss_d = poss_d
      )
    )
  }

auto_munge_pbp <-
  function(...,
           season = config$season,
           poss_min = config$poss_min,
           gp_min = config$gp_min,
           mp_min = config$mp_min,
           scale = config$scale,
           collapse = config$collapse,
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
      scale = scale,
      collapse = collapse,
      skip = skip,
      verbose = verbose,
      export = export,
      backup = backup,
      clean = clean,
      n_keep = n_keep
    )
  }

