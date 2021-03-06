

# Filtering out entire "stints" where a single player does not meet the criteria.
.POSS_MIN <- 0
.GP_MIN <- .POSS_MIN
.MP_MIN <- .POSS_MIN
..filter_pbp1 <-
  function(...,
           pbp,
           players_summary_compare,
           poss_min = .POSS_MIN,
           gp_min = .GP_MIN,
           mp_min = .MP_MIN) {
    players_summary_compare_filt <-
      players_summary_compare %>%
      filter(
        poss_both_calc < poss_min |
          gp_calc < gp_min |
          mp_both_calc < mp_min
      )

    pbp %>%
      anti_join(players_summary_compare_filt, by = "id_player") %>%
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
           players_summary_compare,
           poss_min = .POSS_MIN,
           gp_min = .GP_MIN,
           mp_min = .MP_MIN) {
    players_summary_compare_filt <-
      players_summary_compare %>%
      filter(
        poss_both_calc >= poss_min,
        gp_calc >= gp_min,
        mp_both_calc >= mp_min
      )
    pbp %>%
      semi_join(players_summary_compare_filt, by = "id_player")
  }

# + It is worth separating this into its own function so that the `*min` parameters
# can be "abstracted" away (with the ellipses).
# + NOTE: I don't think the filtering is done correclty here! (Entire stints involving
# the filtered out players should be removed!)
# UPDATE: This is now implemented in one of the `..filter*()` functions,
# but it doesn't produce better results(?). A seperate `..filter()` function
# has been created to have the "original" filtering implementation.
# + Make the function "flexibile/dynamic" with `NULL` parameters.
# Do this so that this function can be used
# (i.e. outside the parent `reshape*()` function, where `players_summary_compare`
# may not be calculated immediately before hand).
# + May or may not actually use `side` in some of the following functions.
.filter_pbp <-
  function(...,
           # pbp = NULL,
           path_pbp = config$path_pbp,
           # path_players_summary_compare = NULL,
           path_players_summary_compare = config$path_players_summary_compare){

    # if(is.null(pbp))
    pbp <-
      .import_data_from_path(
        ...,
        path = path_pbp
      )

    players_summary_compare <-
      .import_data_from_path(
        ...,
        side = NULL,
        path = path_players_summary_compare
      )
    n_row_before <- pbp %>% nrow()
    # pbp <-
    #   ..filter_pbp1(
    #     ...,
    #     pbp = pbp,
    #     players_summary_compare = players_summary_compare
    #   )
    pbp <-
      ..filter_pbp2(
        ...,
        pbp = pbp,
        players_summary_compare = players_summary_compare
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
           # "Catch" `side` to prevent it from being passed to `.try_import_players_nbastatr()`.
           side,
           poss_long_side,
           path_poss_long_error_side = config$path_poss_long_error_side) {

    poss_long_side_dups <-
      poss_long_side %>%
      count(rn, xid_player, sort = TRUE) %>%
      filter(n > 1L)

    # # Note: Taking this out because it's sort of verbose.
    # n_dups <- poss_long_side_dups %>% nrow()
    # if(n_dups > 0L) {
    #   .display_info(
    #     glue::glue(
    #       "There are {scales::comma(n_dups)} rows with more than 1 ",
    #       "player-side-possession combination."
    #     ),
    #     ...
    #   )
    #
    #   players_nbastatr <- .try_import_players_nbastatr(...)
    #
    #   poss_long_side_dups <-
    #     poss_long_side_dups %>%
    #     mutate(
    #       id_player = xid_player %>% str_replace("^[od]", "") %>% as.integer()
    #     ) %>%
    #     left_join(
    #       players_nbastatr %>% select(id_player, name_player),
    #       by = c("id_player")
    #     )
    #
    #   path_export <-
    #     .export_data_from_path(
    #       ...,
    #       # side = side,
    #       data = poss_long_side_dups,
    #       path = path_poss_long_error_side
    #     )
    # }
    poss_long_side_dups
  }

.convert_to_poss_long_side <-
  function(...,
           # Need `side` here (for `dummy`).
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

    .dummy <- switch(side, o = 1L, d = -1L)
    # .dummy <- 1L
    poss_long_side <-
      poss_long_side %>%
      anti_join(poss_long_side_dups, by = c("rn", "xid_player")) %>%
      arrange(rn, xid_player) %>%
      mutate(dummy = .dummy) %>%
      select(rn, pts, pk, id_game, half, period, xid_player, dummy)

    path_export <-
      .export_data_from_path(
        ...,
        side = side,
        data = poss_long_side,
        path = path_poss_long_side
      )
    poss_long_side
  }

.collapse_poss_wide_side <-
  function(...,
           # side,
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
      # Do/don't group by `game`/`half`? (Was NOT originally).
      poss_wide_side <-
        poss_wide_side %>%
        group_by_at(vars(-pts, -n_poss)) %>%
        # group_by_at(vars(-id_game, -half, -period, -pts, -n_poss)) %>%
        summarise_at(vars(pts, n_poss), funs(sum)) %>%
        ungroup()
    }

    # If `scale = TRUE`, then don't use an actual version of `pp100poss`.
    # (Use `pts`/`poss` instead.)
    .scale_factor <- ifelse(scale, 1L, 100L)
    poss_wide_side %>%
      # select(pts, n_poss, everything()) %>%
      mutate(pp100poss = .scale_factor * pts / n_poss) %>%
      # mutate(pp100poss = pts / n_poss) %>%
      select(pp100poss, pts, n_poss, everything())
  }

.convert_to_poss_wide_side <-
  function(...,
           # side,
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

    n_row <- poss_wide_side %>% nrow()
    n_col <- poss_wide_side %>% ncol()
    .display_info(
      glue::glue("Wide data dimensions: {scales::comma(n_row)} x {n_col}."),
      ...
    )

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

.convert_to_poss_both <-
  function(..., pbp, path_poss_long_side = config$path_poss_long_side) {

    # Note that it doesn't seem easier to re-create these with `.convert_to_poss_long_side()`.
    poss_long_o <-
      .import_data_from_path(
        ...,
        side = "o",
        path = path_poss_long_side
      )
    poss_long_d <-
      .import_data_from_path(
        ...,
        side = "d",
        path = path_poss_long_side
      )

    # Is binding really what should be done here? (Update: Yes.)
    poss_long_both <-
      bind_rows(
        poss_long_o,
        poss_long_d
      )

    poss_long_o %>% count(rn)
    poss_long_both %>%
      count(rn)
    poss_wide_both <-
      .convert_to_poss_wide_side(
        ...,
        side = "both",
        poss_long_side = poss_long_both
      )
  }

.prep_models <-
  function(...) {

    .display_auto_step(
      glue::glue("Preparing data for modelling."),
      ...
    )

    pbp <-
      .filter_pbp(
        ...
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

    poss_both <-
      .convert_to_poss_both(
        ...,
        pbp = pbp
      )

    invisible()
  }

auto_prep_models <-
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

    .prep_models(
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

