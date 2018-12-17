
.extract_estimates <-
  function(..., fit) {
    fit %>%
      broom::tidy() %>%
      filter(term != "(Intercept)") %>%
      mutate_at(vars(term), funs(str_replace(., "^.", ""))) %>%
      mutate_at(
        vars(term),
        funs(str_replace_all(., "^[0]+", "") %>% as.integer())
      ) %>%
      arrange(desc(estimate)) %>%
      select(id_player = term, rapm = estimate) %>%
      filter(!is.na(id_player))
  }

.extract_rapm_estimates_side <-
  function(...,
           side,
           path_rapm_fit_side,
           path_rapm_estimates_side) {
    fit <-
      .import_data_from_path(
        ...,
        path = path_rapm_fit_side
      )

    estimates <-
      .extract_estimates(
        ...,
        fit = fit
      ) %>%
      mutate(prefix = side)

    .export_data_from_path(
      ...,
      data = estimates,
      path = path_rapm_estimates_side
    )

    invisible(estimates)
  }

.extract_rapm_estimates_o <-
  function(...) {
    .extract_rapm_estimates_side(
      ...,
      side = "o",
      path_rapm_fit_side = config$path_rapm_fit_o,
      path_rapm_estimates_side = config$path_rapm_estimates_o
    )
  }

.extract_rapm_estimates_d <-
  function(...) {
    .extract_rapm_estimates_side(
      ...,
      side = "d",
      path_rapm_fit_side = config$path_rapm_fit_d,
      path_rapm_estimates_side = config$path_rapm_estimates_d
    )
  }

extract_rapm_estimates <-
  function(...,
           # Note that some of these are used for `.try_skip()`.
           path_rapm_fit_o = config$path_rapm_fit_o,
           path_rapm_fit_d = config$path_rapm_fit_d,
           path_players_summary_calc = config$path_players_summary_calc,
           path_rapm_estimates = config$path_rapm_estimates) {

    will_skip <-
      .try_skip(
        ...,
        path_reqs =
          c(
            path_rapm_fit_o,
            path_rapm_fit_d,
            path_players_summary_calc
          ),
        path_deps =
          c(
            path_rapm_estimates
          )
      )

    if(will_skip) {
      return(invisible(NULL))
    }

    estimates_o <- .extract_rapm_estimates_o(...)
    estimates_d <- .extract_rapm_estimates_d(...)

    estimates <-
      bind_rows(
        estimates_o,
        estimates_d
      ) %>%
      spread(prefix, rapm) %>%
      rename_at(vars(o, d), funs(paste0(., "rapm"))) %>%
      mutate(rapm = orapm + drapm) %>%
      arrange(desc(rapm))


    # Use this if cross-checking "unexpected" RAPM values with calculated stats
    # to try to identify why these unexpected values may be like they are.
    # players_summary_calc <-
    #   .import_data_from_path(
    #     ...,
    #     path = path_players_summary_calc
    #   )
    players_nbastatr <-
      .try_import_players_nbastatr(...)
    players_slim <-
      players %>%
      select(id_player, name_player, slug_team, season = year_season_first)

    estimates_pretty_base <-
      estimates %>%
      mutate_at(vars(matches("rapm$")), funs(rank = row_number(desc(.)))) %>%
      left_join(players_slim, by = "id_player")

    prefix_cols <- c("o", "d", "")
    cols_order_base <-
      c(
        "season",
        "id_player",
        "name_player",
        paste0(prefix_cols, "rapm"),
        paste0(prefix_cols, "rapm_rank")
      )
    cols_order_other <-
      setdiff(names(players_slim), cols_order_base)
    cols_order <- c(cols_order_base, cols_order_other)

    estimates_pretty <-
      estimates_pretty_base %>%
      select(one_of(cols_order))

    path_export <-
      .export_data_from_path(
        ...,
        data = estimates_pretty,
        path = path_rapm_estimates
      )
    invisible(estimates)
  }

extract_rapm_estimates_auto <-
  purrr::partial(
    extract_rapm_estimates,
    season = config$season,
    skip = config$skip,
    verbose = config$verbose,
    export = config$export,
    backup = config$backup,
    clean = config$clean,
    n_keep = config$n_keep
  )
