
.extract_estimates <-
  function(fit, ...) {
    fit %>%
      broom::tidy() %>%
      filter(term != "(Intercept)") %>%
      mutate_at(vars(term), funs(str_replace(., "^.", ""))) %>%
      mutate_at(
        vars(term),
        funs(str_replace_all(., "^[0]+", "") %>% as.integer())
      ) %>%
      arrange(desc(estimate)) %>%
      select(id = term, rapm = estimate)
  }

.extract_rapm_estimates <-
  function(path_rapm_fit_side,
           path_rapm_estimates_side,
           season,
           ...) {
    fit <-
      .import_data_from_path(
        path = path_rapm_fit_side,
        season = season,
        ...
      )

    estimates <-
      .extract_estimates(
        fit = fit,
        ...
      )

    .export_data_from_path(
      data = estimates,
      path = path_rapm_estimates_side,
      season = season,
      ...
    )

    invisible(estimates)
  }

.get_cols_estimates_pretty <-
  function(...) {
    .PREFIX_COLS_ESTIMATES_ORDER <- c("o", "d", "")
    .COLS_ESIMATES_ORDER <-
      c("name",
        "id",
        paste0(.PREFIX_COLS_ESTIMATES_ORDER, "rapm"),
        paste0(.PREFIX_COLS_ESTIMATES_ORDER, "rapm_rnk")
      )

    cols_players_summary_calc <-
      .get_cols_players_summary_calc()

    cols_raw <-
      c(
        .COLS_ESIMATES_ORDER,
        cols_players_summary_calc
      )
    cols_fct <-
      cols_raw %>%
      factor(levels = unique(cols_raw), ordered = TRUE)
    cols <-
      cols_fct %>%
      unique() %>%
      as.character()
    cols
  }

.join_estimates_with_players_summary_calc <-
  function(estimates,
           path_players_summary_calc,
           season,
           ...) {

    players_summary_calc <-
      .import_data_from_path(
        path = path_players_summary_calc,
        season = season,
        ...
      )

    estimates_pretty <-
      estimates %>%
      mutate_at(vars(matches("rapm$")), funs(rnk = row_number(desc(.)))) %>%
      left_join(players_summary_calc, by = "id")


    cols_estimates_pretty <-
      # estimates_pretty %>%
      .get_cols_estimates_pretty()

    estimates_pretty %>%
      select(one_of(cols_estimates_pretty))
  }

extract_rapm_estimates <-
  function(path_rapm_fit_o,
           path_rapm_fit_d,
           path_players_summary_calc,
           path_rapm_estimates_o,
           path_rapm_estimates_d,
           path_rapm_estimates,
           season = .SEASON,
           ...) {

    will_skip <-
      .try_skip(
        season = season,
        path_reqs =
          c(
            path_rapm_fit_o,
            path_rapm_fit_d,
            path_players_summary_calc
          ),
        path_deps =
          c(
            path_rapm_estimates_o,
            path_rapm_estimates_d,
            path_rapm_estimates
          ),
        ...
      )

    if(will_skip) {
      return(invisible(NULL))
    }

    .extract_rapm_estimates_partially <-
      purrr::partial(
        .extract_rapm_estimates,
        season = season,
        ...
      )

    estimates_o <-
      .extract_rapm_estimates_partially(
        path_rapm_fit_side = path_rapm_fit_o,
        path_rapm_estimates_side = path_rapm_estimates_o
      )
    estimates_d <-
      .extract_rapm_estimates_partially(,
        path_rapm_fit_side = path_rapm_fit_d,
        path_rapm_estimates_side = path_rapm_estimates_d
      )

    # browser()
    estimates <-
      bind_rows(
        estimates_o %>% mutate(prefix = "o"),
        estimates_d %>% mutate(prefix = "d")
      ) %>%
      spread(prefix, rapm) %>%
      rename_at(vars(o, d), funs(paste0(., "rapm"))) %>%
      mutate(rapm = orapm + drapm) %>%
      arrange(desc(rapm))

    .join_estimates_with_players_summary_calc_possibly <-
      purrr::possibly(
        ~.join_estimates_with_players_summary_calc(
          estimates = estimates,
          path_players_summary_calc = path_players_summary_calc,
          season = season,
          verbose = verbose,
          ...
        ),
        otherwise = NULL
      )

    estimates_pretty <-
      .join_estimates_with_players_summary_calc_possibly()

    if(!is.null(estimates_pretty)) {
      estimates <- estimates_pretty
      .display_info(
        "Successfully joined `players_summary_calc` with `estimates`.",
        ...
      )
    } else {
      .display_warning(
        "Could not join `estimates` with `players_summary_calc`.",
        ...
      )
    }

    path_export <-
      .export_data_from_path(
        data = estimates,
        path = path_rapm_estimates,
        season = season,
        ...
      )
    invisible(estimates)
  }

auto_extract_rapm_estimates <-
  purrr::partial(
    extract_rapm_estimates,
    path_rapm_fit_o = config$path_rapm_fit_o,
    path_rapm_fit_d = config$path_rapm_fit_d,
    path_players_summary_calc = config$path_players_summary_calc,
    path_rapm_estimates_o = config$path_rapm_estimates_o,
    path_rapm_estimates_d = config$path_rapm_estimates_d,
    path_rapm_estimates = config$path_rapm_estimates,
    season = config$season,
    skip = config$skip_fit,
    debug = config$debug,
    verbose = config$verbose,
    export = config$export,
    backup = config$backup,
    clean = config$clean,
    n_keep = config$n_keep
  )
