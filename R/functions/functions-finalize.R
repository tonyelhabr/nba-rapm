
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
  function(path_rapm_fit_side_format,
           path_rapm_estimates_side_format,
           season,
           ...) {
    fit <-
      .import_data_from_path_format(
        path_format = path_rapm_fit_side_format,
        season = season,
        ...
      )

    estimates <-
      .extract_estimates(
        fit = fit,
        ...
      )

    .export_data_from_path_format(
      data = estimates,
      path_format = path_rapm_estimates_side_format,
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

    cols_players_summary <-
      .get_cols_players_summary()

    cols_raw <-
      c(
        .COLS_ESIMATES_ORDER,
        cols_players_summary
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

.join_estimates_with_players_summary <-
  function(estimates,
           path_players_summary_format,
           season,
           ... {

    players_summary <-
      .import_data_from_path_format(
        path_format = path_players_summary_format,
        season = season,
        ...
      )

    estimates_pretty <-
      estimates %>%
      mutate_at(vars(matches("rapm$")), funs(rnk = row_number(desc(.)))) %>%
      left_join(players_summary, by = "id")


    cols_estimates_pretty <-
      # estimates_pretty %>%
      .get_cols_estimates_pretty()

    estimates_pretty %>%
      select(one_of(cols_estimates_pretty))
  }

extract_rapm_estimates <-
  function(path_rapm_fit_o_format,
           path_rapm_fit_d_format,
           path_players_summary_format,
           path_rapm_estimates_o_format,
           path_rapm_estimates_d_format,
           path_rapm_estimates_format,
           season = .SEASON,
           skip = .SKIP,
           ...) {

    will_skip <-
      .try_skip(
        skip = skip,
        season = season,
        path_format_reqs =
          c(
            path_rapm_fit_o_format,
            path_rapm_fit_d_format,
            path_players_summary_format
          ),
        path_format_deps =
          c(
            path_rapm_estimates_o_format,
            path_rapm_estimates_d_format,
            path_rapm_estimates_format
          ),
        verbose = verbose,
        # call_name = rlang::call_name(),
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
        path_rapm_fit_side_format = path_rapm_fit_o_format,
        path_rapm_estimates_side_format = path_rapm_estimates_o_format
      )
    estimates_d <-
      .extract_rapm_estimates_partially(,
        path_rapm_fit_side_format = path_rapm_fit_d_format,
        path_rapm_estimates_side_format = path_rapm_estimates_d_format
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

    .join_estimates_with_players_summary_possibly <-
      purrr::possibly(
        ~.join_estimates_with_players_summary(
          estimates = estimates,
          path_players_summary_format = path_players_summary_format,
          season = season,
          verbose = verbose,
          ...
        ),
        otherwise = NULL
      )

    estimates_pretty <-
      .join_estimates_with_players_summary_possibly()

    if(!is.null(estimates_pretty)) {
      estimates <- estimates_pretty
      .display_info(
        "Successfully joined `players_summary` with `estimates`.",
        ...
      )
    } else {
      .display_warning(
        "Could not join `estimates` with `players_summary`.",
        ...
      )
    }

    path_export <-
      .export_data_from_path_format(
        data = estimates,
        path_format = path_rapm_estimates_format,
        season = season,
        ...
      )
    invisible(estimates)
  }

auto_extract_rapm_estimates <-
  purrr::partial(
    extract_rapm_estimates,
    path_rapm_fit_o_format = args$path_rapm_fit_o_format,
    path_rapm_fit_d_format = args$path_rapm_fit_d_format,
    path_players_summary_format = args$path_players_summary_format,
    path_rapm_estimates_o_format = args$path_rapm_estimates_o_format,
    path_rapm_estimates_d_format = args$path_rapm_estimates_d_format,
    path_rapm_estimates_format = args$path_rapm_estimates_format,
    season = args$season,
    skip = args$skip_fit,
    verbose = args$verbose,
    export = args$export,
    backup = args$backup,
    clean = args$clean,
    n_keep = args$n_keep
  )
