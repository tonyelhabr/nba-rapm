
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
      select(id = term, rapm = estimate)
  }

.extract_rapm_estimates <-
  function(...,
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
      )

    .export_data_from_path(
      ...,
      data = estimates,
      path = path_rapm_estimates_side
    )

    invisible(estimates)
  }

.get_cols_estimates_pretty <-
  function(...) {
    cols_estimates_order <-
      c("name",
        "id",
        paste0(c("o", "d", ""), "rapm"),
        paste0(c("o", "d", ""), "rapm_rnk")
      )

    cols_players_summary_calc <-
      .get_cols_players_summary_calc()

    cols_raw <-
      c(
        cols_estimates_order,
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
  function(...,
           estimates,
           path_players_summary_calc) {

    players_summary_calc <-
      .import_data_from_path(
        ...,
        path = path_players_summary_calc
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
  function(...,
           path_rapm_fit_o = config$path_rapm_fit_o,
           path_rapm_fit_d = config$path_rapm_fit_d,
           path_players_summary_calc = config$path_players_summary_calc,
           path_rapm_estimates_o = config$path_rapm_estimates_o,
           path_rapm_estimates_d = config$path_rapm_estimates_d,
           path_rapm_estimates) {

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
            path_rapm_estimates_o,
            path_rapm_estimates_d,
            path_rapm_estimates
          )
      )

    if(will_skip) {
      return(invisible(NULL))
    }

    .extract_rapm_estimates_partially <-
      purrr::partial(
        .extract_rapm_estimates,
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
          ...,
          estimates = estimates,
          path_players_summary_calc = path_players_summary_calc
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
        ...,
        data = estimates,
        path = path_rapm_estimates
      )
    invisible(estimates)
  }

auto_extract_rapm_estimates <-
  purrr::partial(
    extract_rapm_estimates,
    season = config$season,
    path_rapm_estimates = config$path_rapm_estimates,
    skip = config$skip_fit,
    verbose = config$verbose,
    export = config$export,
    backup = config$backup,
    clean = config$clean,
    n_keep = config$n_keep
  )
