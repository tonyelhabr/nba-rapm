

.get_x_glmnet <-
  function(data_wide, fmla, ...) {
    fmla %>%
      model.matrix(data_wide)
  }

.get_y_glmnet <-
  function(data_wide, ...) {
    data_wide %>%
      pull(pts)
  }

.get_x_glmnet <-
  function(data_wide, fmla, ...) {
    fmla %>%
      model.matrix(data_wide)
  }
.get_y_glmnet <-
  function(data_wide, ...) {
    data_wide %>%
      pull(pts)
  }

.get_lambda_optm <-
  function(x, y, ..., side = c("o", "d"), seed = .SEED) {
    set.seed(seed)
    fit_glmnet_cv <-
      glmnet::cv.glmnet(
        parallel = TRUE,
        x = x,
        y = y,
        alpha = 0
      )
    fit_glmnet_cv$lambda.min
  }

.get_estimates <-
  function(x, y, lambda, ...) {
    fit_glmnet <-
      glmnet::glmnet(
        intercept = TRUE,
        x = x,
        y = y,
        alpha = 0,
        lambda = lambda
      )

    estimates <-
      fit_glmnet %>%
      broom::tidy() %>%
      filter(term != "(Intercept)") %>%
      mutate_at(vars(term), funs(str_replace(., "^.", ""))) %>%
      mutate_at(
        vars(term),
        funs(str_replace_all(., "^[0]+", "") %>% as.integer())
      ) %>%
      arrange(desc(estimate)) %>%
      select(id = term, rapm = estimate)

    estimates
  }


.fit_rapm_model_byside <-
  function(side = c("o", "d"),
           season,
           path_data_wide_side_format,
           ...,
           # skip = .SKIP,
           verbose = .VERBOSE,
           export = .EXPORT,
           optimize = .OPTIMIZE,
           seed = .SEED,
           lambda = .LAMBDA,
           path_data_final_side_format) {
    side <- match.arg(side)

    data_wide <-
      .import_data_from_path_format(
        path_format = path_data_wide_side_format,
        season = season,
        verbose = verbose,
        ...
      )

    fmla <- formula(pts ~ .)

    x_glmnet <-
      data_wide %>%
      .get_x_glmnet(fmla = fmla)
    y_glmnet <-
      data_wide %>%
      .get_y_glmnet()

    if(optimize) {
      lambda <-
        .get_lambda_optm(
          seed = seed,
          side = side,
          x = x_glmnet,
          y = y_glmnet
        )
      msg <- sprintf("Found %f to be the optimal `lambda` for %srapm.", lambda, side)
    } else {
      msg <- sprintf("Using default `lambda = %f` for %srapm.", lambda, side)
    }
    display_info(msg, verbose = verbose)

    estimates <-
      .get_estimates(
        x = x_glmnet,
        y = y_glmnet,
        lambda = lambda
      )

    .export_data_from_path_format(
      data = estimates,
      path_format = path_data_final_side_format,
      season = season,
      verbose = verbose,
      export = export,
      ...
    )


    estimates %>% arrange(desc(rapm))
  }

.join_estimates_with_players_summary <-
  function(data,
           season,
           path_players_summary_format,
           ...,
           verbose = .VERBOSE) {
    players_summary <-
      .import_data_from_path_format(
        path_format = path_players_summary_format,
        season = season,
        verbose = verbose,
        ...
      )

    # TODO: Make a function for this?
    .SUFFIX_COLS_STATS_ORDER1 <- c("o", "d")
    .SUFFIX_COLS_STATS_ORDER2 <- c(.SUFFIX_COLS_STATS_ORDER1, "")
    .COLS_ESIMATES_PRETTY_ORDER <-
      c("name",
        "id",
        paste0(.SUFFIX_COLS_STATS_ORDER2, "rapm"),
        paste0(.SUFFIX_COLS_STATS_ORDER2, "rapm_rnk")
      )

    data %>%
      mutate_at(vars(matches("rapm$")), funs(rnk = row_number(desc(.)))) %>%
      left_join(players_summary, by = "id") %>%
      select(one_of(.COLS_ESIMATES_PRETTY_ORDER))
  }

fit_rapm_models <-
  function(season,
           path_data_wide_o_format,
           path_data_wide_d_format,
           path_players_summary_format,
           ...,
           skip = .SKIP,
           verbose = .VERBOSE,
           export = .EXPORT,
           optimize_o = .OPTIMIZE,
           optimize_d = .OPTIMIZE,
           seed_o = .SEED,
           seed_d = .SEED,
           lambda_o = .LAMBDA,
           lambda_d = .LAMBDA,
           path_data_final_o_format,
           path_data_final_d_format,
           path_data_final_format) {

    if(skip) {
      return(invisible(NULL))
    }

    .fit_rapm_model_byside_partially <-
      purrr::partial(
        .fit_rapm_model_byside,
        season = season,
        verbose = verbose,
        export = export,
        ...
      )

    estimates_o <-
      .fit_rapm_model_byside_partially(
        side = "o",
        path_data_wide_side_format = path_data_wide_o_format,
        optimize = optimize_o,
        seed = seed_o,
        lambda = lambda_o,
        path_data_final_side_format = path_data_final_o_format
      )
    estimates_d <-
      .fit_rapm_model_byside_partially(
        side = "d",
        path_data_wide_side_format = path_data_wide_d_format,
        optimize = optimize_d,
        seed = seed_d,
        lambda = lambda_d,
        path_data_final_side_format = path_data_final_d_format
      )

    estimates <-
      bind_rows(
        estimates_o %>% mutate(prefix = "o"),
        estimates_d %>% mutate(prefix = "d")
      ) %>%
      spread(prefix, rapm) %>%
      rename_at(vars(o, d), funs(paste0(., "rapm"))) %>%
      mutate(rapm = orapm + drapm) %>%
      arrange(desc(rapm))


    .join_estimates_with_players_summary_partially <-
      purrr::partial(
        .join_estimates_with_players_summary,
        data = estimates,
        path_players_summary_format = path_players_summary_format,
        season = season,
        verbose = verbose,
        ...
      )

    .join_estimates_with_players_summary_possibly <-
      purrr::possibly(
        .join_estimates_with_players_summary_partially,
        otherwise = NULL
      )

    estimates_pretty <-
      .join_estimates_with_players_summary_possibly()

    if(!is.null(estimates_pretty)) {
      estimates <- estimates_pretty
      display_info(
        "Successfully joined `players_summary` with `estimates`.",
        verbose = verbose
      )
    } else {
      display_warning(
        "Could not join `estimates` with `players_summary`.",
        verbose = verbose
      )
    }

    path_data_clean <-
      .export_data_from_path_format(
        data = estimates,
        path_format = path_data_final_format,
        season = season,
        verbose = verbose,
        export = export,
        ...
      )
    estimates
  }

do_fit_rapm_models <-
  purrr::partial(
    fit_rapm_models,
    season = args$season,
    path_data_wide_o_format = args$path_data_wide_o_format,
    optimize_o = args$optimize_o,
    seed_o = args$seed_o,
    lambda_o = args$lambda_o,
    path_data_wide_d_format = args$path_data_wide_d_format,
    optimize_d = args$optimize_d,
    seed_d = args$seed_d,
    lambda_d = args$lambda_d,
    path_players_summary_format = args$path_players_summary_format,
    path_data_final_o_format = args$path_data_final_o_format,
    path_data_final_d_format = args$path_data_final_d_format,
    path_data_final_format = args$path_data_final_format,
    skip = args$skip_final,
    verbose = args$verbose,
    export = args$export_final
  )
