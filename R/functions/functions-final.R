

.get_x_glmnet <-
  function(data_wide, ...) {
    fmla %>%
      model.matrix(data_wide)
  }

.get_y_glmnet <-
  function(data_wide, ...) {
    data_wide %>%
      pull(pts)
  }

.get_x_glmnet <-
  function(data_wide, ...) {
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


fit_rapm_model_byside <-
  function(season,
           path_data_wide_format,
           side = c("o", "d"),
           ...,
           # skip = .SKIP,
           verbose = .VERBOSE,
           export = .EXPORT,
           optimize = .OPTIMIZE,
           seed = .SEED,
           lambda = .LAMBDA) {

    data_wide <-
      .import_data_from_path_format(
        path_format = path_data_wide_format,
        season = season,
        verbose = verbose
      )
    fmla <- formula(pts ~ .)
    x_glmnet <-
      data_wide %>%
      .get_x_glmnet()
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
    }

    if(verbose) {
      if(optimze) {
        msg <- sprintf("Found %f to be the optimal `lambda` for %srapm.", lambda, side)
      } else {
        msg <- sprintf("Using default `lambda = %f` for %srapm.", lambda, side)
      }
      display_msg(msg)
    }

    estimates <-
      .get_estimates(
        x = x_glmnet,
        y = y_glmnet,
        lambda = lambda
      )

    estimates %>% arrange(desc(rapm))
  }


# do_fit_rapm_model_o <-
#   purrr::partial(
#     fit_rapm_model_byside,
#     season = args$season,
#     path_data_wide_format = args$path_data_wide_o_format,
#     side = "o",
#     optimize = args$optimize_o,
#     seed = args$seed_o,
#     lambda = args$lambda_o
#   )
#
# do_fit_rapm_model_d <-
#   purrr::partial(
#     fit_rapm_model_byside,
#     season = args$season,
#     path_data_wide_format = args$path_data_wide_d_format,
#     side = "d",
#     optimize = args$optimize_d,
#     seed = args$seed_d,
#     lambda = args$lambda_d
#   )

.join_players_summary <-
  function(season,
           path_players_summary_format,
           ...,
           verbose = .VERBOSE) {
    players_summary <-
      .import_data_from_path_format(
        path_format = path_players_summary_format,
        season = season,
        verbose = verbose
      )

    estimates <-
      estimates %>%
      mutate_at(vars(matches("rapm")), funs(rnk = row_number(desc(.)))) %>%
      left_join(players_summary, by = "id")

    display_msg(
      "Successfully joined `players_summary` with `estimates`.", verbose = verbose
    )
    estimates
  }

.join_players_summary_possibly <-
  purrr::possibly(
    .join_players_summary,
    otherwise = NULL
  )

do_fit_rapm_models <-
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
           path_data_final_format) {

    if(skip) {
      return(invisible(NULL))
    }

    estimates_o <-
      fit_rapm_model_byside(
        season = season,
        path_data_wide_format = path_data_wide_o_format,
        side = "o",
        optimize = optimize_o,
        seed = seed_o,
        lambda = lambda_o
      )
    estimates_d <-
      fit_rapm_model_byside(
        season = season,
        path_data_wide_format = path_data_wide_d_format,
        side = "d",
        optimize = optimize_d,
        seed = seed_d,
        lambda = lambda_d
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

    estimates_pretty <-
      .join_players_summary_possibly(
        data = estimates,
        path_players_summary_format = path_players_summary_format,
        season = season,
        verbose = verbose
      )

    if(!is.null(estimates_pretty)) {
      estimates <- estimates_pretty
    }


    if (export) {
      path_data_clean <-
        .export_data_from_path_format(
          data = estimates,
          path_format = path_data_final_format,
          season = season,
          verbose = verbose
        )
    }
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
    lambda_d = args$lambda_d
  )
