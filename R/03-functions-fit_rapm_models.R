
.FMLA <- formula("pts ~ .")
.get_x_glmnet <-
  function(possession_data, fmla = .FMLA) {
    fmla %>% model.matrix(possession_data)
  }
.get_y_glmnet <-
  function(possession_data) {
    possession_data %>% pull(pts)
  }

.UNITS <- "in"
.HEIGHT <- 5
.WIDTH <- 7
.visualize_glmnet <-
  function(...,
           data,
           side = c("o", "d"),
           path,
           units = .UNITS,
           height = .HEIGHT,
           width = .WIDTH) {
    require("ggfortify")
    viz <-
      ggfortify::autoplot(data)
    path_export <-
      .export_data_from_path(
        ...,
        data = viz,
        path = path,
        units = units,
        height = height,
        width = width
      )
    invisible(viz)
  }

.visualize_glmnet_cv <-
  function(...,
           data,
           side = c("o", "d"),
           path = .get_config_name(config$path_fit_cv, side = side),
           units = .UNITS,
           height = .HEIGHT,
           width = .WIDTH) {
    .visualize_glmnet(
      ...,
      data = data,
      path = path,
      units = units,
      height = height,
      width = width
    )
  }

.get_lambda_optm <-
  function(...,
           x,
           y,
           seed = .SEED,
           path_rapm_fit_cv_side,
           path_rapm_fit_cv_error_side,
           path_rapm_fit_cv_coefs_side) {
    set.seed(seed)
    fit <-
      glmnet::cv.glmnet(
        parallel = TRUE,
        keep = TRUE,
        x = x,
        y = y,
        alpha = 0
      )
    browser()

    # plot(fit)
    viz_fit_error <-
      .visualize_glmnet_cv(
        ...,
        data = fit,
        path = path_rapm_fit_cv_error_side
      )
    # plot(fit$glmnet.fit)
    viz_fit_coefs <-
      .visualize_glmnet(
        ...,
        data = fit$glmnet.fit,
        path = path_rapm_fit_cv_coefs_side
      )
    path_export <-
      .export_data_from_path(
        ...,
        data = fit$lambda.min %>% tibble(lambda_min = .),
        path = path_rapm_fit_cv_side
      )
    invisible(fit$lambda.min)
  }

.fit_rapm_model_side <-
  function(...,
           x,
           y,
           lambda,
           seed = .SEED,
           side = c("o", "d"),
           path_rapm_fit_side) {
    set.seed(seed)
    fit <-
      glmnet::glmnet(
        intercept = TRUE,
        x = x,
        y = y,
        alpha = 0,
        lambda = lambda
      )
    viz_fit_coefs <-
      .visualize_glmnet(
        ...,
        data = fit,
        path = path_rapm_fit_side
      )
    path_export <-
      .export_data_from_path(
        ...,
        data = fit,
        path = path_rapm_fit_side
      )
    invisible(fit)
  }

.fit_rapm_model_byside <-
  function(...,
           # side = c("o", "d"),
           path_possession_data_side,
           path_rapm_fit_cv_side,
           path_rapm_fit_side,
           optimize = .OPTIMIZE,
           lambda = .LAMBDA) {
    possession_data <-
      .import_data_from_path(
        ...,
        path = path_possession_data_side
      )
    x_glmnet <-
      possession_data %>%
      .get_x_glmnet()
    y_glmnet <-
      possession_data %>%
      .get_y_glmnet()

    if(optimize) {
      lambda <-
        .get_lambda_optm(
          ...,
          x = x_glmnet,
          y = y_glmnet,
          path_rapm_fit_cv_side
        )
      msg <- glue::glue("Found {usethis::ui_value(lambda)} to be the optimal {usethis::ui_field('lambda')}.")
    } else {
      msg <- glue::glue("Using default {usethis::ui_field('lambda')} = {usethis::ui_value(lambda)}.")
    }
    .display_info(msg, ...)

    fit <-
      .fit_rapm_model_side(
        ...,
        x = x_glmnet,
        y = y_glmnet,
        lambda = lambda,
        path_rapm_fit_side = path_rapm_fit_side
      )
    fit
  }

fit_rapm_models <-
  function(...,
           # Unfortunately, these cant be abstracted away with `...`
           # because they have _[o|d]` suffixes.
           path_possession_data_o = config$path_possession_data_o,
           path_possession_data_d = config$path_possession_data_d,
           path_rapm_fit_cv_o = config$path_rapm_fit_cv_o,
           path_rapm_fit_cv_d = config$path_rapm_fit_cv_d,
           path_rapm_fit_o = config$path_rapm_fit_o,
           path_rapm_fit_d = config$path_rapm_fit_d,
           optimize_o = .OPTIMIZE,
           optimize_d = .OPTIMIZE,
           seed_o = .SEED,
           seed_d = .SEED,
           lambda_o = .LAMBDA,
           lambda_d = .LAMBDA) {

    will_skip <-
      .try_skip(
        ...,
        path_reqs =
          c(
            path_possession_data_o,
            path_possession_data_d
          ),
        path_deps =
          c(
            path_rapm_fit_o,
            path_rapm_fit_d
          )
      )

    if(will_skip) {
      return(invisible(NULL))
    }

    .fit_rapm_model_byside_partially <-
      purrr::partial(
        .fit_rapm_model_byside,
        ...
      )
    fit_o <-
      # .fit_rapm_model_byside_partially(
      .fit_rapm_model_byside(
        ...,
        path_possession_data_side = path_possession_data_o,
        optimize = optimize_o,
        seed = seed_o,
        lambda = lambda_o,
        path_rapm_fit_cv_side = path_fit_cv_o,
        path_rapm_fit_side = path_rapm_fit_o
      )
    fit_d <-
      .fit_rapm_model_byside_partially(
        path_possession_data_side = path_possession_data_d,
        optimize = optimize_d,
        seed = seed_d,
        lambda = lambda_d,
        path_rapm_fit_cv_side = path_fit_cv_d,
        path_rapm_fit_side = path_rapm_fit_d
      )
    invisible(list(fit_o = fit_o, fit_d = fit_d))
  }

# fit_rapm_models_auto <-
#   purrr::partial(
#     fit_rapm_models,
#     season = config$season,
#     optimize_o = config$optimize_o,
#     optimize_d = config$optimize_d,
#     seed_o = config$seed_o,
#     seed_d = config$seed_d,
#     lambda_o = config$lambda_o,
#     lambda_d = config$lambda_d,
#     skip = config$skip,
#     verbose = config$verbose,
#     export = config$export,
#     backup = config$backup,
#     clean = config$clean,
#     n_keep = config$n_keep
#   )

fit_rapm_models_auto <-
  function(...,
           season = config$season,
           optimize_o = config$optimize_o,
           optimize_d = config$optimize_d,
           seed_o = config$seed_o,
           seed_d = config$seed_d,
           lambda_o = config$lambda_o,
           lambda_d = config$lambda_d,
           skip = config$skip,
           verbose = config$verbose,
           export = config$export,
           backup = config$backup,
           clean = config$clean,
           n_keep = config$n_keep) {
    fit_rapm_models(
      # ...,
      season = season,
      optimize_o = optimize_o,
      optimize_d = optimize_d,
      seed_o = seed_o,
      seed_d = seed_d,
      lambda_o = lambda_o,
      lambda_d = lambda_d,
      skip = skip,
      verbose = verbose,
      export = export,
      backup = backup,
      clean = clean,
      n_keep = n_keep
    )
  }
