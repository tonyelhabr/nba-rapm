
.FMLA <- formula("pts ~ .")
.get_x_glmnet <-
  function(possession_data, fmla = .FMLA) {
    fmla %>% model.matrix(possession_data)
  }
.get_y_glmnet <-
  function(possession_data) {
    possession_data %>% pull(pts)
  }

.get_lambda_optm <-
  function(x, y, ..., seed = .SEED) {
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

.fit_rapm_model_side <-
  function(...,
           x,
           y,
           lambda,
           path_rapm_fit_side) {
    fit <-
      glmnet::glmnet(
        intercept = TRUE,
        x = x,
        y = y,
        alpha = 0,
        lambda = lambda
      )
    .export_data_from_path(
      ...,
      data = fit,
      path = path_rapm_fit_side
    )
    invisible(fit)
  }

.fit_rapm_model_byside <-
  function(...,
           path_possession_data_side,
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
          y = y_glmnet
        )
      msg <- sprintf("Found %f to be the optimal `lambda`.", lambda)
    } else {
      msg <- sprintf("Using default `lambda = %f`.", lambda)
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
           path_possession_data_o = config$path_possession_data_o,
           path_possession_data_d = config$path_possession_data_d,
           path_rapm_fit_o = config$path_rapm_fit_o,
           path_rapm_fit_d = config$path_rapm_fit_d,
           # Unfortunately, these cant be abstracted away with `...`
           # because they have _[o|d]` suffixes.
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
      .fit_rapm_model_byside_partially(
        path_possession_data_side = path_possession_data_o,
        optimize = optimize_o,
        seed = seed_o,
        lambda = lambda_o,
        path_rapm_fit_side = path_rapm_fit_o
      )
    fit_d <-
      .fit_rapm_model_byside_partially(
        path_possession_data_side = path_possession_data_d,
        optimize = optimize_d,
        seed = seed_d,
        lambda = lambda_d,
        path_rapm_fit_side = path_rapm_fit_d
      )
    invisible(list(fit_o = fit_o, fit_d = fit_d))
  }

auto_fit_rapm_models <-
  purrr::partial(
    fit_rapm_models,
    season = config$season,
    optimize_o = config$optimize_o,
    optimize_d = config$optimize_d,
    seed_o = config$seed_o,
    seed_d = config$seed_d,
    lambda_o = config$lambda_o,
    lambda_d = config$lambda_d,
    skip = config$skip_fit,
    verbose = config$verbose,
    export = config$export,
    backup = config$backup,
    clean = config$clean,
    n_keep = config$n_keep
  )
