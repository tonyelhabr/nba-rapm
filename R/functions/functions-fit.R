
.FMLA <- "pts ~ ."
.get_x_glmnet <-
  function(possession_data, fmla = .FMLA, ...) {
    fmla %>%
      model.matrix(possession_data)
  }

.get_y_glmnet <-
  function(possession_data, ...) {
    possession_data %>%
      pull(pts)
  }

.get_x_glmnet <-
  function(possession_data, fmla = .FMLA, ...) {
    fmla %>%
      model.matrix(possession_data)
  }
.get_y_glmnet <-
  function(possession_data, ...) {
    possession_data %>%
      pull(pts)
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
  function(x,
           y,
           lambda,
           path_rapm_fit_side_format,
           season,
           ...) {
    fit <-
      glmnet::glmnet(
        intercept = TRUE,
        x = x,
        y = y,
        alpha = 0,
        lambda = lambda
      )
    .export_data_from_path_format(
      data = fit,
      path_format = path_rapm_fit_side_format,
      season = season,
      ...
    )
    invisible(fit)
  }

.fit_rapm_model_byside <-
  function(path_possession_data_side_format,
           path_rapm_fit_side_format,
           season,
           optimize = .OPTIMIZE,
           lambda = .LAMBDA,
           ...) {

    possession_data <-
      .import_data_from_path_format(
        path_format = path_possession_data_side_format,
        season = season,
        verbose = verbose,
        ...
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
          x = x_glmnet,
          y = y_glmnet,
          ...
        )
      msg <- sprintf("Found %f to be the optimal `lambda`.", lambda)
    } else {
      msg <- sprintf("Using default `lambda = %f`.", lambda)
    }
    .display_info(msg, ...)

    fit <-
      .fit_rapm_model_side(
        x = x_glmnet,
        y = y_glmnet,
        lambda = lambda,
        path_rapm_fit_side_format = path_rapm_fit_side_format,
        season = season,
        ...
      )
    fit
  }

fit_rapm_models <-
  function(path_possession_data_o_format,
           path_possession_data_d_format,
           path_rapm_fit_o_format,
           path_rapm_fit_d_format,
           season,
           # Unfortunately, these can't be abstracted away with `...`
           # because they have _[o|d]` suffixes.
           optimize_o = .OPTIMIZE,
           optimize_d = .OPTIMIZE,
           seed_o = .SEED,
           seed_d = .SEED,
           lambda_o = .LAMBDA,
           lambda_d = .LAMBDA,
           ...) {

    will_skip <-
      .try_skip(
        season = season,
        path_format_reqs =
          c(
            path_possession_data_o_format,
            path_possession_data_d_format
          ),
        path_format_deps =
          c(
            path_rapm_fit_o_format,
            path_rapm_fit_d_format
          ),
        ...
      )

    if(will_skip) {
      return(invisible(NULL))
    }

    .fit_rapm_model_byside_partially <-
      purrr::partial(
        .fit_rapm_model_byside,
        season = season,
        ...
      )

    fit_o <-
      .fit_rapm_model_byside_partially(
        path_possession_data_side_format = path_possession_data_o_format,
        optimize = optimize_o,
        seed = seed_o,
        lambda = lambda_o,
        path_rapm_fit_side_format = path_rapm_fit_o_format
      )
    fit_d <-
      .fit_rapm_model_byside_partially(
        path_possession_data_side_format = path_possession_data_d_format,
        optimize = optimize_d,
        seed = seed_d,
        lambda = lambda_d,
        path_rapm_fit_side_format = path_rapm_fit_d_format
      )
    invisible(list(fit_o = fit_o, fit_d = fit_d))
  }

auto_fit_rapm_models <-
  purrr::partial(
    fit_rapm_models,
    path_possession_data_o_format = args$path_possession_data_o_format,
    path_possession_data_d_format = args$path_possession_data_d_format,
    path_rapm_fit_o_format = args$path_rapm_fit_o_format,
    path_rapm_fit_d_format = args$path_rapm_fit_d_format,
    season = args$season,
    optimize_o = args$optimize_o,
    optimize_d = args$optimize_d,
    seed_o = args$seed_o,
    seed_d = args$seed_d,
    lambda_o = args$lambda_o,
    lambda_d = args$lambda_d,
    skip = args$skip_fit,
    debug = args$debug,
    verbose = args$verbose,
    export = args$export,
    backup = args$backup,
    clean = args$clean,
    n_keep = args$n_keep
  )
