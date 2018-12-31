
.FMLA <- formula("pts ~ .")
.get_x_glmnet <-
  function(possession_data, fmla = .FMLA) {
    fmla %>% model.matrix(possession_data)
  }
.get_y_glmnet <-
  function(possession_data) {
    possession_data %>% pull(pts)
  }

.visualize_glmnet <-
  function(...,
           data,
           path_viz_rapm_fit_side = config$path_viz_rapm_fit_side) {
    suppressWarnings(suppressPackageStartupMessages(requireNamespace("ggfortify")))
    viz <-
      autoplot(data)
    path_export <-
      .export_data_from_path(
        ...,
        data = viz,
        path = path_viz_rapm_fit_side
      )
    invisible(viz)
  }

.visualize_glmnet_cv <-
  function(...,
           data,
           path_viz_rapm_fit_cv_side = config$path_viz_rapm_fit_cv_side) {
    suppressWarnings(suppressPackageStartupMessages(requireNamespace("ggfortify")))
    viz <-
      autoplot(data)
    path_export <-
      .export_data_from_path(
        ...,
        data = viz,
        path = path_viz_rapm_fit_cv_side
      )
    invisible(viz)
  }

.get_lambda <-
  function(...,
           x,
           y,
           optimize = .OPTIMIZE,
           lambda = .LAMBDA,
           seed = .SEED,
           path_lambda_side = config$path_lambda_side) {
    # browser()
    if(!optimize) {
      .display_info(
        glue::glue(
          "Using the specified value {usethis::ui_value(lambda)} for {usethis::ui_field('lambda')}."),
        ...
      )
      path_export <-
        .export_data_from_path(
          ...,
          data = lambda %>% tibble(lambda = .),
          path = path_lambda_side
        )
      return(lambda)
    }
    set.seed(seed)
    fit <-
      glmnet::cv.glmnet(
        parallel = TRUE,
        keep = TRUE,
        x = x,
        y = y,
        alpha = 0
      )

    # plot(fit)
    viz_glmnet <-
      .visualize_glmnet_cv(
        ...,
        data = fit
      )
    # # plot(fit$glmnet.fit)
    viz_glmnet <-
      .visualize_glmnet(
        ...,
        data = fit$glmnet.fit
      )
    .display_info(
      glue::glue(
        "Found {usethis::ui_value(fit$lambda.min)} ",
        "to be the optimal {usethis::ui_field('lambda')}."
      ),
      ...
    )

    path_export <-
      .export_data_from_path(
        ...,
        data = fit$lambda.min %>% tibble(lambda = .),
        path = path_lambda_side
      )
    invisible(fit$lambda.min)
  }

.fit_rapm_model_side <-
  function(...,
           x,
           y,
           lambda = .LAMBDA,
           seed = .SEED,
           path_rapm_fit_side = config$path_rapm_fit_side) {
    set.seed(seed)
    fit <-
      glmnet::glmnet(
        intercept = TRUE,
        x = x,
        y = y,
        lambda = lambda,
        alpha = 0,
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
           lambda, # "Catch the `lambda` here.
           path_possession_data_side = config$path_possession_data_side) {

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

    lambda <-
      .get_lambda(
        ...,
        lambda = lambda,
        x = x_glmnet,
        y = y_glmnet
      )

    fit <-
      .fit_rapm_model_side(
        ...,
        x = x_glmnet,
        y = y_glmnet,
        lambda = lambda
      )
    fit
  }

fit_rapm_models <-
  function(...,
           path_possession_data_side = config$path_possession_data_side,
           path_rapm_fit_side = config$path_rapm_fit_side,
           # optimize = .OPTIMIZE,
           # seed = .SEED,
           # Unfortunately, these cant be abstracted away with `...`
           # because they have _[o|d]` suffixes.
           lambda_o = .LAMBDA,
           lambda_d = .LAMBDA) {

    will_skip <-
      .try_skip(
        ...,
        path_reqs =
          c(
            .get_path_from(..., path = path_possession_data_side, side = "o"),
            .get_path_from(..., path = path_possession_data_side, side = "d")
          ),
        path_deps =
          c(
            .get_path_from(..., path = path_rapm_fit_side, side = "o"),
            .get_path_from(..., path = path_rapm_fit_side, side = "d")
          )
      )

    if(will_skip) {
      return(invisible(NULL))
    }

    .display_info(
      glue::glue("Step 3: Fitting models."),
      ...
    )

    fit_o <-
      .fit_rapm_model_byside(
        ...,
        side = "o",
        lambda = lambda_o
      )
    fit_d <-
      .fit_rapm_model_byside(
        ...,
        side = "d",
        lambda = lambda_d
      )
    invisible(list(fit_o = fit_o, fit_d = fit_d))
  }

fit_rapm_models_auto <-
  function(...,
           season = config$season,
           optimize = config$optimize,
           seed = config$seed,
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
      optimize = optimize,
      seed = seed,
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
