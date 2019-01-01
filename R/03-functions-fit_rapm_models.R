
.Y <- "pp100poss"

.FMLA <- formula(glue::glue("{.Y} ~ . - pts - n"))
.get_x_glmnet <-
  function(possession_data, fmla = .FMLA) {
    fmla %>% model.matrix(possession_data)
  }
.get_y_glmnet <-
  function(possession_data, y = .Y) {
    possession_data %>% pull(!!sym(y))
  }

# NOTE: Need to make sure the number of terms is not large!
.visualize_glmnet <-
  function(...,
           data,
           path_viz_rapm_fit_side = config$path_viz_rapm_fit_side) {
    # suppressWarnings(suppressPackageStartupMessages(library("ggfortify")))
    # # viz <- data %>% autoplot()
    viz <-
      data %>%
      ggplot(aes(x = lambda, y = estimate, color = term)) +
      geom_line() +
      scale_x_log10() +
      teplot::theme_te()
    path_export <-
      .export_data_from_path(
        ...,
        data = viz,
        path = path_viz_rapm_fit_side
      )
    # invisible(viz)
    viz
  }

.visualize_glmnet_cv <-
  function(...,
           data,
           path_viz_rapm_fit_cv_side = config$path_viz_rapm_fit_cv_side) {
    suppressWarnings(suppressPackageStartupMessages(library("ggfortify")))
    viz <-
      data %>%
      autoplot() +
      teplot::theme_te()
    path_export <-
      .export_data_from_path(
        ...,
        data = viz,
        path = path_viz_rapm_fit_cv_side
      )
    # invisible(viz)
    viz
  }

.get_lambda <-
  function(...,
           x,
           y,
           intercept = .INTERCEPT,
           optimize = .OPTIMIZE,
           lambda = .LAMBDA,
           seed = .SEED,
           path_lambda_side = config$path_lambda_side) {

    if(!optimize) {
      .display_info(
        glue::glue(
          "Using the specified value {scales::comma(lambda)} for",
          " {usethis::ui_field('lambda')}."
        ),
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
        intercept = intercept,
        x = x,
        y = y,
        alpha = 0
      )

    # plot(fit)
    viz_glmnet_cv <-
      .visualize_glmnet_cv(
        ...,
        data = fit
      )

    terms <-
      fit$glmnet.fit %>%
      broom::tidy()
    terms_optm_arr <-
      terms %>%
      filter(lambda == fit$lambda.min) %>%
      arrange(desc(abs(estimate)))
    terms_filt <-
      terms %>%
      inner_join(
        terms_optm_arr %>%
          select(term) %>%
          slice(1:10),
        by = "term"
      )
    # plot(fit$glmnet.fit)
    viz_glmnet <-
      .visualize_glmnet(
        ...,
        data = terms_filt
      )
    .display_info(
      glue::glue(
        "Found {scales::comma(fit$lambda.min)} ",
        "to be the optimal {usethis::ui_field('lambda')}."
      ),
      ...
    )

    # TODO: Use fit$lambda.fit %>% tidy() %>% filter(lambda == fit$lambda.1se) instead?
    # (UPDATE: I believe this is actually the same thing.)
    # TODO: Plot standard errors?
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
           intercept = .INTERCEPT,
           lambda = .LAMBDA,
           seed = .SEED,
           path_rapm_fit_side = config$path_rapm_fit_side) {
    set.seed(seed)
    fit <-
      glmnet::glmnet(
        intercept = intercept,
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

    .display_progress(
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
           intercept = config$intercept,
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
      intercept = intercept,
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
