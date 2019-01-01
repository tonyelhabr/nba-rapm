
.Y <- "pp100poss"

# Note that intercept term must be specified by a boolean in `glmnet::glmnet()`,
# so it does not matter if `+ 0` or `+ 1` is included in the formula.
.FMLA <- formula(glue::glue("{.Y} ~ . - pts - n_poss"))

.SCALE <- TRUE
.get_x_glmnet <-
  function(data, fmla = .FMLA, ..., scale = .SCALE) {
    browser()
    # fmla %>% model.matrix(poss)
    if(scale) {
      data <-
        data %>%
        gather(xid_player, .n_poss, matches("^[o|d]")) %>%
        spread(xid_player, n_scaled)
    }
    fmla %>% Matrix::sparse.model.matrix(data)
  }
.get_y_glmnet <-
  function(data, y = .Y, ...) {
    data %>%
      # mutate(pp100poss = pts / n_poss) %>%
      pull(!!sym(y))
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
    invisible(viz)
    # viz
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
    invisible(viz)
    viz
  }

.filter_glmnet_terms_to_visualize <-
  function(...,
           fit) {
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
    terms_filt
  }

.get_lambda <-
  function(...,
           x,
           y,
           intercept = .INTERCEPT,
           scale = .SCALE,
           optimize = .OPTIMIZE,
           lambda = .LAMBDA,
           seed = .SEED,
           path_lambda_side = config$path_lambda_side) {

    if(!optimize) {
      .display_info(
        glue::glue(
          "Using the specified value {scales::comma(lambda)} for ",
          "{usethis::ui_field('lambda')}."
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

    terms_filt <-
      fit %>%
      .filter_glmnet_terms_to_visualize()

    # plot(fit$glmnet.fit)
    viz_glmnet <-
      .visualize_glmnet(
        ...,
        data = terms_filt
      )
    .display_info(
      glue::glue(
        "Found {scales::comma(fit$lambda.min)} to be the optimal",
        "{usethis::ui_field('lambda')} for minimizing MSE. ",
        "(Alsoe, found {scales::comma(fit$lambda.1se)} as the ",
        "{usethis::ui_field('lambda')} at 1 standard error.)"
      ),
      ...
    )

    # TODO: Use fit$lambda.fit %>% tidy() %>% filter(lambda == fit$lambda.1se) instead?
    # (UPDATE: I believe this is actually the same thing.)
    # TODO: Plot standard errors?
    path_export <-
      .export_data_from_path(
        ...,
        data = tibble(lambda_min = fit$lambda.min, lambda_1se = fit$lambda.1se),
        path = path_lambda_side
      )
    invisible(fit$lambda.min)
  }

.fit_rapm_model_side <-
  function(...,
           x,
           y,
           intercept = .INTERCEPT,
           # scale = .SCALE,
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
           # "Catch `lambda` here to pass to `.get_lambda()` (which otherwise
           # may confict with the `lambda` passed to `.fit_rapm_model_side()`
           # if passed through `...`.)
           lambda,
           path_poss_wide_side = config$path_poss_wide_side) {

    poss <-
      .import_data_from_path(
        ...,
        path = path_poss_wide_side
      )
    x_glmnet <- .get_x_glmnet(..., data = poss)
    y_glmnet <- .get_y_glmnet(..., data = poss)

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

# TODO!(Use `pk`?)
.fit_rapm_model_bothsides <-
  function(...,
           lambda,
           path_poss_wide_side = config$path_poss_wide_side) {
    poss_o <-
      .import_data_from_path(
        ...,
        side = "o",
        path = path_poss_wide_side
      )
    poss_d <-
      .import_data_from_path(
        ...,
        side = "d",
        path = path_poss_wide_side
      )
    poss <-
      inner_join(
        poss_o,
        poss_d
      )
  }

fit_rapm_models <-
  function(...,
           path_poss_wide_side = config$path_poss_wide_side,
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
            .get_path_from(..., path = path_poss_wide_side, side = "o"),
            .get_path_from(..., path = path_poss_wide_side, side = "d")
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
    # fit_od <- .fit_rapm_model_bothsides(...)
    invisible(list(fit_o = fit_o, fit_d = fit_d))
  }

fit_rapm_models_auto <-
  function(...,
           season = config$season,
           intercept = config$intercept,
           scale = config$scale,
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
      scale = scale,
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
