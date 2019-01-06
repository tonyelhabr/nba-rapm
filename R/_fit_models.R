
# # Investigating some stuff with `{glmnet}`.
# x <- .get_x_glmnet(data = mtcars, fmla = formula("mpg ~ ."))
# y <- .get_y_glmnet(data = mtcars, "mpg")
# glmnet::glmnet(x, y) %>% broom::tidy()
#
# fit_cv <- glmnet::cv.glmnet(x, y)
# fit_cv$lambda.1se
# fit_cv$lambda.min # 0.6647582
# fit <- glmnet::glmnet(x, y, lambda = 0.6647582)
# fit %>% broom::tidy()

.Y_RAPM <- "pp100poss"

# Note that intercept term must be specified by a boolean in `glmnet::glmnet()`,
# so it does not matter if `+ 0` or `+ 1` is included in the formula.
# .FMLA_RAPM <- formula(glue::glue("{.Y_RAPM} ~ . - pts - n_poss"))
# .FMLA_APM <- .FMLA_RAPM

.VARS_X_EXCLUDE <- c("pts", "n_poss", "pk", "id_game", "half", "period")
# .RGX_X_INCLUDE <- "^[od][0-9]+"

..paste_fmla_vars <-
  function(x, include = TRUE, sep = "") {
    backtick <- "`"
    sign <- ifelse(include, "+", "-")
    collapse <- paste0(backtick, " ", sign, " ", backtick)
    paste0(backtick, paste(x, collapse = collapse, sep = sep), backtick)
  }

..get_from_rgx <-
  function(data, rgx = NULL) {
    if(!is.null(rgx)) {
      x <- data %>% names() %>% str_subset(rgx)
    }
    unique(x)
  }
# ..combine_x <-
#   function(data, x = NULL, x_rgx = NULL) {
#     x_rgx <- ..get_from_rgx(data = data, rgx = x_rgx)
#     x <- purrr::compact(list(x, x_rgx))
#     x <- unique(x)
#     stopifnot(all(x %in% names(data)))
#     x
#   }

.create_fmla <-
  function(data,
           y,
           x = NULL,
           x_include = NULL,
           x_exclude = NULL,
           rgx_x_include = NULL,
           rgx_x_exclude = NULL,
           intercept = TRUE) {
    stopifnot(is.character(y), length(y) == 1L, any(names(data) == y))

    if (is.null(x)) {
      if (!is.null(x_include) | !is.null(rgx_x_include)) {
        if(is.null(x_include)) {
          x_include <- ..get_from_rgx(data = data, rgx = rgx_x_include)
        }
        # stopifnot(all(x_include %in% names(data)))
        x_include <- intersect(names(data), x_include)
        vars_x <- ..paste_fmla_vars(x = x_include, include = TRUE)
      } else {
        vars_x <- "."
      }
      if (!is.null(x_exclude) | !is.null(rgx_x_exclude)) {
        if(is.null(x_exclude)) {
          x_exclude <- ..get_from_rgx(data = data, rgx = rgx_x_exclude)
        }
        # stopifnot(all(x_exclude %in% names(data)))
        x_exclude <- intersect(names(data), x_exclude)
        vars_x_exclude <- ..paste_fmla_vars(x = x_exclude, include = FALSE)
        vars_x <- paste0(vars_x, " - ", vars_x_exclude)
      }
    } else {
      stopifnot(is.character(x))
      vars_x <- x
    }
    if (!intercept) {
      suffix <- " + 0"
    } else {
      suffix <- " + 0"
    }
    vars_x <- paste0(vars_x, suffix)
    fmla_chr <- paste0(y, " ~ ", vars_x)
    fmla <- as.formula(fmla_chr)
    fmla
  }

.create_fmla_rapm <-
  function(data, y = .Y_RAPM, x_exclude = .VARS_X_EXCLUDE) {
    .create_fmla(
      data = data,
      y = y,
      x_exclude = x_exclude
    )
  }

# fmla <- .create_fmla(data = mtcars, y = "mpg", rgx_x_include = "c", x_exclude = NULL)

.get_x_glmnet <-
  function(data, ...) {
    # fmla %>% model.matrix(poss)
    fmla <- .create_fmla_rapm(data = data)
    fmla %>% Matrix::sparse.model.matrix(data)
  }
.get_y_glmnet <-
  function(data, y = .Y_RAPM, ...) {
    data %>%
      # mutate(pp100poss = pts / n_poss) %>%
      pull(!!sym(y))
  }

# NOTE: Need to make sure the number of terms is not large!
.visualize_glmnet <-
  function(...,
           data,
           path_viz_rapm_model_side = config$path_viz_rapm_model_side) {
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
        path = path_viz_rapm_model_side
      )
    viz
    # viz
  }

.visualize_glmnet_cv <-
  function(...,
           data,
           path_viz_rapm_model_cv_side = config$path_viz_rapm_model_cv_side) {
    suppressWarnings(suppressPackageStartupMessages(library("ggfortify")))
    viz <-
      data %>%
      autoplot() +
      teplot::theme_te()
    path_export <-
      .export_data_from_path(
        ...,
        data = viz,
        path = path_viz_rapm_model_cv_side
      )
    viz
    viz
  }

# TODO: `slice()` specific players.
.filter_glmnet_terms_to_visualize <-
  function(fit) {
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
    terms_filt
  }

.get_lambda_glmnet <-
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
        # standardize = TRUE, # default
        x = x,
        y = y,
        alpha = 0
      )

    # Simiolar to calling`plot(fit)`.
    viz_glmnet_cv <-
      .visualize_glmnet_cv(
        ...,
        data = fit
      )

    terms_filt <-
      fit %>%
      .filter_glmnet_terms_to_visualize()

    # Similar to calling `plot(fit$glmnet.fit)`.
    viz_glmnet <-
      .visualize_glmnet(
        ...,
        data = terms_filt
      )
    .display_info(
      glue::glue(
        "Found {scales::comma(fit$lambda.min)} to be the optimal ",
        "{usethis::ui_field('lambda')} for minimizing MSE. ",
        "(Also, found {scales::comma(fit$lambda.1se)} as the ",
        "{usethis::ui_field('lambda')} at 1 standard error.)"
      ),
      ...
    )

    # ~~TODO: Use fit$lambda.fit %>% tidy() %>% filter(lambda == fit$lambda.1se) instead?
    # Update: I believe this is actually the same thing.
    # TODO: Plot standard errors?
    # Update: Standard errors are actually pretty difficult to calculate for `{glmnet}`.
    path_export <-
      .export_data_from_path(
        ...,
        data = tibble(lambda_min = fit$lambda.min, lambda_1se = fit$lambda.1se),
        path = path_lambda_side
      )
    fit$lambda.min
  }

.fit_rapm_model <-
  function(...,
           x,
           y,
           intercept = .INTERCEPT,
           lambda = .LAMBDA,
           seed = .SEED,
           path_rapm_model_side = config$path_rapm_model_side) {
    set.seed(seed)
    fit <-
      glmnet::glmnet(
        intercept = intercept,
        # standardize = FALSE,
        x = x,
        y = y,
        lambda = lambda,
        alpha = 0,
      )
    path_export <-
      .export_data_from_path(
        ...,
        data = fit,
        path = path_rapm_model_side
      )
    fit
  }

.fit_rapm_model_side <-
  function(...,
           # "Catch `lambda` here to pass to `.get_lambda_glmnet()` (which otherwise
           # may confict with the `lambda` passed to `.fit_rapm_model()`
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
      .get_lambda_glmnet(
        ...,
        lambda = lambda,
        x = x_glmnet,
        y = y_glmnet
      )

    fit <-
      .fit_rapm_model(
        ...,
        x = x_glmnet,
        y = y_glmnet,
        lambda = lambda
      )
    fit
  }

.fit_apm_model_side <-
  function(...,
           path_poss_wide_side = config$path_poss_wide_side,
           path_apm_model_side = config$path_apm_model_side) {

    poss <-
      .import_data_from_path(
        ...,
        path = path_poss_wide_side
      )

    fmla <- .create_fmla_rapm(data = poss)
    fit <- glm(fmla, data = poss)
    path_export <-
      .export_data_from_path(
        ...,
        data = fit,
        path = path_apm_model_side
      )
    fit
  }

.fit_models <-
  function(...,
           # Unfortunately, these cant be abstracted away with `...`
           # because they have _[o|d]` suffixes.
           lambda_o = .LAMBDA,
           lambda_d = .LAMBDA) {

    .display_auto_step(
      glue::glue("Fitting models."),
      ...
    )

    fit_rapm_o <-
      .fit_rapm_model_side(
        ...,
        side = "o",
        lambda = lambda_o
      )
    fit_rapm_d <-
      .fit_rapm_model_side(
        ...,
        side = "d",
        lambda = lambda_d
      )
    fit_rapm_both <-
      .fit_rapm_model_side(
      ...,
      side = "both",
      lambda = (lambda_o + lambda_d) / 2,
    )

    if(FALSE) {
      fit_apm_o <-
        .fit_apm_model_side(
          ...,
          side = "o"
        )
      fit_apm_o <-
        .fit_apm_model_side(
          ...,
          side = "d"
        )
      # fit_apm_both <-
      #   .fit_apm_model_side(
      #     ...,
      #     side = "both"
      #   )
    }
    invisible()
  }

auto_fit_models <-
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
    .fit_models(
      ...,
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

# # This was used once "ad-hoc".
# .fit_models_manually <-
#   function(..., skip = FALSE, season = .SEASONS) {
#     purrr::map(season, ~.extract_glment_coefs(..., skip = skip, season = .x))
#   }


