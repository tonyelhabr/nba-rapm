
# x <- .get_x_glmnet(data = mtcars, fmla = formula("mpg ~ ."))
# y <- .get_y_glmnet(data = mtcars, "mpg")
# glmnet::glmnet(x, y) %>% broom::tidy()
#
# fit_cv <- glmnet::cv.glmnet(x, y)
# fit_cv$lambda.1se
# fit_cv$lambda.min # 0.6647582
# fit <- glmnet::glmnet(x, y, lambda = 0.6647582)
# fit %>% broom::tidy()

.Y <- "pp100poss"

# Note that intercept term must be specified by a boolean in `glmnet::glmnet()`,
# so it does not matter if `+ 0` or `+ 1` is included in the formula.
.FMLA <- formula(glue::glue("{.Y} ~ . - pts - n_poss"))

.get_x_glmnet <-
  function(data, fmla = .FMLA, ...) {
    # fmla %>% model.matrix(poss)
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
    invisible(viz)
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
    invisible(viz)
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
        # standardize = TRUE,
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
        "Found {scales::comma(fit$lambda.min)} to be the optimal ",
        "{usethis::ui_field('lambda')} for minimizing MSE. ",
        "(Also, found {scales::comma(fit$lambda.1se)} as the ",
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

# Note that this is a work-around since `broom:::tidy.glmnet()` isn't working(?)
.tidy_glmnet <- function(x, return_zeros = FALSE, ...) {
  # This first line is the major change to `broom:::tidy.glmnet()`.
  # (Comments from the source code are removed as well, and some other logic
  # having to do with inheriting from `"multnet"` is removed).
  # beta <- coef(x)
  beta <- x$beta
  beta_d <-
    broom:::fix_data_frame(
      as.matrix(beta),
      newnames = 1:ncol(beta),
      newcol = "term"
    )
  ret <- tidyr::gather(beta_d, step, estimate, -term)
  ret <- ret %>%
    mutate(
      step = as.numeric(step),
      lambda = x$lambda[step],
      dev.ratio = x$dev.ratio[step]
    )

  if (!return_zeros) {
    ret <- filter(ret, estimate != 0)
  }

  as_tibble(ret)
}

..extract_rapm_coefs <-
  function(..., fit) {
    fit %>%
      # broom::tidy() %>%
      .tidy_glmnet() %>%
      # filter(term == "(Intercept)") %>%
      mutate_at(
        vars(term),
        funs(if_else(. != "(Intercept)", ., "0"))
      ) %>%
      mutate_at(
        vars(term),
        funs(str_replace_all(., "^[od]", "") %>% as.integer())
      ) %>%
      arrange(desc(estimate)) %>%
      select(id_player = term, rapm = estimate) %>%
      filter(!is.na(id_player))
  }

.extract_rapm_coefs_side <-
  function(...,
           path_rapm_model_side = config$path_rapm_model_side,
           path_rapm_coefs_var_side = config$path_rapm_coefs_var_side,
           path_rapm_coefs_side = config$path_rapm_coefs_side) {
    fit <-
      .import_data_from_path(
        ...,
        path = path_rapm_model_side
      )


    # TODO: Do something better here!
    path_export <-
      .export_data_from_path(
        ...,
        data = fit %>% .tidy_glmnet(),
        path = path_rapm_coefs_var_side
      )
    rapm_coefs <-
      ..extract_rapm_coefs(
        ...,
        fit = fit
      )

    .export_data_from_path(
      ...,
      data = rapm_coefs,
      path = path_rapm_coefs_side
    )

    invisible(rapm_coefs)
  }

.join_rapm_coefs_od <-
  function(...,
           rapm_coefs_o = NULL,
           rapm_coefs_d = NULL,
           path_rapm_coefs_side = config$path_rapm_coefs_side) {

    if(is.null(rapm_coefs_o)) {
      rapm_coefs_o <-
        .import_data_from_path(
          ...,
          side = "o",
          path = path_rapm_coefs_side
        )
    }

    if(is.null(rapm_coefs_d)) {
      rapm_coefs_d <-
        .import_data_from_path(
          ...,
          side = "d",
          path = path_rapm_coefs_side
        )
    }

    bind_rows(
      rapm_coefs_o %>% mutate(side = "o"),
      rapm_coefs_d %>% mutate(side = "d")
    ) %>%
      spread(side, rapm) %>%
      rename_at(vars(o, d), funs(paste0(., "rapm"))) %>%
      mutate(rapm = orapm + drapm) %>%
      arrange(desc(rapm)) %>%
      # mutate_at(vars(matches("rapm$")), funs(rank = row_number(desc(.)))) %>%
      mutate_at(vars(rapm), funs(rank = row_number(desc(.))))
  }

# + TODO: Make this work for individual sides! (Right now, it only works for
# `rapm_coefs` combined.
# + This has been separated into its own function so that it can be
# used "dynamically" in multiple places. (This is also why it does not
# export the output data.)
.name_coefs <-
  function(..., rapm_coefs, players_nbastatr = NULL) {

    if(is.null(players_nbastatr)) {
      players_nbastatr <-
        .try_import_players_nbastatr(...)
    }
    players_slim <-
      players_nbastatr %>%
      select(id = id_player, name = name_player, slug = slug_team)

    rapm_coefs_named_base <-
      rapm_coefs %>%
      rename(id = id_player) %>%
      left_join(players_slim, by = "id")

    cols_rapm <-
      rapm_coefs %>%
      names() %>%
      str_subset("rapm")

    n_cols_rapm <- length(cols_rapm)
    if(!(n_cols_rapm == 1L | n_cols_rapm == 3L)) {
      .display_error(
        glue::glue("An unexpected number of RAPM columns {n_cols_rapm} were detected."),
        ...
      )
    }
    # cols_rapm <- paste0(c("o", "d", ""), "rapm")
    # prefix <- cols_rapm %>% str_remove("rapm")
    # match.arg(prefix, choices = c("o", "d", ""), several.ok = TRUE)

    cols_other_base <-
      intersect(c("id", "name", "slug", "rank"), names(rapm_coefs))
    cols_order_base <-
      c(
        cols_other_base,
        cols_rapm
      )
    cols_order_other <-
      setdiff(names(players_slim), cols_order_base)
    cols_order <- c(cols_order_other, cols_order_base)

    rapm_coefs_named <-
      rapm_coefs_named_base %>%
      select(one_of(cols_order))
    rapm_coefs_named
  }

.finalize_rapm_coefs <-
  function(...,
           rapm_coefs,
           show = FALSE,
           path_rapm_coefs = config$path_rapm_coefs) {
    rapm_coefs <-
      .name_coefs(
        ...,
        rapm_coefs = rapm_coefs
      )

    # Use this if cross-checking "unexpected" RAPM values with calculated stats
    # to try to identify why these unexpected values may be like they are.
    # players_summary_calc <-
    #   .import_data_from_path(
    #     ...,
    #     path = path_players_summary_calc
    #   )

    path_export <-
      .export_data_from_path(
        ...,
        data = rapm_coefs,
        path = path_rapm_coefs
      )

    if(interactive() & show) {
      file.show(
        .get_path_from(
          ...,
          path = path_rapm_coefs
        )
      )
    }
    rapm_coefs
  }

# Note that this can be used interactively (due to the `NULL` logic).
.join_rapm_coefs <-
  function(...,
           show = TRUE,
           rapm_coefs = NULL,
           path_rapm_coefs = config$path_rapm_coefs,
           path_rapm_coefs_join = config$path_rapm_coefs_join) {

    if(is.null(rapm_coefs)) {
      rapm_coefs <- .try_import_rapm_coefs(...)
    }
    rapm_basektball_analytics <- .try_import_rapm_szou(...)
    rpm_espn <- .try_import_rpm_espn(...)

    rapm_coefs_slim <-
      rapm_coefs %>%
      select(-matches("[o|d]rapm_rank")) %>%
      rename_at(vars(matches("rank|rapm")), funs(paste0(., "_calc"))) %>%
      select(
        name,
        matches("rank|rapm")
      )
    rapm_basektball_analytics_slim <-
      rapm_basektball_analytics %>%
      rename_at(vars(matches("rank|rapm")), funs(paste0(., "_szou"))) %>%
      select(
        name,
        slug,
        matches("rank|rapm")
      )
    rpm_espn_slim <-
      rpm_espn %>%
      rename_at(vars(matches("rank|rpm")), funs(paste0(., "_espn"))) %>%
      select(
        name,
        position,
        matches("rank|rpm")
      )

    suppressMessages(
      rapm_coefs_join <-
        rapm_coefs_slim %>%
        left_join(rapm_basektball_analytics_slim) %>%
        left_join( rpm_espn_slim) %>%
        select(
          name,
          slug,
          matches("rank"),
          matches("^r"),
          matches("^o"),
          matches("^d")
        )
    )

    # Make names distinct.
    suppressMessages(
      rapm_coefs_join <-
        rapm_coefs_join %>%
        # group_by(name) %>%
        # filter(row_number() == 1) %>%
        # ungroup()
        group_by(name) %>%
        summarise_at(vars(slug), funs(paste(., collapse = ", ", sep = ""))) %>%
        ungroup() %>%
        left_join(rapm_coefs_join)
    )

    path_export <-
      .export_data_from_path(
        ...,
        data = rapm_coefs_join,
        path = path_rapm_coefs_join
      )
    if(interactive() & show) {
      file.show(
        .get_path_from(
          ...,
          path = path_rapm_coefs_join
        )
      )
    }
    invisible(rapm_coefs_join)
  }


.fit_rapm_models <-
  function(...,
           # Unfortunately, these cant be abstracted away with `...`
           # because they have _[o|d]` suffixes.
           lambda_o = .LAMBDA,
           lambda_d = .LAMBDA) {

    .display_auto_step(
      glue::glue("Fitting RAPM models."),
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
    # invisible(list(fit_o = fit_o, fit_d = fit_d))

    rapm_coefs_o <- .extract_rapm_coefs_side(..., side = "o")
    rapm_coefs_d <- .extract_rapm_coefs_side(..., side = "d")

    rapm_coefs <-
      .join_rapm_coefs_od(
        ...,
        rapm_coefs_o = rapm_coefs_o,
        rapm_coefs_d = rapm_coefs_d
      )

    rapm_coefs <-
      .finalize_rapm_coefs(
        ...,
        rapm_coefs = rapm_coefs
      )

    rapm_coefs_join <-
      .join_rapm_coefs(
        ...,
        rapm_coefs = rapm_coefs
      )
    invisible(rapm_coefs_join)

  }

auto_fit_rapm_models <-
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
    .fit_rapm_models(
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
# .extract_rapm_coefs_manual <-
#   function(..., skip = FALSE, season = .SEASONS) {
#     purrr::map(season, ~.extract_rapm_coefs(..., skip = skip, season = .x))
#   }


