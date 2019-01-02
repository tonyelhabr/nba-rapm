
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

..extract_coefs <-
  function(..., fit) {
    fit %>%
      # broom::tidy() %>%
      .tidy_glmnet() %>%
      filter(term != "(Intercept)") %>%
      mutate_at(
        vars(term),
        funs(str_replace_all(., "^[od]", "") %>% as.integer())
      ) %>%
      arrange(desc(estimate)) %>%
      select(id_player = term, rapm = estimate) %>%
      filter(!is.na(id_player))
  }

.extract_coefs_side <-
  function(...,
           path_rapm_fit_side = config$path_rapm_fit_side,
           path_rapm_coefs_var_side = config$path_rapm_coefs_var_side,
           path_rapm_coefs_side = config$path_rapm_coefs_side) {
    fit <-
      .import_data_from_path(
        ...,
        path = path_rapm_fit_side
      )


    # TODO: Do something better here!
    path_export <-
      .export_data_from_path(
        ...,
        data = fit %>% .tidy_glmnet(),
        path = path_rapm_coefs_var_side
      )
    rapm_coefs <-
      ..extract_coefs(
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

.join_coefs_od <-
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

.extract_coefs <-
  function(...,
           path_rapm_fit_side = config$path_rapm_fit_side,
           # path_players_summary_calc = config$path_players_summary_calc,
           path_rapm_coefs = config$path_rapm_coefs) {

    will_skip <-
      .try_skip(
        ...,
        path_reqs =
          c(
            # .get_path_from(..., path = path_players_summary_calc)
            .get_path_from(..., path = path_rapm_fit_side, side = "o"),
            .get_path_from(..., path = path_rapm_fit_side, side = "d")
          ),
        path_deps =
          c(
            .get_path_from(..., path = path_rapm_coefs)
          )
      )

    if(will_skip) {
      return(invisible(NULL))
    }

    .display_auto_step(
      glue::glue("Step 4: Extracting model rapm_coefs."),
      ...
    )

    rapm_coefs_o <- .extract_coefs_side(..., side = "o")
    rapm_coefs_d <- .extract_coefs_side(..., side = "d")

    rapm_coefs <-
      .join_coefs_od(
        ...,
        rapm_coefs_o = rapm_coefs_o,
        rapm_coefs_d = rapm_coefs_d
      )

    # Use this if cross-checking "unexpected" RAPM values with calculated stats
    # to try to identify why these unexpected values may be like they are.
    # players_summary_calc <-
    #   .import_data_from_path(
    #     ...,
    #     path = path_players_summary_calc
    #   )
    rapm_coefs_named <- .name_coefs(..., rapm_coefs = rapm_coefs)

    path_export <-
      .export_data_from_path(
        ...,
        data = rapm_coefs_named,
        path = path_rapm_coefs
      )
    invisible(rapm_coefs_named)
  }

extract_coefs_auto <-
  function(...,
           season = config$season,
           skip = config$skip,
           verbose = config$verbose,
           export = config$export,
           backup = config$backup,
           clean = config$clean,
           n_keep = config$n_keep) {

    .extract_coefs(
      # ...,
      season = season,
      skip = skip,
      verbose = verbose,
      export = export,
      backup = backup,
      clean = clean,
      n_keep = n_keep
    )
  }

# # This was used once "ad-hoc".
# .extract_coefs_manual <-
#   function(..., skip = FALSE, season = .SEASONS) {
#     purrr::map(season, ~.extract_coefs(..., skip = skip, season = .x))
#   }

