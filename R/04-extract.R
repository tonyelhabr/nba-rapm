
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

.extract_coefs <-
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

.extract_rapm_coefs_side <-
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
    coefs <-
      .extract_coefs(
        ...,
        fit = fit
      )

    .export_data_from_path(
      ...,
      data = coefs,
      path = path_rapm_coefs_side
    )

    invisible(coefs)
  }

# + TODO: Make this work for individual sides! (Right now, it only works for
# `coefs` combined.
# + This has been separated into its own function so that it can be
# used "dynamically" in multiple places. (This is also why it does not
# export the output data.)
.name_coefs <-
  function(..., coefs, players_nbastatr = NULL) {

    if(is.null(players_nbastatr)) {
      players_nbastatr <-
        .try_import_players_nbastatr(...)
    }
    players_slim <-
      players_nbastatr %>%
      select(id = id_player, name = name_player, slug = slug_team)

    coefs_named_base <-
      coefs %>%
      rename(id = id_player) %>%
      left_join(players_slim, by = "id")

    prefix_cols <- c("o", "d", "")
    cols_order_base <-
      c(
        "id",
        "name",
        "slug",
        # paste0(prefix_cols, "rapm_rank"),
        "rank",
        paste0(prefix_cols, "rapm")
      )
    cols_order_other <-
      setdiff(names(players_slim), cols_order_base)
    cols_order <- c(cols_order_base, cols_order_other)

    coefs_named <-
      coefs_named_base %>%
      select(one_of(cols_order))
    coefs_named

  }

extract_rapm_coefs <-
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

    .display_progress(
      glue::glue("Step 4: Extracting model coefs."),
      ...
    )
    # browser()
    coefs_o <- .extract_rapm_coefs_side(..., side = "o")
    coefs_d <- .extract_rapm_coefs_side(..., side = "d")

    coefs <-
      bind_rows(
        coefs_o %>% mutate(side = "o"),
        coefs_d %>% mutate(side = "d")
      ) %>%
      spread(side, rapm) %>%
      rename_at(vars(o, d), funs(paste0(., "rapm"))) %>%
      mutate(rapm = orapm + drapm) %>%
      arrange(desc(rapm)) %>%
      # mutate_at(vars(matches("rapm$")), funs(rank = row_number(desc(.)))) %>%
      mutate_at(vars(rapm), funs(rank = row_number(desc(.))))


    # Use this if cross-checking "unexpected" RAPM values with calculated stats
    # to try to identify why these unexpected values may be like they are.
    # players_summary_calc <-
    #   .import_data_from_path(
    #     ...,
    #     path = path_players_summary_calc
    #   )
    coefs_named <- coefs %>% .name_coefs(...)

    path_export <-
      .export_data_from_path(
        ...,
        data = coefs_named,
        path = path_rapm_coefs
      )
    invisible(coefs_named)
  }

extract_rapm_coefs_auto <-
  function(...,
           season = config$season,
           skip = config$skip,
           verbose = config$verbose,
           export = config$export,
           backup = config$backup,
           clean = config$clean,
           n_keep = config$n_keep) {
    extract_rapm_coefs(
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
# .extract_rapm_coefs_manual <-
#   function(..., skip = FALSE, season = .SEASONS) {
#     purrr::map(season, ~extract_rapm_coefs(..., skip = skip, season = .x))
#   }

