
# Note that this is a work-around since `broom:::tidy.glmnet()` isn't working(?)
.tidy_glmnet <- function(x, return_zeros = FALSE, ...) {
  # This is the major change to `broom:::tidy.glmnet()`.
  # beta <- coef(x)
  beta <- x$beta
  # browser()
  beta_d <-
      broom:::fix_data_frame(
      as.matrix(beta),
      newnames = 1:ncol(beta),
      newcol = "term"
    )
    ret <- tidyr::gather(beta_d, step, estimate, -term)
    # add values specific to each step
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

.extract_estimates <-
  function(..., fit) {
    # browser()
    fit %>%
      # broom::tidy() %>%
      .tidy_glmnet() %>%
      filter(term != "(Intercept)") %>%
      mutate_at(vars(term), funs(str_replace(., "^.", ""))) %>%
      mutate_at(
        vars(term),
        funs(str_replace_all(., "^[0]+", "") %>% as.integer())
      ) %>%
      arrange(desc(estimate)) %>%
      select(id_player = term, rapm = estimate) %>%
      filter(!is.na(id_player))
  }

.extract_rapm_estimates_side <-
  function(...,
           side,
           path_rapm_fit_side,
           path_rapm_estimates_side) {
    fit <-
      .import_data_from_path(
        ...,
        path = path_rapm_fit_side
      )

    estimates <-
      .extract_estimates(
        ...,
        fit = fit
      ) %>%
      mutate(prefix = side)

    .export_data_from_path(
      ...,
      data = estimates,
      path = path_rapm_estimates_side
    )

    invisible(estimates)
  }

.extract_rapm_estimates_o <-
  function(...) {
    .extract_rapm_estimates_side(
      ...,
      side = "o",
      path_rapm_fit_side = config$path_rapm_fit_o,
      path_rapm_estimates_side = config$path_rapm_estimates_o
    )
  }

.extract_rapm_estimates_d <-
  function(...) {
    .extract_rapm_estimates_side(
      ...,
      side = "d",
      path_rapm_fit_side = config$path_rapm_fit_d,
      path_rapm_estimates_side = config$path_rapm_estimates_d
    )
  }

extract_rapm_estimates <-
  function(...,
           # Note that some of these are used for `.try_skip()`.
           path_rapm_fit_o = config$path_rapm_fit_o,
           path_rapm_fit_d = config$path_rapm_fit_d,
           path_players_summary_calc = config$path_players_summary_calc,
           path_rapm_estimates = config$path_rapm_estimates) {

    will_skip <-
      .try_skip(
        ...,
        path_reqs =
          c(
            path_rapm_fit_o,
            path_rapm_fit_d,
            path_players_summary_calc
          ),
        path_deps =
          c(
            path_rapm_estimates
          )
      )

    if(will_skip) {
      return(invisible(NULL))
    }

    # browser()
    estimates_o <- .extract_rapm_estimates_o(...)
    estimates_d <- .extract_rapm_estimates_d(...)

    estimates <-
      bind_rows(
        estimates_o,
        estimates_d
      ) %>%
      spread(prefix, rapm) %>%
      rename_at(vars(o, d), funs(paste0(., "rapm"))) %>%
      mutate(rapm = orapm + drapm) %>%
      arrange(desc(rapm))


    # Use this if cross-checking "unexpected" RAPM values with calculated stats
    # to try to identify why these unexpected values may be like they are.
    # players_summary_calc <-
    #   .import_data_from_path(
    #     ...,
    #     path = path_players_summary_calc
    #   )
    players_nbastatr <-
      .try_import_players_nbastatr(...)
    players_slim <-
      players_nbastatr %>%
      select(id = id_player, name = name_player, slug = slug_team)

    estimates_pretty_base <-
      estimates %>%
      # mutate_at(vars(matches("rapm$")), funs(rank = row_number(desc(.)))) %>%
      mutate_at(vars(rapm), funs(rank = row_number(desc(.)))) %>%
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

    estimates_pretty <-
      estimates_pretty_base %>%
      select(one_of(cols_order))

    path_export <-
      .export_data_from_path(
        ...,
        data = estimates_pretty,
        path = path_rapm_estimates
      )
    invisible(estimates_pretty)
  }

# extract_rapm_estimates_auto <-
#   purrr::partial(
#     extract_rapm_estimates,
#     season = config$season,
#     skip = config$skip,
#     verbose = config$verbose,
#     export = config$export,
#     backup = config$backup,
#     clean = config$clean,
#     n_keep = config$n_keep
#   )

extract_rapm_estimates_auto <-
  function(...,
           season = config$season,
           skip = config$skip,
           verbose = config$verbose,
           export = config$export,
           backup = config$backup,
           clean = config$clean,
           n_keep = config$n_keep) {
    extract_rapm_estimates(
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
# .extract_rapm_estimates_manual <-
#   function(..., skip = FALSE, season = .SEASONS) {
#     purrr::map(season, ~extract_rapm_estimates(..., skip = skip, season = .x))
#   }

