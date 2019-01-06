

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

.postprocess_broom_tidy <-
  function(data) {
    data %>%
      mutate_at(
        vars(term),
        funs(str_replace_all(., "^[od]", "") %>% as.integer())
      ) %>%
      arrange(desc(estimate)) %>%
      select(id_player = term, estimate) %>%
      filter(!is.na(id_player))
  }

.extract_glment_coefs <-
  function(..., fit) {
    fit %>%
      # broom::tidy() %>%
      .tidy_glmnet() %>%
      # filter(term == "(Intercept)") %>%
      mutate_at(
        vars(term),
        funs(if_else(. != "(Intercept)", ., "0"))
      )
  }

.extract_rapm_coefs_side <-
  function(...,
           path_rapm_model_side = config$path_rapm_model_side,
           # path_rapm_coefs_var_side = config$path_rapm_coefs_var_side,
           path_rapm_coefs_side = config$path_rapm_coefs_side) {
    fit <-
      .import_data_from_path(
        ...,
        path = path_rapm_model_side
      )

    # # TODO: Do something better here (or nothing at at all)?
    # path_export <-
    #   .export_data_from_path(
    #     ...,
    #     data = fit %>% .tidy_glmnet(),
    #     path = path_rapm_coefs_var_side
    #   )
    rapm_coefs <-
      .extract_glment_coefs(
        ...,
        fit = fit
      ) %>%
      .postprocess_broom_tidy()

    .export_data_from_path(
      ...,
      data = rapm_coefs,
      path = path_rapm_coefs_side
    )
    rapm_coefs
  }


.summarise_coefs_both <-
  function(data) {
    data %>%
      group_by(id_player) %>%
      summarise_at(vars(-id_player), funs(sum)) %>%
      ungroup()
  }

.extract_rapm_coefs_both <-
  function(...) {
    .extract_rapm_coefs_side(
      ...,
      side = NULL
    ) %>%
      .summarise_coefs_both() %>%
      rename(rapm = estimate)
  }

.extract_apm_coefs_side <-
  function(...,
           path_apm_model_side = config$path_apm_model_side,
           # path_apm_coefs_var_side = config$path_apm_coefs_var_side,
           path_apm_coefs_side = config$path_apm_coefs_side) {
    fit <-
      .import_data_from_path(
        ...,
        path = path_apm_model_side
      )

    apm_coefs <-
      fit %>%
      broom::tidy() %>%
      .postprocess_broom_tidy()

    .export_data_from_path(
      ...,
      data = apm_coefs,
      path = path_apm_coefs_side
    )
    apm_coefs
  }

.extract_apm_coefs_both <-
  function(...) {
    .extract_apm_coefs_side(
      ...,
      side = NULL
    ) %>%
      .summarise_coefs_both() %>%
      rename(apm = estimate)
  }


.combine_coefs_side <-
  function(...,
           coefs_o,
           coefs_d,
           col,
           suffix = col) {
    stopifnot(is.character(suffix), length(suffix) == 1L)
    col <- sym(suffix)
    col_o <- sym(glue::glue("o{suffix}"))
    col_d <- sym(glue::glue("d{suffix}"))
    bind_rows(
      coefs_o %>% mutate(side = "o"),
      coefs_d %>% mutate(side = "d")
    ) %>%
      spread(side, estimate) %>%
      rename_at(vars(o, d), funs(paste0(., suffix))) %>%
      mutate(!!col := !!col_o + !!col_d)
  }


.rank_coefs <-
  function(data, col) {
    col <- ensym2(col)
    data %>%
      arrange(desc(!!col)) %>%
      mutate_at(vars(!!col), funs(rank = row_number(desc(.))))
  }

# + TODO: Make this work for individual sides! (Right now, it only works for
# `rapm_coefs_calc` combined.
# + This has been separated into its own function so that it can be
# used "dynamically" in multiple places. (This is also why it does not
# export the output data.)
.name_coefs <-
  function(...,
           coefs,
           rgx = "",
           # "Catch" `side` to prevent it from being used in the `nbastatr` function.
           side) {


    players_nbastatr <-
      .try_import_players_nbastatr(...)

    players <-
      players_nbastatr %>%
      select(id = id_player, name = name_player, slug = slug_team)

    coefs_named_base <-
      coefs %>%
      rename(id = id_player) %>%
      left_join(players, by = "id")

    cols_rapm <-
      coefs %>%
      names() %>%
      str_subset(rgx)

    n_cols_rapm <- length(cols_rapm)
    if(!(n_cols_rapm == 1L | n_cols_rapm == 3L)) {
      .display_error(
        glue::glue(
          "An unexpected number of coefficient columns ({n_cols_rapm}) ",
          " was detected."
        ),
        ...
      )
    }

    cols_other_base <-
      intersect(c("id", "name", "slug", "rank"), names(coefs))
    cols_order_base <-
      c(
        cols_other_base,
        cols_rapm
      )
    cols_order_other <-
      setdiff(names(players), cols_order_base)
    cols_order <- c(cols_order_other, cols_order_base)

    coefs_named <-
      coefs_named_base %>%
      select(one_of(cols_order))
    coefs_named
  }


.finalize_coefs <-
  function(...,
           coefs,
           col,
           path) {

    coefs <- coefs %>% .rank_coefs(col = !!col)
    coefs <- .name_coefs(..., coefs = coefs, rgx = col)

    path_export <-
      .export_data_from_path(
        ...,
        data = coefs,
        path = path
      )
    coefs
  }


.finalize_rapm_coefs <-
  function(...,
           path = config$path_rapm_coefs_calc) {

    .finalize_coefs(
      ...,
      col = "rapm",
      side = NULL,
      path = path
    )
  }

.finalize_rapm_coefs_both <-
  function(...,
           path = config$path_rapm_coefs_calc) {

    .finalize_coefs(
      ...,
      col = "rapm",
      side = "both",
      path = path
    )
  }

.finalize_apm_coefs <-
  function(...,
           path = config$path_apm_coefs_calc) {

    .finalize_coefs(
      ...,
      col = "apm",
      side = NULL,
      path = path
    )
  }

.finalize_apm_coefs_both <-
  function(...,
           path = config$path_apm_coefs_calc) {

    .finalize_coefs(
      ...,
      col = "apm",
      side = "both",
      path = path
    )
  }

# Note that this can be used interactively (due to the `NULL` logic).
.join_metrics <-
  function(...,
           show = TRUE,
           path_players_summary_compare = config$path_players_summary_compare,
           rapm_coefs_calc = NULL,
           rapm_coefs_both_calc = NULL,
           apm_coefs_calc = NULL,
           apm_coefs_both_calc = NULL,
           rapm_sz = .try_import_rapm_sz(...),
           rpm_espn = .try_import_rpm_espn(...),
           # rpm_rd = .try_import_rapm_rd(...)<
           bpm_nbastatr = .try_import_bpm_nbastatr(...),
           path_metrics_join = config$path_metrics_join) {

    players_summary_compare <- .try_import_players_summary_compare(...)
    players_summary_compare_min <-
      players_summary_compare %>%
      select(
        # id_player,
        name = name_player,
        pm_calc,
        pm_nbastatr
      )


    .minimize_coefs <- function(data, src = .SRC) {
      # src <- match.arg(.SRC)
      data %>%
        select(-matches("[o|d]rapm_rank")) %>%
        rename_at(vars(matches("rank|rapm")), funs(paste0(., "_", src))) %>%
        select(
          name,
          matches("^slug$"),
          matches("^position"),
          matches("rank"),
          matches("pm")
        )
    }

    # TODO: `while` loop for joining if `NULL`?
    # while(FALSE) {}
    suppressMessages(
      metrics_join <-
        players_summary_compare_min %>%
        left_join(
          rapm_coefs_calc %>%
            .minimize_coefs(src = "calc")
        ) %>%
        left_join(
          rapm_coefs_both_calc %>%
            .minimize_coefs(src = "both_calc")
        ) %>%
        left_join(
          rapm_sz %>%
            .minimize_coefs(src = "sz")
        ) %>%
        left_join(
          rpm_espn %>%
            .minimize_coefs(src = "espn")
        )
    )
    metrics_join <-
      metrics_join %>%
      select(
        name,
        slug,
        matches("rank"),
        matches("pm")
      )

    # TODO: Fix this!
    # Make names distinct.
    suppressMessages(
      metrics_join <-
        metrics_join %>%
        group_by(name) %>%
        filter(row_number() == 1) %>%
        ungroup() %>%
        # group_by(name) %>%
        # summarise_at(vars(slug), funs(paste(., collapse = ", ", sep = ""))) %>%
        # ungroup() %>%
        left_join(metrics_join)
    )

    path_export <-
      .export_data_from_path(
        ...,
        data = metrics_join,
        path = path_metrics_join
      )
    if(interactive() & show) {
      file.show(
        .get_path_from(
          ...,
          path = path_metrics_join
        )
      )
    }
    metrics_join
  }

.extract_coefs <-
  function(...) {

    .display_auto_step(
      glue::glue("Extracting model coefficients."),
      ...
    )

    # rapm_coefs_calc ----
    rapm_coefs_o_calc <- .extract_rapm_coefs_side(..., side = "o")
    rapm_coefs_d_calc <- .extract_rapm_coefs_side(..., side = "d")
    rapm_coefs_calc <-
      .combine_coefs_side(
        ...,
        coefs_o = rapm_coefs_o_calc,
        coefs_d = rapm_coefs_d_calc,
        col = "rapm"
      )
    rapm_coefs_calc <-
      .finalize_rapm_coefs(
        ...,
        coefs = rapm_coefs_calc
      )

    # rapm_coefs_both_calc ----
    rapm_coefs_both_calc <- .extract_rapm_coefs_both(...)
    rapm_coefs_both_calc <-
      .finalize_rapm_coefs_both(
        ...,
        coefs = rapm_coefs_both_calc
      )

    # apm_coefs_calc ----
    apm_coefs_o_calc<- .extract_apm_coefs_side(..., side = "o")
    apm_coefs_d_calc <- .extract_apm_coefs_side(..., side = "d")

    apm_coefs_calc <-
      .combine_coefs_side(
        ...,
        coefs_o = apm_coefs_o_calc,
        coefs_d = apm_coefs_d_calc,
        col = "apm"
      )

    apm_coefs_calc <-
      .finalize_apm_coefs(
        ...,
        coefs = apm_coefs_calc
      )

    # rapm_coefs_both_calc ----
    # apm_coefs_both_calc <- .extract_apm_coefs_both(...)
    # apm_coefs_both_calc <-
    #   .finalize_apm_coefs_both(
    #     ...,
    #     coefs = apm_coefs_both_calc
    #   )

    # coefs_join ----
    metrics_join <-
      .join_metrics(
        ...,
        rapm_coefs_calc = rapm_coefs_calc,
        rapm_coefs_both_calc = rapm_coefs_both_calc,
        apm_coefs_calc = apm_coefs_calc# ,
        # apm_coefs_both_calc = apm_coefs_both_calc
      )
    metrics_join

  }

auto_extract_coefs <-
  function(...,
           season = config$season,
           skip = config$skip,
           verbose = config$verbose,
           export = config$export,
           backup = config$backup,
           clean = config$clean,
           n_keep = config$n_keep) {
    .extract_coefs(
      ...,
      season = season,
      skip = skip,
      verbose = verbose,
      export = export,
      backup = backup,
      clean = clean,
      n_keep = n_keep
    )
  }

