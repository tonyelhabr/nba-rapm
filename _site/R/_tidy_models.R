

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
      select(id = term, estimate) %>%
      filter(!is.na(id))
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
      group_by(id) %>%
      summarise_at(vars(-id), funs(sum)) %>%
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


.rank_arrange <-
  function(data, col) {
    col <- ensym2(col)
    data %>%
      mutate_at(vars(!!col), funs(rank = row_number(desc(.)))) %>%
      arrange(desc(!!col))
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
      left_join(players, by = "id")

    cols_metric <-
      coefs %>%
      names() %>%
      str_subset(rgx)

    n_cols_metric <- length(cols_metric)
    # Note that 1 is expected when just `*` (e.g. `rapm`), and 3 is expected when `o*`, `d*` and `*`
    if(!(n_cols_metric == 1L | n_cols_metric == 3L)) {
      .display_error(
        glue::glue(
          "An unexpected number of coefficient columns ({n_cols_metric}) ",
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
        cols_metric
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

    coefs <- coefs %>% .rank_arrange(col = !!col)
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

.METRIC_SRC <-
  c("rapm_calc", "rapm_both_calc", "rapm_sz", "rpm_espn", "pm_calc", "pm_nbastatr", "bpm_nbastatr")
.SRC <- c("calc", "both_calc", "sz", "espn", "nbastatr")
# .minimize_coefs <- function(data, src = .SRC, suffix = paste0("_", src), rgx = "rank|pm") {
#   # src <- match.arg(.SRC)
#   data %>%
#     # select(-matches("name")) %>%
#     select(-matches("[o|d].*pm_rank")) %>%
#     rename_at(vars(matches(rgx)), funs(paste0(., suffix))) %>%
#     select(
#       name,
#       matches("^slug$"),
#       matches("^position"), # from `espn` data.
#       matches("rank"),
#       matches("pm")
#     )
# }

# Reference: `{tetidy}` package
# TODO: Use this for `.name_coefs()`?
.select_one_of <- function (data, cols, drop = FALSE, warn = !drop, stop = FALSE)  {
    cols_all <- names(data)
    cols_in <- intersect(cols, cols_all)
    if (!drop) {
      cols_nin <- setdiff(cols_all, cols)
      cols_fct <- factor(cols_all, levels = c(cols_in, cols_nin))
    } else {
      cols_mismatch <- setdiff(cols, cols_in)

      # Note that `dplyr::select()` already has a warning for extra columns.
      if(length(cols_mismatch) > 0 & (warn | stop)) {
        cols_mismatch <- paste0("`", paste(cols_mismatch, collapse = "`, `", sep = ""), "`")
        msg <- sprintf("Extra columns specified in `cols`: %s.", cols_mismatch)
        if(warn) {
          warning(msg, call. = FALSE)
        } else if (stop) {
          stop(msg, call. = FALSE)
        }
      }
      cols_fct <- factor(cols, levels = cols)
    }
    # Set `.vars` to prevent the warning from occurring here (but this doesn't work(?)).
    # res <- dplyr::select(data, tidyselect::one_of(levels(cols_fct), .vars = cols_fct))
    suppressWarnings(
      res <- dplyr::select(data, tidyselect::one_of(levels(cols_fct)))
    )
    res
}
# Debugging...
# .cols_mtcars <- c("carb", "vs", "a", "b")
# .select_one_of(mtcars, cols = .cols_mtcars, drop = F)
# .select_one_of(mtcars, cols = .cols_mtcars, drop = T)

..get_cols_metrics <-
  function(...) {
    prefix3 <- c("o", "d", "")
    c(
      "id",
      "name",
      "slug",
      # "position",
      c("gp_calc", "poss_both_calc", "mp_both_calc"),
      paste0("rank",
             c(
               "_pm_nbastatr",
               "_bpm_nbastatr",
               "_rapm_calc",
               "_rapm_both_calc",
               "_apm_calc",
               "_apm_both_calc",
               "_rapm_sz",
               "_rpm_espn"
             )),
      # paste0("pm", c("_calc", "_nbastatr")),
      paste0("pm", c("_nbastatr")),
      paste0(paste0(prefix3, "bpm"), "_nbastatr"),
      paste0(paste0(prefix3, "rapm"), "_calc"),
      paste0("rapm", "_both_calc"),
      # paste0(paste0(prefix3, "apm"), "_calc"),
      paste0(paste0("apm"), "_calc"),
      paste0("apm", "_both_calc"),
      paste0(paste0(prefix3, "rapm"), "_sz"),
      paste0(paste0(prefix3, "rpm"), "_espn")

    )
  }

.join_metrics <-
  function(...,
           x,
           y,
           sep = "_",
           col_rank = "rank",
           metric,
           src,
           rgx_rename = paste0(metric, "|", src),
           rgx_select = paste0("id|name|", col_rank, "|", rgx_rename)) {
    stopifnot(is.character(metric), length(metric) == 1L)
    stopifnot(is.character(src), length(src) == 1L)

    col_rank <- ensym2(col_rank)
    # col_rank <- sym(col_rank)
    y_renamed <-
      y %>%
      select(matches(rgx_select)) %>%
      rename_at(vars(matches(rgx_rename)), funs(paste0(., sep, src))) %>%
      rename_at(vars(!!col_rank), funs(paste0(., sep, metric, sep, src)))
    suppressMessages(
      res <- x %>% left_join(y_renamed)
    )
    res
  }


.combine_metrics_noncalc <-
  function(...) {

    # players_summary_compare <- .try_import_players_summary_compare(...)
    pm_nbastatr <- .try_import_pm_nbastatr(...)
    bpm_nbastatr <- .try_import_bpm_nbastatr(...)
    rapm_sz <- .try_import_rapm_sz(...)
    rpm_espn <- .try_import_rpm_espn(...)
    # rpm_rd <- .try_import_rapm_rd(...)

    cols_metrics <-
      ..get_cols_metrics(...)

    suppressMessages(
      metrics_join <-
        pm_nbastatr %>%
        mutate(
          rank_pm_nbastatr = row_number(desc(pm_nbastatr))
        ) %>%
        # left_join(
        #   bpm_nbastatr %>%
        #     rename_at(vars(matches("pm")), funs(paste0(., "_nbastatr"))) %>%
        #     rename_at(vars(rank), funs(paste0(., "_bpm_nbastatr")))
        # ) %>%
        .join_metrics(
          ...,
          x = .,
          y = bpm_nbastatr,
          metric = "bpm",
          src = "nbastatr"
        ) %>%
        # left_join(
        #   rapm_sz %>%
        #     rename_at(vars(matches("pm")), funs(paste0(., "_sz"))) %>%
        #     rename_at(vars(rank), funs(paste0(., "_rapm_sz")))
        # ) %>%
        .join_metrics(
          ...,
          x = .,
          y = rapm_sz,
          metric = "rapm",
          src = "sz"
        ) %>%
        # left_join(
        #   rpm_espn %>%
        #     rename_at(vars(matches("pm")), funs(paste0(., "_espn"))) %>%
        #     rename_at(vars(rank), funs(paste0(., "_rpm_espn")))
        # )
        .join_metrics(
          ...,
          x = .,
          y = rpm_espn,
          metric = "rpm",
          src = "espn"
        )
    )

    metrics_join <-
      metrics_join %>%
      .select_one_of(cols = cols_metrics, drop = TRUE, warn = FALSE)
    metrics_join
  }

.finalize_metrics_join <-
  function(...,
           show = TRUE,
           metrics_join,
           path_metrics_join = config$path_metrics_join) {

    cols_metrics <- ..get_cols_metrics()
    metrics_join <-
      metrics_join %>%
      .select_one_of(cols = cols_metrics, drop = TRUE, warn = FALSE)

    metrics_join <-
      metrics_join %>%
      # group_by(name) %>%
      # mutate_at(vars(slug), funs(paste(., collapse = ", ", sep = ""))) %>%
      # ungroup() %>%
      group_by(name) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      arrange(rank_pm_nbastatr)

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

.tidy_models <-
  function(...) {

    .display_auto_step(
      glue::glue("Summarizing information about the models."),
      ...
    )

    metrics_join <-
      .combine_metrics_noncalc(...)

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

    metrics_join <-
      .join_metrics(
        ...,
        x = metrics_join,
        y = rapm_coefs_calc,
        metric = "rapm",
        src = "calc"
      )

    # rapm_coefs_both_calc ----
    rapm_coefs_both_calc <- .extract_rapm_coefs_both(...)
    rapm_coefs_both_calc <-
      .finalize_rapm_coefs_both(
        ...,
        coefs = rapm_coefs_both_calc
      )

    metrics_join <-
      .join_metrics(
        ...,
        x = metrics_join,
        y = rapm_coefs_both_calc,
        metric = "rapm",
        src = "both_calc"
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

    metrics_join <-
      .join_metrics(
        ...,
        x = metrics_join,,
        y = apm_coefs_calc,
        metric = "apm",
        src = "calc"
      )

    # rapm_coefs_both_calc ----
    # apm_coefs_both_calc <- .extract_apm_coefs_both(...)
    # apm_coefs_both_calc <-
    #   .finalize_apm_coefs_both(
    #     ...,
    #     coefs = apm_coefs_both_calc
    #   )
    metrics_join <-
      .finalize_metrics_join(
        ...,
        metrics_join = metrics_join
      )
    metrics_join

  }

auto_tidy_models <-
  function(...,
           season = config$season,
           skip = config$skip,
           verbose = config$verbose,
           export = config$export,
           backup = config$backup,
           clean = config$clean,
           n_keep = config$n_keep) {

    .tidy_models(
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

