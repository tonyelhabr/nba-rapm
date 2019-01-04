
.compare_allinone_metrics <-
  function(...,
           path_rapm_coefs_join = config$path_rapm_coefs_join,
           path_allinone_metrics_compare = config$path_allineone_metrics_compare) {
    rapm_coefs_join <-
      .import_data_from_path(
        ...,
        path = path_rapm_coefs_join
      )
  }


.SRC <- c("calc", "szou", "espn")
.SRC_DEBUG <- c(.SRC, "nbastatr")
.create_rgx <-
  function(x) {
    paste0("(", paste(x, collapse = ")|(", sep = ""), ")") %>%
      str_replace_all("\\.", "[.]")
  }

.correlate_rapm_coefs <-
  function(...,
           path_rapm_coefs_join = config$path_rapm_coefs_join,
           src = .SRC,
           path_rapm_coefs_cors = config$path_rapm_coefs_cors) {
    rapm_coefs_join <-
      .import_data_from_path(
        ...,
        path = path_rapm_coefs_join
      )

    rgx_src <- src %>% .create_rgx()
    rapm_coefs_tidy <-
      rapm_coefs_join %>%
      gather(metric_src, value, matches(rgx_src)) %>%
      separate(metric_src, into = c("metric", "src"), sep = "_") %>%
      mutate_at(vars(src), funs(factor(., levels = !!src)))

    rapm_coefs_calcs <-
      # Don't need to normalize ranks because `"spearman"` method is being used(?)
      rapm_coefs_tidy %>%
      filter(metric == "rank") %>%
      spread(src, value) %>%
      select_at(vars(matches(rgx_src))) %>%
      corrr::correlate(method = "spearman", quiet = TRUE)

    rapm_coefs_cors <-
      rapm_coefs_calcs %>%
      gather(src2, value, matches(rgx_src)) %>%
      rename(src1 = rowname) %>%
      filter(!is.na(value)) %>%
      filter(src1 < src2) %>%
      # Just making sure that `calc` is always `src1`.
      mutate(temp = src1) %>%
      mutate(
        src1 = if_else(temp != "calc" & src2 == "calc", "calc", src1),
        src2 = if_else(temp != "calc" & src2 == "calc", temp, src2)
      ) %>%
      select(-temp) %>%
      arrange(src1, src2)

    path_export <-
      .export_data_from_path(
        ...,
        data = rapm_coefs_cors,
        path = path_rapm_coefs_cors
      )
    invisible(rapm_coefs_cors)
  }

# .SEQ_POSS_MIN <- seq(0, 5000, by = 5000)
.SEQ_POSS_MIN <- seq(0, 5000, by = 2500)
# .SEQ_LAMBDA <- seq(0, 1000, by = 1000)
.SEQ_LAMBDA <- seq(0, 1000, by = 200)

.get_rapm_coefs_cors_grid <-
  function(...,
           season = .SEASON,
           seq_poss_min = .SEQ_POSS_MIN,
           seq_lambda_o = .SEQ_LAMBDA,
           seq_lambda_d = .SEQ_LAMBDA,
           progress = TRUE,
           path_rapm_coefs_cors_grid = config$path_rapm_coefs_cors_grid) {

    .validate_season(season)
    params_grid <-
      crossing(
        .season = season,
        .poss_min = seq_poss_min,
        .lambda_o = seq_lambda_o,
        .lambda_d = seq_lambda_d
      ) %>%
      mutate(idx_grp = row_number()) %>%
      select(idx_grp, everything())
    f_clean <- memoise::memoise(auto_clean_pbp)
    f_reshape <- memoise::memoise(auto_reshape_pbp)

    if (progress) {
      .pb <- .create_pb(total = nrow(params_grid))
    }
    rapm_coefs_cors_grid_calcs <-
      params_grid %>%
      group_by(idx_grp) %>%
      mutate(results = purrr::pmap(list(
        .season, .poss_min, .lambda_o, .lambda_d
      ),
      .f = ~ {
        .display_auto_step(glue::glue("{.season}, {.poss_min}, {.lambda_o}, {.lambda_d}"))
        f_clean(# auto_clean_pbp(
          season = .season,
          # skip = FALSE
          skip = TRUE
        )
        f_reshape(# auto_reshape_pbp(
          season = .season,
          poss_min = .poss_min,
          skip = FALSE
        )
        auto_fit_rapm_models(
          season = .season,
          skip = FALSE,
          # skip = TRUE,
          lambda_o = .lambda_o,
          lambda_d = .lambda_d,
          optimize = FALSE
        )
        auto_extract_rapm_coefs(
          season = .season,
          skip = FALSE
        )
        rapm_coefs_cors <-
          .correlate_rapm_coefs(season = .season)
        # print(coefs_cors)
        if (!is.null(.pb)) {
          .pb$tick()
        }
        rapm_coefs_cors
      }))

    rapm_coefs_cors_grid <-
      rapm_coefs_cors_grid_calcs %>%
      ungroup() %>%
      unnest(results) %>%
      rename_at(vars(matches("^\\.")), funs(str_remove(., "^\\."))) %>%
      # filter(src1 == "calc" | src2 == "calc") %>%
      arrange(desc(value))
    path_export <-
      .export_data_from_path(
        ...,
        season = season,
        data = rapm_coefs_cors_grid,
        path = path_rapm_coefs_cors_grid
      )
    invisible(rapm_coefs_cors_grid)
  }

.summarise_rapm_coefs_cors <-
  function(...,
           path_rapm_coefs_cors_grid_summary = config$path_rapm_coefs_cors_grid_summary) {
    rapm_coefs_cors_grid <- .try_import_rapm_coefs_cors_grid(...)
    rapm_coefs_cors_grid_summary <-
      rapm_coefs_cors_grid %>%
      filter(src1 == "calc") %>%
      # spread(src2, value)
      group_by_at(vars(-idx_grp, -src2, -value)) %>%
      summarise(
        n = n(),
        value = sum(value)
      ) %>%
      ungroup() %>%
      arrange(desc(value))
    path_export <-
      .export_data_from_path(
        ...,
        data = rapm_coefs_cors_grid_summary,
        path_rapm_coefs_cors_grid_summary = path_rapm_coefs_cors_grid_summary
      )
    invisible(path_rapm_coefs_cors_grid_summary)
  }

.visualize_rapm_coefs_cors <-
  function(...,
           src1 = "calc",
           src2 = "szou",
           poss_min = NULL,
           path_viz_rapm_coefs_cors_grid = config$path_viz_rapm_coefs_cors_grid) {
    rapm_coefs_cors_grid <- .try_import_rapm_coefs_cors_grid(...)
    if(is.null(poss_min)) {
      poss_min <-
        rapm_coefs_cors_grid %>%
        tetidy::pull_distinctly(poss_min) %>%
        max()
      .display_info(
        glue::glue("Setting {usethis::ui_field('poss-min')} to {usethis::ui_value(poss_min)}."),
        ...
      )
    }
    # Reference: `hw5.Rmd` from ISYE 6501 class.
    # viz_summ_glmnet <-
    #   summ_glmnet %>%
    #   filter(str_detect(model_desc, "glmnet")) %>%
    #   mutate_at(vars(lambda), funs(log10(.))) %>%
    #   mutate(`Rank, By Model` = rnk_bymodel) %>%
    #   ggplot(aes(x = alpha, y = lambda, size = RMSE, color = `Rank, By Model`)) +
    #   geom_point() +
    #   geom_contour(aes(z = `Rank, By Model`)) +
    #   scale_color_gradient(low = "black", high = "#EEEEEE") +
    #   # ggplot(aes(x = alpha, y = lambda, fill = -rnk_bymodel)) +
    #   # geom_tile() +
    #   facet_wrap( ~ model_desc, scales = "free") +
    #   teplot::theme_te(legend.title = element_text()) +
    #   labs(
    #     title = "RMSE for {glmnet} Models",
    #     x = "alpha",
    #     y = "log10(lambda)"
    #   )
    # viz_summ_glmnet
    # Colors reference: https://github.com/drsimonj/corrr/blob/master/R/cor_df.R
    viz <-
      rapm_coefs_cors_grid %>%
      filter(src1 == !!src1) %>%
      filter(src2 == !!src2) %>%
      filter(poss_min == !!poss_min) %>%
      ggplot(aes(
        x = lambda_o,
        y = lambda_d,
        z = value,
        # color = value,
        size = value
      )) +
      geom_raster(aes(fill = value)) +
      # geom_contour(color = "black") +
      geom_point(color = "black") +
      scale_fill_gradientn(
        limits = c(-1, 1),
        colors = rev(c("indianred2", "white", "skyblue1"))
      ) +
      guides(size = FALSE) +
      teplot::theme_te(legend.position = "right") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      labs(
        x = "`lambda` for ORAPM",
        y = "`lambda` for DRAPM",
        title = "Sensitivity of RAPM Correlations to `lambda` for ORAPM and DRAPM",
        subtitle = glue::glue(
          "Calculated RAPM values correlated with those from ",
          "http://basketball-analytics.gitlab.io/rapm-data/."
        ),
        caption = "By Tony ElHabr"
      )
    path_export <-
      .export_data_from_path(
        ...,
        data = viz,
        path_viz_rapm_coefs_cors_grid = path_viz_rapm_coefs_cors_grid
      )
    # invisible(viz)
    viz
  }

.analyze_rapm_coefs <-
  function(...) {
    .summarise_rapm_coefs_cors(...)
    .visualize_rapm_coefs_cors(...)
  }

# This is primarily so that this step can/will be caught by the function(s)
# to analyze profiling information.
auto_analyze_rapm_coefs <-
  function(...,
           season = config$season,
           # skip = config$skip,
           verbose = config$verbose,
           export = config$export,
           backup = config$backup,
           clean = config$clean,
           n_keep = config$n_keep) {
    .analyze_rapm_coefs(
      ...,
      season = config$season,
      # skip = config$skip,
      verbose = config$verbose,
      export = config$export,
      backup = config$backup,
      clean = config$clean,
      n_keep = config$n_keep
    )
  }
