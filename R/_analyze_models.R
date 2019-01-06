
# to update, start ----
.compare_metrics <-
  function(...,
           path_metrics_join = config$path_metrics_join,
           path_metrics_compare = config$path_metrics_compare) {
    metrics_join <-
      .import_data_from_path(
        ...,
        path = path_metrics_join
      )
  }

.create_rgx <-
  function(x) {
    paste0("(", paste(x, collapse = ")|(", sep = ""), ")") %>%
      str_replace_all("\\.", "[.]")
  }

.correlate_metrics <-
  function(...,
           path_metrics_join = config$path_metrics_join,
           src = .SRC,
           path_metrics_cors = config$path_metrics_cors) {
    metrics_join <-
      .import_data_from_path(
        ...,
        path = path_metrics_join
      )

    rgx_src <- src %>% .create_rgx()
    rapm_coefs_tidy <-
      metrics_join %>%
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

    metrics_cors <-
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
        data = metrics_cors,
        path = path_metrics_cors
      )
    metrics_cors
  }

# .SEQ_POSS_MIN <- seq(0, 5000, by = 5000)
.SEQ_POSS_MIN <- seq(0, 5000, by = 2500)
# .SEQ_LAMBDA <- seq(0, 1000, by = 1000)
.SEQ_LAMBDA <- seq(0, 1000, by = 200)

.get_metrics_cors_grid <-
  function(...,
           season = .SEASON,
           seq_poss_min = .SEQ_POSS_MIN,
           seq_lambda_o = .SEQ_LAMBDA,
           seq_lambda_d = .SEQ_LAMBDA,
           progress = TRUE,
           path_metrics_cors_grid = config$path_metrics_cors_grid) {

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
    f_prep <- memoise::memoise(auto_reshape_pbp)

    if (progress) {
      .pb <- .create_pb(total = nrow(params_grid))
    }
    metrics_cors_grid_calcs <-
      params_grid %>%
      group_by(idx_grp) %>%
      mutate(results = purrr::pmap(list(
        .season, .poss_min, .lambda_o, .lambda_d
      ),
      .f = ~ {
        .display_auto_step(glue::glue("{.season}, {.poss_min}, {.lambda_o}, {.lambda_d}"))
        f_clean(
          season = .season,
          # skip = FALSE
          skip = TRUE
        )
        f_prep(
          season = .season,
          poss_min = .poss_min,
          skip = FALSE
        )
        auto_fit_models(
          season = .season,
          skip = FALSE,
          # skip = TRUE,
          lambda_o = .lambda_o,
          lambda_d = .lambda_d,
          optimize = FALSE
        )
        auto_tidy_models(
          season = .season,
          skip = FALSE
        )
        metrics_cors <-
          .correlate_metrics(season = .season)
        # print(metrics_cors)
        if (!is.null(.pb)) {
          .pb$tick()
        }
        metrics_cors
      }))

    metrics_cors_grid <-
      metrics_cors_grid_calcs %>%
      ungroup() %>%
      unnest(results) %>%
      rename_at(vars(matches("^\\.")), funs(str_remove(., "^\\."))) %>%
      # filter(src1 == "calc" | src2 == "calc") %>%
      arrange(desc(value))
    path_export <-
      .export_data_from_path(
        ...,
        season = season,
        data = metrics_cors_grid,
        path = path_metrics_cors_grid
      )
    metrics_cors_grid
  }

.summarise_metrics_cors <-
  function(...,
           path_metrics_cors_grid_summary = config$path_metrics_cors_grid_summary) {
    metrics_cors_grid <- .try_import_metrics_cors_grid(...)
    metrics_cors_grid_summary <-
      metrics_cors_grid %>%
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
        data = metrics_cors_grid_summary,
        path_metrics_cors_grid_summary = path_metrics_cors_grid_summary
      )
    path_metrics_cors_grid_summary
  }

.visualize_metrics_cors <-
  function(...,
           src1 = "calc",
           src2 = "sz",
           poss_min = NULL,
           path_viz_metrics_cors_grid = config$path_viz_metrics_cors_grid) {
    metrics_cors_grid <- .try_import_metrics_cors_grid(...)
    if(is.null(poss_min)) {
      poss_min <-
        metrics_cors_grid %>%
        tetidy::pull_distinctly(poss_min) %>%
        max()
      .display_info(
        glue::glue("Setting `poss_min` to {crayon::yellow(poss_min)}."),
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
    viz <-
      metrics_cors_grid %>%
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
        path_viz_metrics_cors_grid = path_viz_metrics_cors_grid
      )
    viz
  }
# to update, end ----
.unnest_grid_broom <-
  function(data, value) {
    value <- ensym2(value)
    data %>%
      select(y, x, data) %>%
      unnest(data) %>%
      select(y, x, !!value)
  }

.broomify_grid <-
  function(data, f, ..., col_unnest = "fit", col_out = "data") {
    col_unnest <- ensym2(col_unnest)
    col_out <- ensym2(col_out)
    data %>%
      mutate(data = purrr::pmap(list(fit), ~f(..1))) %>%
      .unnest_grid_broom(...)
  }

# .tabularize_grid_broom <-
#   function(data, value) {
#     value <- ensym2(value)
#     data %>%
#       .unnest_grid_broom(!!value) %>%
#       spread(x, !!value)
#   }

# Note: Intended for this to work with both `broom::tidy()` and
# `broom::glance()` output formats.
.visualize_grid_broom <-
  function(data, value, upper = FALSE, limits = c(-1, 1)) {

    if(!upper) {
      data <-
        data %>%
        filter(y < x)
    }

    value <- ensym2(value)
    data %>%
      ggplot(aes(x = x, y = y, fill = !!value)) +
      geom_tile(alpha = 0.75) +
      geom_text(aes(label = round(!!value, 2), size = !!value)) +
      # Colors reference: https://github.com/drsimonj/corrr/blob/master/R/cor_df.R
      # scale_fill_gradientn(
      #   limits = limits,
      #   # colors = rev(c("indianred2", "white", "skyblue1"))
      #   colors = c("red", "white", "green")
      # ) +
      # scico::scale_fill_scico(
      #   limits = limits,
      #   direction = -1,
      #   palette = "roma"
      # ) +
      viridis::scale_fill_viridis(
        limits = limits,
        direction = 1,
        option = "E"
      ) +
      # coord_equal() +
      guides(size = FALSE) +
      teplot::theme_te(base_size = 11.5) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        # legend.position = "none",
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      labs(
        x = NULL,
        y = NULL,
        caption = "By Tony ElHabr."
      )
  }


.visualize_metrics_join_summary <-
  function(...,
           metrics_join_summary,
           season,
           path_viz_metrics_join_summary = config$path_viz_metrics_join_summary) {

    subtitle <- sprintf("%04d NBA Season", season)
    caption <-
      paste0(
        "By Tony ElHabr.\n",
        "\n",
        "Terminology:\n",
        "`apm` = Adjusted Plus-Minus (PM); ",
        "`bpm` = Box Score PM; ",
        "`rapm` = Regularized Adjusted PM.\n",
        "\n",
        "`o/d` prefix = offensive/defensive;\n",
        "`both` = calculated without splitting offense and defense.\n",
        "`calc` = self-calculated (in this project);\n",
        "`nbastatr` = derived from `{nbastatR}` package (created by A. Bresler);\n",
        "`espn` = scraped from ESPN (credits to Jeremias Engelmann);\n",
        "`sz` = scraped from http://basketball-analytics.gitlab.io/rapm-data/ ",
        "website (created S. Zou)."
      )

    viz <-
      metrics_join_summary %>%
      .visualize_grid_broom(adj.r.squared) +
      labs(
        title = "Pair-wise % Explained (Adjusted R-Squared) of Metrics",
        subtitle = subtitle,
        caption = caption
      )

    .export_data_from_path(
      ...,
      data = viz,
      season = season,
      path = path_viz_metrics_join_summary,
      units = "in",
      height = 10,
      width = 8
    )
    viz
  }

.compare_metrics_join_summary <-
  function(...,
           path_metrics_join = config$path_metrics_join,
           path_metrics_join_summary = config$path_metrics_join_summary) {

    metrics_join <-
      .import_data_from_path(
        ...,
        path = path_metrics_join
      )

    cols_all <- metrics_join %>% names()
    cols_rank <-
      cols_all %>%
      str_subset("rank")
    cols_metric <-
      cols_all %>%
      str_subset("pm") %>%
      setdiff(cols_rank)

    grid_fmlas <-
      crossing(
        y = cols_metric,
        x = cols_metric
      )

    grid_fits <-
      grid_fmlas %>%
      filter(x != y) %>%
      mutate(fit =
               purrr::pmap(list(y, x),
                           ~lm(formula = formula(paste0(..1, " ~ ", ..2, " + 0")),
                               data = metrics_join)
               )
      )
    # grid_coefs <-
    #   grid_fits %>%
    #   .broomify_grid(broom::tidy, estimate) %>%
    #   filter(!str_detect(y, "^pm")) %>%
    #   arrange(desc(estimate))
    # grid_coefs

    metrics_join_summary  <-
      grid_fits %>%
      .broomify_grid(broom::glance, adj.r.squared) %>%
      arrange(desc(adj.r.squared))


    viz_metrics_join_summary <-
      .visualize_metrics_join_summary(
        ...,
        metrics_join_summary = metrics_join_summary
      )

    .export_data_from_path(
      ...,
      data = metrics_join_summary,
      path = path_metrics_join_summary
    )
    metrics_join_summary
  }

.analyze_models <-
  function(...) {
    # .summarise_metrics_cors(...)
    # .visualize_metrics_cors(...)
    .compare_metrics_join_summary(...)
  }

# This is primarily so that this step can/will be caught by the function(s)
# to analyze profiling information.
auto_analyze_models <-
  function(...,
           season = config$season,
           skip = config$skip,
           verbose = config$verbose,
           export = config$export,
           backup = config$backup,
           clean = config$clean,
           n_keep = config$n_keep) {
    .analyze_models(
      ...,
      season = config$season,
      skip = config$skip,
      verbose = config$verbose,
      export = config$export,
      backup = config$backup,
      clean = config$clean,
      n_keep = config$n_keep
    )
  }
