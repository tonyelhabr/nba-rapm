
.get_proj_profile <-
  function(..., path_src = "R/_main.R", path) {
    stopifnot(file.exists(path_src))
    pd <- proftools::profileExpr(source(path_src))
    path_export <- readr::write_rds(x = pd, path = path)
    # path_export <-
    #   .export_data_from_path(
    #     ...,
    #     season = NULL,
    #     validate = FALSE,
    #     data = pd,
    #     path = path
    #   )
    pd
  }

.try_import_proj_profile <-
  function(...) {
    .try_import_thing(
      ...,
      validate = FALSE, # Because this is not associated with a season.
      f_get = .get_proj_profile,
      path = config$path_proj_profile
    )
  }


.extract_proj_profile <-
  function(...) {
    pd <- .try_import_proj_profile(...)
    funcs <- lsf.str(all.names = TRUE, envir = .GlobalEnv) %>% c()
    rgx_funcs <- funcs %>% .create_rgx()

    pd_arr <-
      pd %>%
      proftools:::fgData(colormap = NULL, reorder = "time") %>%
      as_tibble(rownames = "idx_time") %>%
      mutate_at(vars(idx_time), funs(as.integer)) %>%
      arrange(left, bottom, right, top)

    pd_arr_filt <-
      pd_arr %>%
      filter(str_detect(label, rgx_funcs))

    suppressMessages(
      proj_funcs <-
        pd_arr_filt %>%
        left_join(
          pd_arr_filt %>%
            filter(str_detect(label, "^auto_")) %>%
            mutate(grp = str_remove(label, "^auto_"), idx_grp = row_number())
        ) %>%
        fill(grp) %>%
        fill(idx_grp) %>%
        mutate(idx = row_number()) %>%
        mutate_at(vars(grp), funs(forcats::fct_inorder)) %>%
        group_by(idx_grp) %>%
        mutate(idx_grp_intra = row_number()) %>%
        ungroup() %>%
        select(
          idx,
          idx_grp_inter = idx_grp,
          idx_grp_intra,
          func_grp = grp,
          func = label
        )
    )
    proj_funcs
  }

visualize_proj_profile <-
  function(...,
           path_proj_profile = config$path_viz_proj_profile) {
  proj_funcs <- .extract_proj_profile(...)
  n_func_grps <- nrow(proj_funcs)
  # # Maybe `cli::tree()` using something like...
  # proj_funcs %>%
  #   select(func_grp, func) %>%
  #   filter(func_grp == ".clean_pbp") %>%
  #   cli::tree(root = ".clean_pbp")
  suppressWarnings(suppressPackageStartupMessages(library("ggrepel")))
  viz <-
    proj_funcs %>%
    ggplot(aes(x = idx, y = idx_grp_intra, color = func_grp)) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = func), hjust = 0, force = 0.1, show.legend = FALSE) +
    scale_colour_brewer(palette = "Set1") +
    guides(color = guide_legend(title = 'Function "Group"', override.aes = list(size = 3))) +
    xlim(-1, n_func_grps + 2) +
    teplot::theme_te(legend.title = element_text()) +
    theme(
      legend.text = element_text(size = 11),
      legend.position = "bottom",
      axis.text = element_blank(),
    ) +
    labs(
      title = "Execution Order of Custom Functions in Project",
      # x = 'Function "Group"',
      x = NULL,
      y = 'Call Stack Order Within Function "Group"'
    )
  path_export <-
    .export_data_from_path(
      ...,
      season = NULL,
      validate = FALSE,
      data = viz,
      path = path_proj_profile,
      units = "in",
      height = 7,
      width = 9
    )
  viz
}
