
.separate_lineup <-
  function(data, col, prefix = "x", suffix = 1:5, sep = "-") {
    data %>%
      separate(!!enquo(col), into = paste0(prefix, suffix), sep = sep)
  }

process_cleaned_data <-
  function(season,
           path_data_clean_format,
           ...,
           skip = .SKIP,
           verbose = .VERBOSE,
           export = .EXPORT,
           path_data_wide_o_format,
           path_data_wide_d_format,
           path_players_format,
           path_players_summary_format) {

    if(skip) {
      return(invisible(NULL))
    }

    data <-
      .import_data_from_path_format(
        path_format = path_data_clean_format,
        season = season,
        verbose = verbose
      )

    data <-
      data %>%
      .separate_lineup(lineup1, suffix = 1:5) %>%
      .separate_lineup(lineup2, suffix = 6:10) %>%
      mutate_at(vars(matches("^x")), funs(as.integer))
    data

    data <-
      data %>%
      mutate(poss_num = row_number()) %>%
      gather(dummy, id, matches("^x")) %>%
      select(-dummy) %>%
      mutate(side = if_else(is_off == 0L, "d", "o")) %>%
      mutate(xid = sprintf("%s%07d", side, as.integer(id))) %>%
      select(-is_off) %>%
      mutate(dummy = 1L)
    data
    # ... More to add
  }

do_widen_data <-
  purrr::partial(
    process_cleaned_data,
    season = args$season,
    path_data_clean_format = args$path_data_clean_format,
    skip = args$skip_intermediate,
    verbose = args$verbose,
    export = args$export_intermediate
  )
