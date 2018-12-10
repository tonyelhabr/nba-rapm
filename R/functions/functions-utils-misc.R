
# select_one_of <-
#   function(data, cols, drop = FALSE) {
#
#     .cols <- names(data)
#
#     if(!drop) {
#       cols_in <- intersect(cols, .cols)
#       cols_nin <- setdiff(.cols, cols_order)
#       cols_fct <- factor(.cols, levels = c(cols_in, cols_nin))
#     } else {
#       cols_fct <- factor(cols, levels = cols)
#     }
#     dplyr::select(data, tidyselect::one_of(levels(cols_fct)))
#   }

.try_skip <-
  function(skip,
           season,
           path_format_deps,
           path_format_reqs = NULL,
           ...,
           verbose = .VERBOSE,
           call_name = NULL,
           safe = TRUE) {
    if (is.null(call_name)) {
      call_name <- "function"
    }
    if (!skip) {
      # return(invisible(FALSE))
      if(!is.null(path_format_reqs)) {
        path_reqs_exist <-
          purrr::map_lgl(
            path_format_reqs,
            ~.get_path_from_format(
              path_format = .x,
              season = season
            ) %>%
              file.exists()
          )
        if(all(path_reqs_exist)) {
          msg <- sprintf("Could skip %s since all required input files exist.", call_name)
        } else {
          msg <-
            sprintf(
              paste0(
                "Would not be able to skip %s anyways (if `skip = TRUE` ",
                "were true) since not all required input files exist."
              ),
              call_name
            )
        }
        .display_info(msg, verbose = verbose)
      }
      return(invisible(FALSE))
    }

    for (path_format in path_format_deps) {
      path <-
        .get_path_from_format(path_format = path_format, season = season)
      if (!file.exist(path)) {

        msg <-
          sprintf(
            "Can't skip %s because up-stream file dependency %s does not exist!",
            call_name,
            path
          )
        if (safe) {
          msg <- sprintf("%s\nOver-ruling `skip = TRUE` and continuing.", msg)
          .display_warning(msg, verbose = verbose)
          return(invisible(FALSE))
        } else {
          .display_error(msg, verbose = verbose)
          stop(call. = FALSE)
        }
      }
    }
    skip
  }
