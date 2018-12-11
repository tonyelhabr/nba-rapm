
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
           path_deps,
           path_reqs = NULL,
           ...,
           verbose = .VERBOSE,
           call_name = NULL,
           safe = TRUE) {
    if (is.null(call_name)) {
      call_name <- "function"
    }
    if (!skip) {
      # return(invisible(FALSE))
      if(!is.null(path_reqs)) {
        path_reqs_exist <-
          purrr::map_lgl(
            path_reqs,
            ~.get_path_from(
              path = .x,
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

    for (path in path_deps) {
      path <-
        .get_path_from(path = path, season = season)
      if (!file.exist(path)) {

        msg <-
          sprintf(
            "Cant skip %s because up-stream file dependency %s does not exist!",
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
