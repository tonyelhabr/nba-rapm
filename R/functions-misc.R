
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
  function(...,
           skip,
           path_deps,
           path_reqs = NULL,
           call_name = NULL,
           safe = TRUE) {
    if (is.null(call_name)) {
      call_name <- "function"
    }

    if (!skip) {
      # return(invisible(FALSE))
      if(!is.null(path_reqs)) {
        # Rely on full path being creted beforehand (in order to avoid ambiguity with `side`
        # being specified).
        path_reqs_check <-
          purrr::map_chr(
            path_reqs,
            ~.get_path_from(
              path = .x,
              validate = FALSE
            )
          )
        path_reqs_exist <- path_reqs_check %>% purrr::map_lgl(file.exists)

        if(all(path_reqs_exist)) {
          # msg <- glue::glue("Could skip {call_name} since all required input files exist.")
        } else {
          .display_info(
            glue::glue(
              "You would HAVE TO skip {call_name} since not ",
              "all required input files exist."
            ),
            ...
          )
        }

      }
      return(invisible(FALSE))
    }

    for (path in path_deps) {
      if (!file.exists(path)) {
        msg <-
          glue::glue(
            "Can't skip {call_name} because up-stream file dependency at ",
            "{path} does not exist."
           )
        if (safe) {
          msg <- glue::glue(
            "{msg} Over-ruling `skip = TRUE` and continuing",
            " (because `safe = TRUE`)."
          )
          .display_warning(msg, ...)
          return(invisible(FALSE))
        } else {
          .display_error(msg, ...)
        }
      }
    }
    skip
  }


.create_pb <-
  function(total, ..., width = 80, format = "[:bar] :percent eta :eta\n") {
    progress::progress_bar$new(
      total = total,
      format = format,
      width = width
    )
  }


