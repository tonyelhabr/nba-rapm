
# twitterbot project ----
.preprocess_compare_n_rows <-
  function(message, warn, stop, ...) {

    n_msg <- sum(c(message, warn, stop), na.rm = TRUE)
    if(n_msg < 1) {
      return(NULL)
    }

    if(n_msg > 1) {
      msg <- paste0("Only one of `message`, `warn`, and `stop` can be set to `TRUE`.")
      stop(msg, call. = FALSE)
    }

    invisible(TRUE)
  }

.compare_n_row_eq <-
  function(data1 = NULL,
           data2 = NULL,
           n1 = ifelse(!is.null(data1), nrow(data1), NA_real_),
           n2 = ifelse(!is.null(data2), nrow(data1), NA_real_),
           nm1 = ifelse(!is.null(data1), deparse(substitute(data1)), "data1"),
           nm2 = ifelse(!is.null(data2), deparse(substitute(data1)), "data2"),
           message = FALSE,
           warn = FALSE,
           stop = !message) {

    .preprocess_compare_n_rows(
      message = message,
      warn = warn,
      stop = stop
    )

    if (n1 != n2) {
      msg <-
        glue::glue(
            "`{nm1}` and `{nm2}` do not have the same number of rows ",
            "({sprintf('%d', n1)} != {sprintf('%d', n2)}). ",
            "Something unexpected happened."
          )
      stop(msg, call. = FALSE)
    }
    invisible(TRUE)
  }
