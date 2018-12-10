
.display_msg <-
  function(msg, ..., verbose = .VERBOSE, type = c("info", "warning", "error")) {
    if(type == "info" && !verbose) {
      return(invisible(NULL))
    }
    # cat(sprintf("%s: %s\n", toupper(type), ...))
    # msg <- paste0(..., collapse = "")
    cat(sprintf("%s: %s\n", toupper(type), msg))
  }

.display_info <- function(verbose = TRUE, ...) {
  .display_msg(..., verbose = verbose, type = "info")
}

.display_warning <- function(...) {
  .display_msg(..., type = "warning")
  # warning(call. = FALSE)
}

.display_error <- function(...) {
  .display_msg(..., type = "error")
  # stop(call. = FALSE)
}
