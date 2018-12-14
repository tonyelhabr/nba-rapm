
.display_msg <-
  function(msg, ..., verbose = .VERBOSE, type = c("info", "warning", "error")) {
    if(type == "info" && !verbose) {
      return(invisible(NULL))
    }
    # cat(sprintf("%s: %s\n", toupper(type), ...))
    # msg <- paste0(..., collapse = "")
    cat(sprintf("%s: %s\n", toupper(type), msg))
  }

.display_info <- function(..., verbose = TRUE) {
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

# Reference: `glue` package vignette.
.sprintf_transformer <- function(text, envir) {
  m <- regexpr(":.+$", text)
  if (m != -1) {
    format <- substring(regmatches(text, m), 2)
    regmatches(text, m) <- ""
    res <- eval(parse(text = text, keep.source = FALSE), envir)
    do.call(sprintf, list(glue::glue("%{format}f"), res))
  } else {
    eval(parse(text = text, keep.source = FALSE), envir)
  }
}

.glue_fmt <- function(..., .envir = parent.frame()) {
  glue::glue(..., .transformer = sprintf_transformer, .envir = .envir)
}
# glue_fmt("x = {pi:0.4.01}")
# glue_fmt("y = {100.42:09.03}")
# sprintf("%09.03f", 100.42)
