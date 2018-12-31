
.display_info <- function(x, ..., .envir = parent.frame(), verbose = TRUE) {
  # .display_msg(..., verbose = verbose, type = "info")
  if(!verbose) {
    return(invisible(NULL))
  }
  # usethis::ui_line(glue::glue("Info: {x}"), .envir = .envir)
  usethis::ui_done(glue::glue("Info: {x}"), .envir = .envir)
}

.display_warning <- function(x, ..., .envir = parent.frame()) {
  usethis::ui_warn(x, .envir = .envir)
}

.display_error <- function(x, ..., .envir = parent.frame()) {
  usethis::ui_stop(x, .envir = .envir)
}

# # Reference: `glue` package vignette.
# .sprintf_transformer <- function(text, envir) {
#   m <- regexpr(":.+$", text)
#   if (m != -1) {
#     format <- substring(regmatches(text, m), 2)
#     regmatches(text, m) <- ""
#     res <- eval(parse(text = text, keep.source = FALSE), envir)
#     do.call(sprintf, list(glue::glue("%{format}f"), res))
#   } else {
#     eval(parse(text = text, keep.source = FALSE), envir)
#   }
# }
#
# .glue_fmt <- function(..., .envir = parent.frame()) {
#   glue::glue(..., .transformer = sprintf_transformer, .envir = .envir)
# }
#
# # Testing...
# glue_fmt("x = {pi:0.4.01}")
# glue_fmt("y = {100.42:09.03}")
# sprintf("%09.03f", 100.42)
