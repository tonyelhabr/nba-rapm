
# https://github.com/tidyverse/dplyr/blob/master/R/utils.r
commas <- function(...) paste0(..., collapse = ", ")

# https://github.com/tidyverse/dplyr/blob/master/R/error.R
glubort <- function(header, ..., .envir = parent.frame(), .abort = rlang::abort) {
  text <- glue::glue(..., .envir = .envir)
  if (!rlang::is_null(header)) text <- paste0(header, " ", text)
  .abort(text)
}

bad <- function(..., .envir = parent.frame()) {
  glubort(NULL, ..., .envir = parent.frame())
}

bad_args <- function(args, ..., .envir = parent.frame()) {
  glubort(fmt_args(args), ..., .envir = .envir)
}

parse_args <- function(x) {
  # convert single formula to list of length 1
  x <- unlist(list(x), recursive = FALSE)
  is_fml <- purrr::map_lgl(x, is_formula)
  x[is_fml] <- purrr::map_chr(purrr::map(x[is_fml], "[[", 2), rlang::as_string)
  unlist(x)
}

fmt_args <- function(x) {
  x <- parse_args(x)
  fmt_obj(x)
}

fmt_obj <- function(x) {
  fmt_comma(fmt_obj1(x))
}

fmt_obj1 <- function(x) {
  paste0("`", x, "`")
}

fmt_comma <- function(..., .max = 6) {
  x <- paste0(...)
  if (length(x) > .max) {
    length(x) <- .max
    x[[.max]] <- "..."
  }

  commas(x)
}

# test ----

var <- c(1, 2)
var <- "1"
# https://github.com/tidyverse/dplyr/blob/master/R/pull.R
if (!is.numeric(var) || length(var) != 1) {
  bad_args("var", "must evaluate to a single number")
}



