
# test intersection of ellipses and given arguments ----
f <- function(...) {
  dots <- list(...)
  x <- list(a = 1, b = 2, c = 3, d = 4)
  # browser()
  # u <- intersect(names(dots), names(x))
  for(name in names(x)) {
    arg <- x[[name]]
    if(length(dots) > 0) {
      if(name %in% names(dots)) {
        arg <- dots[[name]]
        dots[[name]] <- NULL
      }
    } else {
      print("no more dots")
    }
    message(name, ": ", arg)
  }
}
# f()
f(a = 3, b = 4)

# from argparser docs ----
library(argparser, quietly=TRUE)

p <- arg_parser("pi")
p <-
  add_argument(
    p, "--digits",
    help = "number of significant digits to print",
    default = 7
  )

## Not run:
# If arguments are passed from the command line,
# then we would use the following:
argv <- parse_args(p)

## End(Not run)

# For testing purposes, we can pass a character vector:
if(interactive()) {
  argv <- parse_args(p, c("-d", "30"))
} else {
  argv <- parse_args(p)
}
# Now, the script runs based on the passed arguments
digits <- if (argv$digits > 22) 22 else argv$digits
print(digits)
print(pi, digits=digits)
