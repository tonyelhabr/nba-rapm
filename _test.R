
# Note: command is C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_test2.R" rapm_data.csv rapm_estimates.csv --verbose
p <-
  argparser::arg_parser(
    description = "NBA RAPM Script"
  )
p <-
  argparser::add_argument(
    p, "input",
    help = "Path to input file."
  )
p <-
  argparser::add_argument(
    p, "output",
    help = "Path to output file."
  )
p <-
  argparser::add_argument(
    p,
    "--verbose",
    help = "Flag to display INFO messages",
    flag = TRUE
  )
argv <- argparser::parse_args(p)
# argv <-
#   argparser::parse_args(
#     p,
#     c(
#       "input",
#       "rapm_data.csv",
#       "output",
#       "rapm_estimate.csv",
#       "-v"
#     )
#   )
argv <-
  argparser::parse_args(
    p,
    c(
      "input rapm_data.csv",
      "output rapm_estimate.csv",
      "-v"
    )
  )

display_msg <-
  function(..., type = c("info", "warning", "error")) {
    if(argv$verbose) {
      cat(sprintf("%s: %s\n", toupper(type), ...))
    }
  }
display_info <- function(...) {
  display_msg(..., type = "info")
}

# f <- function(x, nm = deparse(substitute(x))) {
#   # cat(sprintf("`%s` is %s.\n", gsub("argv\\$", "", nm), x))
#   cat(x)
# }
# f(argv$input)

display_info(
  sprintf("`input` path is %s.", argv$input)
)
display_info(
  sprintf("`output` path is %s.", argv$output)
)
display_info(
  sprintf("`verbose` flag is %s.", argv$verbose)
)
