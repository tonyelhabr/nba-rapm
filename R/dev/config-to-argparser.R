
if(interactive()) {
  # Only use `config$yaml`.
  # config$yaml %>% flatten()
  f_keep <- function(x) { names(x) == "arg" }
  config$yaml %>% keep(.p = f_keep)
}

p <- argparser::arg_parser(
  description = "Description",
  name = "Name"
)
for(x in names(config)) {
  message(x)
  message(class(x))
  # message(names(x))
  message(config[x])
  p <-
    argparser::add_argument(
      p,
      arg = paste0("--", x),
      help = "", # deparse(substitute(x))
      default = config[x]
    )
}
argparser::parse_args(p)
# p <-
#   argparser::add_argument(
#     p,
#     arg = config$path_game_summary_raw_format,
#     help = "A help message.",
#     default = config["path_game_summary_raw_format"]
#   )
x <- rerun(5, a = rbernoulli(1), b = sample(10))
x
x %>% keep("a")
x %>% discard("a")
x %>% flatten() %>% keep("a")
# l <- config$yaml %>% unlist(recursive = FALSE)
# l %>% names()
# l[grepl("format", names(l))]
# l %>% str_subset("arg")

.split_col_at <-
  function(data, ..., col, rgx, = "\\.") {
    stopifnot(is.character(col), any(col %in% names(data)))
    data %>%
      pull(!!col) %>%
      str_split(rgx) %>%
      map_dbl(~length(.)) %>%
      max()
  }

.convert_yaml_argv_to_config <-
  function(..., yaml = config$yaml) {
    data_raw <-
      yml %>%
      unlist() %>%
      enframe()


  }
config$yaml %>% yaml::write_yaml("temp.yml")
yml <- yaml::read_yaml("temp.yml")
yml
yml %>% modify_depth(3, as.character())
yml %>% unlist() %>% enframe()
