
to_argparser <-
  function(x, description, name = NULL, .optional = TRUE, .help = "") {
    stopifnot(is.list(x))
    parser <-
      argparser::arg_parser(
        description = description,
        name = name
      )

    this_if_exists_else <- function(x, nm, default) {
      if(length(x[[nm]]) == 1L) {
        return(x[[nm]])
      } else {
        return(default)
      }
    }

    for(name in names(x)) {
      arg <- x[[name]]
      default <- this_if_exists_else(arg, "default", NULL)
      optional <- this_if_exists_else(arg, "optional", .optional)
      if(is.null(default) & is.null(optional)) {
        # No value to work with, so don't add argument.
        next
      } else if (is.null(default)) {
        # Assume it is NOT optional, regardless of the value of `optional`.
        arg_prefix <- ""
      } else if (!optional) {
        # This covers when `optional` is explicitly stated as `FALSE`.
        arg_prefix <- ""
      } else if (optional) {
        # This covers all other cases, (I.E. `optional` is either explicitly set to `TRUE`
        # or not specified).
        arg_prefix <- "--"
      }

      help <- this_if_exists_else(arg, "help", .help) # "No \"help\" message available.")
      # type <- typeof(arg)
      # type <- this_if_exists_else(arg, "type", typeof(arg))
      type <- this_if_exists_else(arg, "type", NULL)
      nargs <- this_if_exists_else(arg, "nargs", NULL)
      flag <- this_if_exists_else(arg, "flag", NULL)
      short <- this_if_exists_else(arg, "short", NULL)

      parser <-
        argparser::add_argument(
          parser = parser,
          arg = paste0(arg_prefix, name),
          help = help,
          default = default,
          type = type,
          nargs = nargs,
          flag = flag,
          short = short
        )
    }
    parser
  }

# p <- to_argparser(config, description = "A description.", name = "A name")
# p
# argparser::parse_args(p)
