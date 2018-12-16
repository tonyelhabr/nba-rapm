
.convert_config_to_args_preparsed <-
  function(x,
           description = "",
           name = NULL,
           ...,
           .optional = TRUE,
           .help = "",
           .type = NULL,
           .nargs = NULL,
           .flag = NULL,
           .short = NA) {
    stopifnot(is.list(x))
    parser <-
      argparser::arg_parser(
        description = description,
        name = name
      )
    .this_if_exists_else <- function(x, nm, default) {
      if(length(x[[nm]]) == 1L) {
        return(x[[nm]])
      } else {
        return(default)
      }
    }

    # To properly implement the cli stuff, need to check intersection
    # of names in `...` and `x`, then use the name/value in `...` where there is overlap.
    for(name in names(x)) {
      arg <- x[[name]]
      default <- .this_if_exists_else(arg, "default", NULL)
      optional <- .this_if_exists_else(arg, "optional", .optional)
      if(is.null(default) & is.null(optional)) {
        # No value to work with, so dont add argument.
        next
      } else if (is.null(default)) {
        # Assume it is NOT optional, regardless of the value of `optional`.
        arg_prefix <- ""
      } else if (!optional) {
        # This covers when `optional` is explicitly stated as `FALSE`.
        arg_prefix <- ""
      } else if (optional) {
        # This covers all other cases,
        # (i.e.. `optional` is either explicitly set to `TRUE`
        # or not specified).
        arg_prefix <- "--"
      }

      help <- .this_if_exists_else(arg, "help", .help)
      # type <- typeof(arg)
      # type <- .this_if_exists_else(arg, "type", typeof(arg))
      # short <- .this_if_exists_else(arg, "short", NULL)
      type <- .this_if_exists_else(arg, "type", .type)
      nargs <- .this_if_exists_else(arg, "nargs", .nargs)
      flag <- .this_if_exists_else(arg, "flag", .flag)
      short <- .this_if_exists_else(arg, "short", .short)

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

.convert_config_to_args <-
  function(x, ...) {
    args_preparsed <- .convert_config_to_args_preparsed(x = x, ...)
    args_parsed <- argparser::parse_args(args_preparsed)
  }

.import_config <-
  function(config = NULL, ..., file) {
    if(is.null(config)) {
      stopifnot(file.exists(file))
      config <- config::get(file = file)
      # config <- config::get(..., file = file)
    }
    res <- .convert_config_to_args(..., x = config)
    res
  }

import_config <-
  function(..., file_cli = "config-cli.yml", file_static = "config-static.yml") {
    config_cli <- .import_config(..., file = file_cli)
    # IMPORTANT: Don't pass dots here!
    config_static <- .import_config(file = file_static)
    res <-
      config::merge(
        config_cli,
        config_static
      )
  }
