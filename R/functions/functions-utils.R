
# config ----
.convert_config_to_args_preparsed <-
  function(x, description = "A default description.", name = NULL, .optional = TRUE, .help = "") {
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

convert_config_to_args <-
  function(...) {
    args_preparsed <- .convert_config_to_args_preparsed(...)
    args_parsed <- argparser::parse_args(args_preparsed)
  }

# files ----
# Make this more like `httr::build_url()`?
.get_path_from_format <- function(path_format, season) {
  path <- sprintf(path_format, season)
}

# path <- "data-raw/play_by_play_with_lineup/play_by_play_with_lineup_2017-18.csv"
# Add `verbose`, etc. (i.e. `backup`) to this(?).
.import_data <-
  function(path, ...) {
    path %>%
      # data.table::fread(sep = ",") %>%
      # readr::read_csv() %>%
      rio::import(...) %>%
      tibble::as_tibble()
  }

.import_data_from_path_format <-
  function(path_format, season, ...) {
    path <- .get_path_from_format(path_format, season)
    data <- .import_data(path = path, ...)
    display_msg(
      sprintf("Successfully imported data from `%s`.", path), verbose = verbose
    )
    data
  }

.export_data <-
  function(data, path, ...) {
      path_export <- rio::export(data, path, ...)
      invisible(path_export)
  }

.import_data_from_path_format <-
  function(data, path_format, season, ...) {
    path <- .get_path_from_format(path_format, season)
    .export_data(data = data, path = path, ...)
  }


.stopifnot_exist <-
  function(path, ..., type = c("file", "dir")) {
    if(type == "file") {
      if(file.exists(path)) {
        return(invisible(NULL))
      }
    } else if (type == "dir") {
      if(dir.exists(path)) {
        return(invisible(NULL))
      }
    }
    display_error(sprintf("`%s` does not exist!", path))
    stop(call. = FALSE)
  }

.stopifnot_exist_dir <-
  function(..., type = "dir") {
    stopifnot_exist(..., type = type)
  }

.stopifnot_exist_file <-
  function(..., type = "file") {
    stopifnot_exist(..., type = type)
  }


# # Straight copy-paste of `tools::file_ext()`.
# .file_ext <-
#   function (x) {
#     pos <- regexpr("\\.([[:alnum:]]+)$", x)
#     ifelse(pos > -1L, substring(x, pos + 1L), "")
#   }


# logging ----
.display_msg <-
  function(..., verbose = TRUE, type = c("info", "warning", "error")) {
    if(type == "info" && !verbose) {
      return(invisible(NULL))
    }
    # cat(sprintf("%s: %s\n", toupper(type), ...))
    msg <- paste0(..., collapse = "")
    cat(sprintf("%s: %s\n", toupper(type), msg))
  }

display_info <- function(verbose = TRUE, ...) {
  .display_msg(..., verbose = verbose, type = "info")
}

display_warning <- function(...) {
  .display_msg(..., type = "warning")
}

display_error <- function(...) {
  .display_msg(..., type = "error")
}


# setup ----
setup_cores <-
  function(multi_core, n_core, ..., verbose = .VERBOSE) {
    if(.Platform$OS.type != "windows") {
      if(multi_core) {
        display_warning(
          "Ignoring `multi_core` = `TRUE` because user system is not Windows."
        )
      }
    } else if(multi_core) {
      suppressWarnings(suppressPackageStartupMessages(library("parallel")))
      suppressWarnings(suppressPackageStartupMessages(library("doParallel")))
      n_core_avail <- parallel::detect_core()
      if(n_core > n_core_avail) {
        display_error(
          sprintf("`n_core` must be less than %d.", n_core_avail)
        )
        stop(call. = FALSE)
      }
      if((n_core != 1) & ((n_core %% 2) != 0)) {
        display_error(
          sprintf("`n_core` must be 1 or an even number (not %d).", n_core)
        )
        stop(call. = FALSE)
      }
      cl <- parallel::makeCluster(n_core)
      doParallel::registerDoParallel(cl)
      on.exit(parallel::stopCluster(cl), add = TRUE)
    }
  }

do_setup_cores <-
  purrr::partial(
    setup_cores,
    multi_core, args$multi_core,
    n_core = args$n_core
  )

# TODO: Call `display_msg()` here?
pre_auto <-
  function(...) {
    message(rep("*", 80L))
    msg <- sprintf("Started script at %s.", Sys.time())
    message(msg)
  }

post_auto <-
  function(...) {
    msg <- sprintf("Finished script at %s.", Sys.time())
    message(msg)
    message(rep("*", 80L))
  }
