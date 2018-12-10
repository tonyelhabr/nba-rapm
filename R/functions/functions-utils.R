
# config ----
.convert_config_to_args_preparsed <-
  function(x, description = "A default description.", name = NULL, .optional = TRUE, .help = "") {
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

    for(name in names(x)) {
      arg <- x[[name]]
      default <- .this_if_exists_else(arg, "default", NULL)
      optional <- .this_if_exists_else(arg, "optional", .optional)
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

      help <- .this_if_exists_else(arg, "help", .help) # "No \"help\" message available.")
      # type <- typeof(arg)
      # type <- .this_if_exists_else(arg, "type", typeof(arg))
      type <- .this_if_exists_else(arg, "type", NULL)
      nargs <- .this_if_exists_else(arg, "nargs", NULL)
      flag <- .this_if_exists_else(arg, "flag", NULL)
      short <- .this_if_exists_else(arg, "short", NULL)

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
  function(...) {
    args_preparsed <- .convert_config_to_args_preparsed(...)
    args_parsed <- argparser::parse_args(args_preparsed)
  }

get_args <-
  function(config = NULL, ...) {
    if(is.null(config)) {
      config <- config::get()
    }
    .convert_config_to_args(config)
  }

# files ----
.validate_raw_data_source <-
  function(raw_data_source = .RAW_DATA_SOURCES, ...) {
    raw_data_source <- match.arg(raw_data_source)
    # .display_error(
    #   sprintf("\"%s\" is Not a valid `raw_data_source`.", raw_data_source)
    # )
    raw_data_source
  }

# Make this more like `httr::build_url()`?
.get_path_from_format <- function(path_format, season, raw_data_source = NULL) {
  stopifnot(is.numeric(season))
  # if(str_detect(path_format, "%s")) {
  #   if(!is.null(raw_data_source)) {
  #     raw_data_source <- .validate_raw_data_source(raw_data_source)
  #     if(raw_data_source == .RAW_DATA_SOURCES[1]) {
  #
  #     }
  #   } else {
  #
  #   }
  #
  #   season_suffix <- sprintf("%02d", (season + 1) %% 2000)
  #   season <- paste0(season, "-", season_suffix)
  # }

  path <- sprintf(path_format, season)
}

.create_dir_ifnecessary <-
  function(dir, verbose = .VERBOSE, execute = TRUE) {
    if(!execute) {
      return(invisible(NULL))
    }
    if(!dir.exists(dir)) {
      invisible(dir.create(dir, recursive = TRUE))
      .display_info(
        sprintf("Created %s folder at %s.", dir, Sys.time()),
        verbose = verbose
      )
    }
    invisible(dir)
  }
s

# Straight copy-paste of `tools::file_ext()`.
.file_ext <-
  function (x) {
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
  }

# path <- "data-raw/play_by_play_with_lineup/play_by_play_with_lineup_2017-18.csv"
# Add `verbose`, etc. (i.e. `backup`) to this(?).
.import_data <-
  function(path, ...) {
    # Set `verbose = FALSE` always to suppress verose `data.table::fread()` messages.
    # Also, note that both `data.table::fread()` and `rio::import()` do not
    # seem to work with `purrr::possibly()` because they call `stop()` without setting `call. = FALSE`
    # after checking if `path` exists.
    if(.file_ext(path) == "csv") {
      res <-
        path %>%
        # data.table::fread(sep = ",", verbose = FALSE) %>%
        # readr::read_csv() %>%
        # rio::import(..., verbose = FALSE) %>%
        rio::import(verbose = FALSE)
    } else {
      res <- path %>% rio::import()
    }
    if(!any("data.frame" == class(res))) {
      return(res)
    }

    res %>%
      tibble::as_tibble() %>%
      janitor::clean_names()
  }

.import_data_from_path_format <-
  function(path_format,
           season,
           ...,
           verbose = .VERBOSE,
           # `import` is only included here in order to be analogous with `export`
           # for `.export_*()`. In reality, `skip` is used before this function
           # is ever called, so `import` is irrelevant.
           import = TRUE) {
    if(!import) {
      return(invisible(NULL))
    }
    path <- .get_path_from_format(path_format, season, ...)
    if(!file.exists(path)) {
      .display_error(
        sprintf("%s does not exist.", path),
        verbose = verbose
      )
    }
    data <- .import_data(path = path, verbose = verbose, ...)
    .display_info(
      sprintf("Successfully imported %s from %s.", "data", path),
      verbose = verbose
    )
    invisible(data)
  }

.export_data <-
  function(data, path, ...) {
    # path_export <- rio::export(data, path, ...)
    # See `.import_data()` for the reasoning for setting `verbose = FALSE` here.
    path_export <- rio::export(data, path)
    invisible(path_export)
  }


.export_data_from_path_format <-
  function(data,
           path_format,
           season,
           ...,
           backup = .BACKUP,
           clean = .CLEAN,
           verbose = .VERBOSE,
           export = .EXPORT) {
    if(!export) {
      return(invisible(NULL))
    }
    path <- .get_path_from_format(path_format, season)
    if (backup) {
      path_backup <- .create_backup(path = path, verbose = verbose, ...)
      # .clean_backup(path = path)
    }
    path_export <-
      .export_data(
        data = data,
        path = path,
        verbose = verbose,
        ...
      )
    .display_info(
      sprintf("Successfully exported %s to %s.", "data", path),
      verbose = verbose
    )
    invisible(path_export)
  }

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

# tetidy package ----
# select_one_of <-
#   function(data, cols, drop = FALSE) {
#
#     .cols <- names(data)
#
#     if(!drop) {
#       cols_in <- intersect(cols, .cols)
#       cols_nin <- setdiff(.cols, cols_order)
#       cols_fct <- factor(.cols, levels = c(cols_in, cols_nin))
#     } else {
#       cols_fct <- factor(cols, levels = cols)
#     }
#     dplyr::select(data, tidyselect::one_of(levels(cols_fct)))
#   }

# setup ----
setup_cores <-
  function(multi_core, n_core, ..., verbose = .VERBOSE) {
    if(.Platform$OS.type != "windows") {
      if(multi_core) {
        .display_warning(
          "Ignoring `multi_core = TRUE` because user system is not Windows."
        )
      }
    } else {
      if(multi_core) {
        if(n_core == 1) {
          .display_warning(
            sprintf(
              paste0(
                "Not using multiple cores (even though `multi_core = TRUE`",
                "becuase `n_core == 1`.", n_core)
            ),
            verbose = verbose
          )
        } else {
          suppressWarnings(suppressPackageStartupMessages(library("parallel")))

          n_core_avail <- parallel::detectCores()
          if(n_core > n_core_avail) {
            .display_error(
              sprintf("`n_core` must be less than %d.", n_core_avail),
              verbose = verbose
            )
            stop(call. = FALSE)
          }

          if((n_core != 1) & ((n_core %% 2) != 0)) {
            .display_error(
              sprintf("`n_core` must be 1 or an even number (not %d).", n_core),
              verbose = verbose
            )
            stop(call. = FALSE)
          }
          cl <- parallel::makeCluster(n_core)
          suppressWarnings(suppressPackageStartupMessages(library("doParallel")))
          doParallel::registerDoParallel(cl)
          on.exit(parallel::stopCluster(cl), add = TRUE)
        }
      }
    }
    return(invisible(NULL))
  }

auto_setup_cores <-
  purrr::partial(
    setup_cores,
    multi_core = ifelse(interactive(), FALSE, args$multi_core),
    n_core = args$n_core,
    verbose = args$verbose
  )

# from other projects ----
# TODO: Call `.display_info()` here?
pre_auto <-
  function(..., execute = !interactive()) {
    if(!execute) {
      return(invisible(NULL))
    }
    message(rep("*", 80L))
    msg <- sprintf("Started script at %s.", Sys.time())
    message(msg)
  }

post_auto <-
  function(..., execute = !interactive()) {
    if(!execute) {
      return(invisible(NULL))
    }
    msg <- sprintf("Finished script at %s.", Sys.time())
    message(msg)
    message(rep("*", 80L))
  }

.create_backup <-
  function(path,
           ...,
           file = tools::file_path_sans_ext(path),
           ext = tools::file_ext(path),
           suffix_backup = format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
           path_backup = sprintf("%s-%s.%s", file, suffix_backup, ext),
           clean = FALSE,
           verbose = .VERBOSE) {
    if (!file.exists(path)) {
      .display_warning(
        sprintf("Backup file %s cannot be created because %s cannot be found!",
                path_backup,
                path),
        verbose = verbose
      )
      return(path_backup)
    }

    if (file.exists(path_backup)) {
      .display_error(
        sprintf("Backup file %s already exists! Are you sure you want to overwrite it?",
                path_backup),
        verbose = verbose
      )
      stop(call. = FALSE)
    }
    invisible(file.copy(from = path, to = path_backup))
    .display_info(
      sprintf("Backed up %s before exporting to %s.", path_backup, path),
      verbose = verbose
    )
    if(clean) {
      .clean_backup(path = path, verbose = verbose, ...)
    }
    invisible(path_backup)
  }

.clean_backup <-
  function(path,
           n_keep = 1L,
           decreasing = TRUE,
           ...,
           dir_backup = dirname(path),
           rgx_backup = paste0(tools::file_path_sans_ext(basename(path)),
                               "-.*",
                               tools::file_ext(path)),
           verbose = .VERBOSE) {
    paths_like_backup <-
      list.files(
        path = dir_backup,
        pattern = rgx_backup,
        recursive = FALSE,
        full.names = TRUE
      )
    n <- length(paths_like_backup)
    if (n < n_keep) {
      if (n == 0L) {
        .display_info(
          sprintf("No backup files to delete."),
          verbose = verbose
        )
        return(path)
      }

      .display_info(
        sprintf(
          paste0(
            "Number of backup files (%.0f) is less than `keep` (%.0f), ",
            "so not deleting any backup files."
          ),
          n,
          n_keep
        ),
        verbose = verbose
      )
      return(path)
    }
    paths_to_keep <-
      sort(paths_like_backup, decreasing = decreasing)[1L:n_keep]
    paths_to_delete <- setdiff(paths_like_backup, paths_to_keep)
    invisible(sapply(
      paths_to_delete,
      unlink,
      recursive = TRUE,
      force = TRUE
    ))

    .display_info(
      sprintf("Deleted %.0f backup files at %s.",
              length(paths_to_delete),
              Sys.time()),
      verbose = verbose
    )
    invisible(path)
  }


.try_skip <-
  function(skip,
           season,
           path_format_deps,
           path_format_reqs = NULL,
           ...,
           verbose = .VERBOSE,
           call_name = NULL,
           safe = TRUE) {
    if (is.null(call_name)) {
      call_name <- "function"
    }
    if (!skip) {
      # return(invisible(FALSE))
      if(!is.null(path_format_reqs)) {
        path_reqs_exist <-
          purrr::map_lgl(
            path_format_reqs,
            ~.get_path_from_format(
              path_format = .x,
              season = season
            ) %>%
              file.exists()
          )
        if(all(path_reqs_exist)) {
          msg <- sprintf("Could skip %s since all required input files exist.", call_name)
        } else {
          msg <-
            sprintf(
              paste0(
                "Would not be able to skip %s anyways (if `skip = TRUE` ",
                "were true) since not all required input files exist."
              ),
              call_name
            )
        }
        .display_info(msg, verbose = verbose)
      }
      return(invisible(FALSE))
    }

    for (path_format in path_format_deps) {
      path <-
        .get_path_from_format(path_format = path_format, season = season)
      if (!file.exist(path)) {

        msg <-
          sprintf(
            "Can't skip %s because up-stream file dependency %s does not exist!",
            call_name,
            path
          )
        if (safe) {
          msg <- sprintf("%s\nOver-ruling `skip = TRUE` and continuing.", msg)
          .display_warning(msg, verbose = verbose)
          return(invisible(FALSE))
        } else {
          .display_error(msg, verbose = verbose)
          stop(call. = FALSE)
        }
      }
    }
    skip
  }
