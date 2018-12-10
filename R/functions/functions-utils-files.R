
# Make this more like `httr:::compose_url()`?
# (See https://github.com/r-lib/httr/blob/af25ebd0e3b72d2dc6e1423242b94efc25bc97cc/R/url-query.r)
# Update: Done.
.SEP <- "_"
.get_path_from_format <-
  function(path_format,
           season = NULL,
           season_type = NULL,
           raw_data_source = NULL,
           sep = .SEP,
           ...) {
    ext <- tools::file_ext(path_format)
    path_format_noext <-
      tools::file_path_sans_ext(path_format)
    if (ext == "") {
     .display_error("Bad path name. Should include a recognizable file extension.",  ...)
      stop(call. = FALSE)
    }
    if (!is.null(season)) {
      season <- .validate_season(season)
    }
    if (!is.null(season_type)) {
      season_type <- .validate_season_type(season_type)
      season_type <- names(season_type)
    }
    if (!is.null(raw_data_source)) {
      raw_data_source <- .validate_raw_data_source(raw_data_source)
      raw_data_source <- names(raw_data_source)
    }
    vals <- purrr::compact(list(season, season_type, raw_data_source))
    vals <- paste0(vals, collapse = sep)
    path <- sprintf("%s%s%s.%s", path_format_noext, sep, vals, ext)
    path
  }


.create_dir_ifnecessary <-
  function(dir, verbose = .VERBOSE) {
    if(!dir.exists(dir)) {
      invisible(dir.create(dir, recursive = TRUE))
      .display_info(
        sprintf("Created %s folder at %s.", dir, Sys.time()),
        verbose = verbose
      )
    }
    invisible(dir)
  }

# path <- "data-raw/play_by_play_with_lineup/play_by_play_with_lineup_2017-18.csv"
# Add `verbose`, etc. (i.e. `backup`) to this(?).
.import_data <-
  function(path, ...) {
    # Set `verbose = FALSE` always to suppress verose `data.table::fread()` messages.
    # Also, note that both `data.table::fread()` and `rio::import()` set `call. = FALSE` in `stop()`
    # if the path does not exist.
    if(!file.exists(path)) {
      .display_error(
        sprintf("No file at %s exists!", path),
        ...
      )
      stop(call. = FALSE)
    }
    ext <- tools::file_ext(path)
    # after checking if `path` exists.
    if(ext == "csv") {
      res <-
        path %>%
        # data.table::fread(sep = ",", verbose = FALSE) %>%
        # readr::read_csv() %>%
        # rio::import(..., verbose = FALSE) %>%
        rio::import(verbose = FALSE)
    } else if(ext == "Rda") {
      load(path)
      return(invisible(NULL))
    } else {
      res <- path %>% rio::import()
    }
    # This is for model-like data.
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
           ...,
           backup = .BACKUP,
           # clean = .CLEAN,
           # n_keep = .N_KEEP,
           # verbose = .VERBOSE,
           export = .EXPORT) {
    if(!export) {
      return(invisible(NULL))
    }
    path <- .get_path_from_format(path_format, ...)
    if (backup) {
      path_backup <- .create_backup(path = path, ...)
      # .clean_backup(path = path)
    }
    path_export <-
      .export_data(
        data = data,
        path = path,
        ...
      )
    .display_info(
      sprintf("Successfully exported %s to %s.", "data", path),
      ...
    )
    invisible(path_export)
  }


.create_backup <-
  function(path,
           ...,
           file = tools::file_path_sans_ext(path),
           ext = tools::file_ext(path),
           suffix_backup = format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
           path_backup = sprintf("%s-%s.%s", file, suffix_backup, ext),
           clean = .CLEAN,
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
           n_keep = .N_KEEP,
           decreasing = TRUE,
           ...,
           dir = dirname(path),
           rgx = paste0(tools::file_path_sans_ext(basename(path)),
                               "-.*",
                               tools::file_ext(path)),
           verbose = .VERBOSE) {
    paths_like_backup <-
      list.files(
        path = dir,
        pattern = rgx,
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

