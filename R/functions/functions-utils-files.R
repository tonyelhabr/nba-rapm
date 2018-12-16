

.SEP <- "_"
.get_path_from <-
  function(...,
           path,
           season,
           # season_type = NULL,
           sep = .SEP) {
    ext <- tools::file_ext(path)
    path_noext <-
      tools::file_path_sans_ext(path)
    if (ext == "") {
     .display_error(
       glue::glue(
         "Bad path name ({usethis::ui_path(path)}). Should include a recognizable file extension."
       ),
       ...
     )
      stop(call. = FALSE)
    }
    # Ignore `season_type` for now.
    # if (!is.null(season_type)) {
    #   .validate_season_type(season_type)
    # }
    # basename_suffix <- purrr::compact(list(season, season_type))
    .validate_season(season)
    basename_suffix <- purrr::compact(list(season))
    # Check that at least one is non-`NULL`.
    if(length(basename_suffix) > 0) {
      basename_suffix <- paste0(sep, paste0(basename_suffix, collapse = sep))
    } else {
      basename_suffix <- ""
    }
    path <- glue::glue("{path_noext}{basename_suffix}.{ext}")
    path
  }


.create_dir_ifnecessary <-
  function(dir, ...) {
    if(!dir.exists(dir)) {
      invisible(dir.create(dir, recursive = TRUE))
      .display_info(
        glue::glue("Created {usethis::ui_path(dir)} at {Sys.time()}."),
        ...
      )
    }
    invisible(dir)
  }

# path <- "data-raw/play_by_play_with_lineup/play_by_play_with_lineup_2017-18.csv"
# Add `verbose`, etc. (i.e. `backup`) to this(?).
.import_data <-
  function(..., path) {
    ext <- tools::file_ext(path)

    if(ext == "csv") {
      # Set `verbose = FALSE` always to suppress verBose `data.table::fread()` messages.
      # Also, note that both `data.table::fread()` and `rio::import()` set `call. = FALSE` in `stop()`
      # if the path does not exist.
      # res <- data.table::fread(file = path, sep = ",", verbose = FALSE)
      # readr::read_csv(file = path, ...)
      # res <- rio::import(..., verbose = FALSE)
      res <- rio::import(file = path, verbose = FALSE)
    # } else if(str_detect(ext, "^[R|r]") {
    } else if(ext == "Rda") {
      load(path)
      return(invisible(NULL))
    } else {
      # Is this necessary ? Can't it be captured by a regular expression for file
      # extensions beginning with [R|r]?
      res <- rio::import(file = path)
    }
    # This is an early-exit for model-like data.
    if(!any("data.frame" == class(res))) {
      return(res)
    }
    res %>%
      tibble::as_tibble() %>%
      janitor::clean_names()
  }

.import_data_from_path <-
  function(...,
           path,
           # `import` is only included here in order to be analogous with `export`
           # for `.export_*()`. In reality, `skip` is used before this function
           # is ever called, so `import` is irrelevant.
           import = TRUE) {
    if(!import) {
      return(invisible(NULL))
    }
    # dots <- list(...)
    # browser()
    path <- .get_path_from(..., path = path)
    if(!file.exists(path)) {
      .display_error(
        glue::glue("No file at {usethis::ui_path(path)} exists."),
        ...
      )
      # stop(call. = FALSE)
      return(invisible(NULL))
    }
    data <- .import_data(..., path = path)
    .display_info(
      glue::glue("Successfully imported data from {usethis::ui_path(path)}."),
      ...
    )
    invisible(data)
  }

.export_data <-
  function(..., data, path) {
    # path_export <- rio::export(data, path, ...)
    # See `.import_data()` for the reasoning for setting `verbose = FALSE` here.
    path_export <- rio::export(x = data, file = path)
    invisible(path_export)
  }


.export_data_from_path <-
  function(data,
           path,
           ...,
           backup = .BACKUP,
           export = .EXPORT) {
    if(!export) {
      return(invisible(NULL))
    }
    path <- .get_path_from(..., path = path)
    if (backup) {
      path_backup <- .create_backup(..., apath = path)
    }
    .create_dir_ifnecessary(..., dir = dirname(path))
    path_export <-
      .export_data(
        ...,
        data = data,
        path = path
      )
    .display_info(
      glue::glue("Successfully exported data to {usethis::ui_path(path)}."),
      ...
    )
    invisible(path_export)
  }


.create_backup <-
  function(path,
           ...,
           file = tools::file_path_sans_ext(path),
           ext = tools::file_ext(path),
           suffix_backup = format(Sys.time(), "%Y%m%d%H%M%S"),
           path_backup = sprintf("%s-%s.%s", file, suffix_backup, ext),
           clean = .CLEAN) {
    if (!file.exists(path)) {
      .display_info(
        glue::glue(
          "Backup file at {path_backup} cannot be created because file to copy at {usethis::ui_path(path)} cannot be found."
        ),
        ...
      )
      return(invisble(path_backup))
    }

    if (file.exists(path_backup)) {
      .display_error(
        glue::glue(
          "Backup file at {path_backup} already exists. Are you sure you want to overwrite it?"
          ),
        ...
      )
      return(invisible(NULL))
    }
    invisible(file.copy(from = path, to = path_backup))
    .display_info(
      glue::glue(
        "Backed up file at {path_backup} before exporting data to {usethis::ui_path(path)}."
        ),
      ...
    )
    if(clean) {
      .clean_backup(..., path = path)
    }
    invisible(path_backup)
  }

.clean_backup <-
  function(path,
           ...,
           n_keep = .N_KEEP,
           dir = dirname(path),
           rgx = paste0(tools::file_path_sans_ext(basename(path)),"-.*", tools::file_ext(path))) {
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
          glue::glue("No backup files to delete."),
          ...
        )
        return(path)
      }

      .display_info(
        glue::glue(
            "Number of backup files ({sprintf('%.0f', n)}) is less than ",
            "`n_keep` ({sprintf('%.0f', n_keep)}), ",
            "so not deleting any backup files."
          ),
        ...
      )
      return(path)
    }
    paths_to_keep <-
      sort(paths_like_backup, decreasing = TRUE)[1L:n_keep]
    paths_to_delete <- setdiff(paths_like_backup, paths_to_keep)
    invisible(sapply(
      paths_to_delete,
      unlink,
      recursive = TRUE,
      force = TRUE
    ))

    .display_info(
      glue::glue("Deleted {sprintf('%.0f', length(paths_to_delete))} backup files at {Sys.time()}."),
      ...
    )
    invisible(path)
  }

