
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
