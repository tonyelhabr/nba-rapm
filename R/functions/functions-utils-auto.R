
setup_cores <-
  function(multi_core, n_core, ...) {
    if(.Platform$OS.type != "windows") {
      if(multi_core) {
        .display_warning(
          glue::glue(
            "Ignoring {usethis::ui_code('multi_core = TRUE')} because user ",
            "system is not Windows."
          ),
          ...
        )
      }
    } else {
      if(multi_core) {
        if(n_core == 1) {
          .display_warning(
            glue::glue(
              "Not using multiple cores (even though {usethis::ui_code('multi_core = TRUE')}) ",
              "becuase {usethis::ui_code('n_core == 1')}."
            ),
            ...
          )
        } else {
          # suppressWarnings(suppressPackageStartupMessages(library("parallel")))
          library("parallel")

          n_core_avail <- parallel::detectCores()
          print("here")
          if(n_core > n_core_avail) {
            .display_error(
              glue::glue("{usethis::ui_code(n_core)} must be less than {n_core_avail}."),
              ...
            )
          }

          if((n_core != 1) & ((n_core %% 2) != 0)) {
            .display_error(
              glue::glue("{usethis::ui_code('n_core')} must be 1 or an even number (not {n_core})."),
              ...
            )
          }
          # cl <- parallel::makeCluster(n_core)
          cl <- doParallel::makeCluster(n_core)
          suppressWarnings(suppressPackageStartupMessages(library("doParallel")))
          doParallel::registerDoParallel(cl)

          .display_info(
            glue::glue("Successfully setup platform to use {n_core} cores."),
            ...
          )
          # on.exit(parallel::stopCluster(cl), add = TRUE)
        }
      }
    }
    return(invisible(NULL))
  }

auto_setup_cores <-
  purrr::partial(
    setup_cores,
    verbose = config$verbose,
    # multi_core = ifelse(interactive(), FALSE, config$multi_core),
    multi_core = config$multi_core,
    n_core = config$n_core
  )

# Reference: https://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster
desetup_cores <-
  function(...) {
    env <- foreach:::.foreachGlobals
    rm(list = ls(name = env), pos = env)
    .display_info(
      glue::glue("Successfully un-registered cores."),
      ...
    )
  }

auto_desetup_cores <-
  purrr::partial(
    desetup_cores,
    verbose = config$verbose,
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
