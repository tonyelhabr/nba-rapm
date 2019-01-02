
# poss_side_wide <- .import_data_from_path(path = config$path_poss_wide_side, side = "o", season = 2017)
# n_poss_max <- poss_side_wide %>% pull(n_poss) %>% sort(decreasing = TRUE) %>% .[[1]]
# poss_side_long <- .import_data_from_path(path = config$path_poss_long_side, side = "o", season = 2017)
# poss_side_long %>% mutate_at(vars(dummy), funs(. / n_poss_max)) %>%.spread_poss_side()

main_auto <-
  function(...) {
    # width_old <- getOption("width")
    # options(width = 80L)
    pre_auto()
    # setup_cores_auto()
    clean_pbp_auto(
      ...,
      skip = TRUE
    )
    munge_pbp_auto(
      ...,
      skip = TRUE,
      # skip = FALSE
    )
    fit_models_auto(
      ...,
      intercept = TRUE,
      optimize = TRUE,
      # optimize = FALSE,
      # lambda_o = 200,
      # lambda_d = 500,
      skip = FALSE
    )
    res <-
      extract_coefs_auto(
        ...,
        skip = FALSE
      )
    # desetup_cores_auto()
    post_auto()
    # options(width = width_old)
    invisible(res)
  }


# # pre_auto()
# # setup_cores_auto()
#
# purrr::walk(
#   # .SEASONS[3],
#   .SEASONS,
#   .f = function(x) {
#     # clean_pbp_auto(season = x, skip = TRUE)
#     munge_pbp_auto(season = x, skip = FALSE)
#     fit_models_auto(
#       season = x,
#       skip = FALSE,
#       # skip = TRUE,
#       # optimize = TRUE
#       optimize = FALSE
#     )
#     extract_coefs_auto(season = x, skip = FALSE)
#   }
# )
#
# # desetup_cores_auto()
# # post_auto()

.setup_cores <-
  function(..., multi_core = TRUE, n_core = 4L) {
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
          # suppressWarnings(suppressPackageStartupMessages(library("doParallel")))

          # browser()
          # library("doParallel")
          cl <- makeCluster(detectCores())

          n_core_avail <- detectCores()
          # print("here")
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
          # cl <- makeCluster(n_core)
          cl <- makeCluster(detectCores())
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

setup_cores_auto <-
  function(...,
           multi_core = ifelse(interactive(), FALSE, config$multi_core),
           # TODO: Fix this. It's causing issues (possibly because
           # "too many" clusters are being registered and not properly unregistered,
           # which can cause this to hang.
           # multi_core = config$multi_core,
           n_core = config$n_core) {
    .setup_cores(
      ...,
      multi_core = multi_core,
      n_core = n_core
    )
  }

# Reference: https://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster
.desetup_cores <-
  function(...) {
    env <- foreach:::.foreachGlobals
    rm(list = ls(name = env), pos = env)
    .display_info(
      glue::glue("Successfully un-registered cores."),
      ...
    )
  }

desetup_cores_auto <-
  function(...) {
    .desetup_cores(
      ...
    )
  }

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

