
# Command with my setup is C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R"

# setup-general  ---------------------------------------------------------------
# Hate having to set directory explicitly, but this prevents
# failure when running from command line.
wd <- "C:/Users/aelhabr/Documents/projects/nba-rapm"
# if(!dir.exists(wd)) {
#   stop("Directory does not exist!", call. = FALSE)
# }
# setwd(wd)

# command line setup -----------------------------------------------------------
library("argparser")
p <-
  argparser::arg_parser(
    description =
      paste0(
        "A script to calculate NBA RAPM. Uses R for data processing."
      ),
    name = "NBA RAPM Script"
  )

p <-
  argparser::add_argument(
    p,
    arg = "--dir",
    default = wd,
    help = "Working directory. MUST exist.",
    type = "character",
    flag = FALSE
  )

input <- "rapm_data.csv"
p <-
  argparser::add_argument(
    p,
    arg = "--input",
    default = input,
    help = "Input file path. MUST exist.",
    type = "character",
    flag = FALSE
  )

output <- "rapm_estimates.csv"
p <-
  argparser::add_argument(
    p,
    arg = "--output",
    default = output,
    help = "Output file path. MUST exist.",
    type = "character",
    flag = FALSE
  )

p <-
  argparser::add_argument(
    p,
    arg = "--verbose",
    default = TRUE,
    help =
      paste0(
        "Boolean to display INFO messages. (Other message types are always shown.)"
      ),
    type = "boolean",
    flag = FALSE
  )

p <-
  argparser::add_argument(
    p,
    "--multi_core",
    default = TRUE,
    help =
      paste0(
        "Boolean to indicate whether or not to use multiple cores."
      ),
    type = "boolean",
    flag = FALSE
  )

p <-
  argparser::add_argument(
    p,
    arg = "--cores",
    default = 4,
    help =
      paste0(
        "Number of cores to use. Only used if --multi_core is TRUE.\n",
        "(Requires `{parallel}` package, as well as `{doParallel}` package on Windows.)"
      ),
    type = "integer",
    flag = FALSE
  )


lambda_optm_o <- 148.00302
lambda_optm_d <- 225.611373
p <-
  argparser::add_argument(
    p,
    arg = "--optimize",
    default = FALSE,
    help =
      paste0(
        "Boolean to indicate whether to calculate optimal `lambda` value for \n",
        "cross-validated ridge regression. If FALSE, uses default values \n",
        sprintf("(%f and %f for offense and defense respectively).", lambda_optm_o, lambda_optm_d)
      ),
    type = "boolean",
    flag = FALSE,
    short = "-opt"
  )
p <-
  argparser::add_argument(
    p,
    arg = "--seed",
    default = 42,
    help =
      paste0(
        "Seed to set prior to running cross-validated ride regression.\n",
        "Only used if --optimize is TRUE."
      ),
    type = "integer",
    flag = FALSE
  )

path_cache_o <- "poss-data-wide-o.rds"
path_cache_d <- "poss-data-wide-d.rds"
io_help <-
  function(which = c("import", "export"), ...) {
    if(which == "import") {
      extra <- " (i.e. from a previous run, assuming the files exist)"
    } else {
      extra <- ""
    }
    paste0(
      sprintf("Boolean to indicate whether to %s \"intermediary\" data%s.\n", which, extra),
      sprintf("If TRUE, will %s the following:\n", which),
      sprintf("- offensive possession data in \"wide\" format to %s.\n", path_cache_o),
      sprintf("- defensive possession data in \"wide\" format to %s.\n", path_cache_d)
    )
  }
p <-
  argparser::add_argument(
    p,
    arg = "--export",
    default = TRUE,
    help = io_help(which = "export"),
    type = "boolean",
    flag = FALSE,
    short = "-ex"
  )

p <-
  argparser::add_argument(
    p,
    arg = "--import",
    default = TRUE,
    help = io_help(which = "import"),
    type = "boolean",
    flag = FALSE,
    short = "-in"
  )

# p <-
#   argparser::add_argument(
#     p,
#     arg = "--help"
#   )

# command-line argument instantiation ------------------------------------------
if (interactive()) {
  message("Running this script in interactive mode! (It is not designed to do so.)")
  argv <-
    argparser::parse_args(
      p
    )
} else {
  argv <- argparser::parse_args(p)
}

# general-purpose functions ----------------------------------------------------
suppressWarnings(suppressPackageStartupMessages(library("tidyverse")))

display_msg <-
  function(..., type = c("info", "warning", "error")) {
    if(type == "info" && !argv$verbose) {
      return(invisible(NULL))
    }
    cat(sprintf("%s: %s\n", toupper(type), ...))
  }

display_info <- function(...) {
  display_msg(..., type = "info")
}

display_warning <- function(...) {
  display_msg(..., type = "warning")
}

display_error <- function(...) {
  display_msg(..., type = "error")
}

stopifnot_exist <-
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

stopifnot_exist_dir <-
  function(..., type = "dir") {
    stopifnot_exist(..., type = type)
  }

stopifnot_exist_file <-
  function(..., type = "file") {
    stopifnot_exist(..., type = type)
  }


# Straight copy-paste of `tools::file_ext()`.
file_ext <-
  function (x) {
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
  }


export_data <-
  function(data, path, ..., ext = file_ext(path)) {
    if(!argv$export) {
      return(invisible(NULL))
    }
    f <- sprintf("write_%s", ext)
    do.call(f, list(x = data, path = path))
  }

import_data <-
  function(path, ..., ext = file_ext(path)) {
    if(!argv$import) {
      return(invisible(NULL))
    }
    # f <- sprintf("readr::read_%s", ext)
    f <- sprintf("read_%s", ext)
    do.call(f, list(path = path))
    # purrr::invoke(f, list(path = path))
  }

# general run-tim info ---------------------------------------------------------
display_info(
  sprintf("`dir` is \"%s\".", argv$dir)
)
display_info(
  sprintf("`input` is \"%s\".", argv$input)
)
display_info(
  sprintf("`output` is \"%s\".", argv$output)
)

stopifnot_exist_dir(argv$dir)
setwd(argv$dir)
stopifnot_exist_file(argv$input)
stopifnot_exist_file(argv$output)

# `multi_core` parsing ---------------------------------------------------------
if(.Platform$OS.type != "windows") {
  if(argv$multi_core) {
    display_warning(
      "Ignoring `multi_core` = `TRUE` because user system is not Windows."
    )
  }
} else if(argv$multi_core) {
  suppressWarnings(suppressPackageStartupMessages(library("parallel")))
  suppressWarnings(suppressPackageStartupMessages(library("doParallel")))
  cores_avail <- parallel::detectCores()
  if(argv$cores > cores_avail) {
    display_error(
      sprintf("`cores` must be less than %d.", cores_avail)
    )
    stop(call. = FALSE)
  }
  if((argv$cores != 1) & ((argv$cores %% 2) != 0)) {
    display_error(
      sprintf("`cores` must be 1 or an even number (not %d).", argv$cores)
    )
    stop(call. = FALSE)
  }
  cl <- parallel::makeCluster(argv$cores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)
}

# data-munging -----------------------------------------------------------------
# Import raw data and process it if cached data paths do not exist.
# Otherwise, just import the cached data.
cond_import <-
  (argv$import & file.exists(path_cache_o) & file.exists(path_cache_d))
# print(cond_import)

if(!cond_import) {

  if(argv$import) {
    import_poss_data_warn <-
      function(path, ..., side = c("o", "d")) {
        if(!file.exists(path)) {
          display_warning(
            sprintf("Can't import because %srapm possession data from %s.", side, path)
          )
        }
      }
    import_poss_data_warn(path = path_cache_o, side = "o")
    import_poss_data_warn(path = path_cache_d, side = "d")
  }

  display_info(
    sprintf("It may take some time to process the data...")
  )

  # Overwrite same variable name (`data`) in order to minimize memory usage.
  data <-
    argv$input %>%
    read_csv() %>%
    rename_all(tolower)

  separate_rapm_data <-
    function(data, col, prefix = "x", suffix = 1L:5L) {
      data %>%
        separate(!!enquo(col), into = paste0(prefix, suffix), sep = "\\.")
    }

  # Separate the `concatlineup*` columns.
  data <-
    data %>%
    mutate_at(vars(matches("concatlineup")), funs(str_replace_all(., "^\\.|\\.$", ""))) %>%
    separate_rapm_data(concatlineup, suffix = 1L:5L) %>%
    separate_rapm_data(concatlineup_opp, suffix = 6L:10L) %>%
    mutate_at(vars(matches("^x")), funs(as.integer))

  # Add "dummy" prefixes (`o` and `d`, i.e. "offense" and "defense") to player `id`s.
  # `o` and `d` are used in order to maintain "order".
  # (For similar reason, front-pad `id`s with "0"s).
  data <-
    data %>%
    gather(dummy, id, matches("^x")) %>%
    select(-dummy) %>%
    mutate_at(vars(ishometeam), funs(if_else(. == 0L, "d", "o"))) %>%
    mutate_at(vars(id), funs(sprintf("%s%06d", ishometeam, as.integer(.)))) %>%
    select(-ishometeam) %>%
    mutate(dummy = 1L)

  # Create wide data sets for the two prefixes---`o` and `d`.
  create_data_wide <-
    function(prefix = c("o", "d"), path) {

      display_info(
        sprintf("Trying to export \"%s\" possession data to %s.", prefix, path)
      )
      res <-
        data %>%
        filter(str_detect(id, sprintf("^%s", prefix))) %>%
        spread(id, dummy, fill = 0L) %>%
        select(-gameid, -overallpossnum) %>%
        mutate(poss = 1L) %>%
        group_by_at(vars(-pts, -poss)) %>%
        summarise_at(vars(pts, poss), funs(sum)) %>%
        ungroup() %>%
        select(pts, poss, everything()) %>%
        filter(poss > 1) %>%
        filter(pts > 0) %>%
        mutate(pts = 100 * pts / poss) %>%
        select(-poss)
      # if(argv$export) {
      #   res %>% write_rds(path)
      # }
      res %>% export_data(path)
      res
    }
  data_wide_o <- create_data_wide(prefix = "o", path = path_cache_o)
  data_wide_d <- create_data_wide(prefix = "d", path = path_cache_d)

} else {

  # Import data only if these were not created in the current run/session.
  import_poss_data_info <-
    function(path, ..., side = c("o", "d")) {
      display_info(
          sprintf("Importing %srapm possession data from \"%s\".", side, path)
        )
      path %>% import_data()
    }
  data_wide_o <- import_poss_data_info(path = path_cache_o, side = "o")
  data_wide_d <- import_poss_data_info(path = path_cache_d, side = "d")
}

# glmnet-setup -----------------------------------------------------------------
suppressWarnings(suppressPackageStartupMessages(library("glmnet")))
# This formula will always be used.
fmla <- formula(pts ~ .)

# Create distinct `x` and `y` data sets for each of the `o` and `d` prefixes.
# (These are used regardless of the value of `optimize`, so do this before the
# `optimize` logic.
create_x_glmnet <-
  function(data_wide, ...) {
    fmla %>%
      model.matrix(data_wide)
  }
create_y_glmnet <-
  function(data_wide, ...) {
    data_wide %>%
      pull(pts)
  }

x_glmnet_o <-
  data_wide_o %>%
  create_x_glmnet()
y_glmnet_o <-
  data_wide_o %>%
  create_y_glmnet()
x_glmnet_d <-
  data_wide_d %>%
  create_x_glmnet()
y_glmnet_d <-
  data_wide_d %>%
  create_y_glmnet()

# optimize ---------------------------------------------------------------------
# Choose the optimal lambda values. (Given the provided data, the default
# values are specified according to the values found "offline".)
if(argv$optimize) {
  display_info(
    sprintf("It may take some time to optimize...")
  )

  get_lambda_optm <-
    function(x, y, ..., side = c("o", "d")) {
      set.seed(argv$seed)
      fit_glmnet_cv <-
        glmnet::cv.glmnet(
          parallel = TRUE,
          x = x,
          y = y,
          alpha = 0
        )

      display_info(
        sprintf("Found %f to be the optimal `lambda` for %srapm.", fit_glmnet_cv$lambda.min, side)
      )
      fit_glmnet_cv$lambda.min
    }
  lambda_optm_o <-
    get_lambda_optm(
      side = "o",
      x = x_glmnet_o,
      y = y_glmnet_o
    )
  lambda_optm_d <-
    get_lambda_optm(
      side = "d",
      x = x_glmnet_d,
      y = y_glmnet_d
    )
}

# create_estimates -------------------------------------------------------------
# Estimate the rapm values for each `id`.
create_estimates <-
  function(x, y, lambda, ..., path) {
    fit_glmnet <-
      glmnet::glmnet(
        intercept = TRUE,
        x = x,
        y = y,
        alpha = 0,
        lambda = lambda
      )

    estimates <-
      fit_glmnet %>%
      broom::tidy() %>%
      filter(term != "(Intercept)") %>%
      mutate_at(vars(term), funs(str_replace(., "^.", ""))) %>%
      mutate_at(
        vars(term),
        funs(str_replace_all(., "^[0]+", "") %>% as.integer())
      ) %>%
      arrange(desc(estimate)) %>%
      select(id = term, rapm = estimate)

    estimates
  }

estimates_o <-
  create_estimates(
    x = x_glmnet_o,
    y = y_glmnet_o,
    lambda = lambda_optm_o
  )

estimates_d <-
  create_estimates(
    x = x_glmnet_d,
    y = y_glmnet_d,
    lambda = lambda_optm_d
  )

# Combine the `o` and `d` estimates.
estimates <-
  bind_rows(
    estimates_o %>% mutate(prefix = "o"),
    estimates_d %>% mutate(prefix = "d")
  ) %>%
  spread(prefix, rapm) %>%
  rename_at(vars(o, d), funs(paste0(., "rapm"))) %>%
  mutate(rapm = orapm + drapm) %>%
  arrange(desc(rapm))
# estimates

estimates %>% export_data(argv$output)
display_info(
  sprintf("Exported final rapm estimates to \"%s\".", argv$output)
)

if(exists("cl")) {
  # closeAllConnections()
  # doParallel::stopImplicitCluster()
  parallel::stopCluster(cl)
}
