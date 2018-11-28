

dir_proj <- "C:/Users/aelhabr/Documents/projects/"
if(!dir.exists(dir_proj)) {
  stop("Project directory does not exist!", call. = FALSE)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  proj <- "nba-rapm"
  wd <- file.path(dir_proj, proj)
  setwd(wd)
} else {
  message("Running this script in interactive mode! (It is not designed to do so.)")
  args <- character()
}

if (length(args) == 0) {
  args <- "default"
} else {

}

library("tidyverse")
library("parallel")
library("doParallel")
library("glmnet")

n_core <- 4
optimize <- FALSE
export <- TRUE
verbose <- TRUE

path_data_raw <- "rapm_data.csv"
path_cache_a <- "data-wide-cache-a.rds"
path_cache_b <- "data-wide-cache-b.rds"
path_terms_a <- "terms-a.csv"
path_terms_b <- "terms-b.csv"
path_terms <- "terms.csv"

lambda_optm_a <- 162.4301
lambda_optm_b <- 205.5687

# command line args validation ----
n_core_avail <- parallel::detectCores()
if(n_core > n_core_avail) {
  msg <- sprintf("`n_core` must be less than %d.", n_core_avail)
  stop(msg, call. = FALSE)
}
if((n_core != 1) & ((n_core %% 2) != 0)) {
  msg <- sprintf("`n_core` must be 1 or an even number (not %d).", n_core)
  stop(msg, call. = FALSE)
}
cl <- parallel::makeCluster(n_core)
doParallel::registerDoParallel(cl)

# Should do more validation...

# Import raw data and process it if cached data paths do not exist.
# Otherwise, just import the cached data.
if(!file.exists(path_cache_a) & !file.exists(path_cache_b)) {

  # Overwrite same variable name (`data`) in order to minimize memory usage.
  data <-
    path_data_raw %>%
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

  # Add "dummy" prefixes (`a` and `b`, i.e. "home" and "away") to player `id`s.
  # `a` and `b` are used in order to maintain "order".
  # (For similar reason, front-pad `id`s with "0"s).
  data <-
    data %>%
    gather(dummy, id, matches("^x")) %>%
    select(-dummy) %>%
    mutate_at(vars(ishometeam), funs(if_else(. == 0L, "b", "a"))) %>%
    mutate_at(vars(id), funs(sprintf("%s%06d", ishometeam, as.integer(.)))) %>%
    select(-ishometeam) %>%
    mutate(dummy = 1L)

  # Create wide data sets for the two prefixes---`a` and `b`.
  create_data_wide <-
    function(prefix = c("a", "b"), path) {

      if(verbose) {
        msg <- sprintf("Trying to cache \"%s\" data to %s.", prefix, path)
        message(msg)
      }

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
        select(-poss) %>%
        write_rds(path)
    }
  create_data_wide(prefix = "a", path = path_cache_a)
  create_data_wide(prefix = "b", path = path_cache_b)

}

# Import the cached data (regardless of whether or not it was generated in this run).
if(verbose) {
  msg <- sprintf("Reading \"a\" data from %s.", path_cache_a)
  message(msg)
}
data_wide_a <- path_cache_a %>% read_rds()
if(verbose) {
  msg <- sprintf("Reading \"b\" data from %s.", path_cache_b)
  message(msg)
}
data_wide_b <- path_cache_b %>% read_rds()

# This formula will always be used.
fmla <- formula(pts ~ .)

# Create distinct `x` and `y` data sets for each of the `a` and `b` prefixes.
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

x_glmnet_a <-
  data_wide_a %>%
  create_x_glmnet()
y_glmnet_a <-
  data_wide_a %>%
  create_y_glmnet()
x_glmnet_b <-
  data_wide_b %>%
  create_x_glmnet()
y_glmnet_b <-
  data_wide_b %>%
  create_y_glmnet()

# Choose the optimal lambda values. (Given the provided data, the default
# values are specified according to the values found "offline".)
if(optimize) {
  get_lambda_optm <-
    function(x, y, ...) {
      fit_glmnet_cv <-
        glmnet::cv.glmnet(
          parallel = TRUE,
          x = x,
          y = y,
          alpha = 0
        )
      fit_glmnet_cv$lambda.min
    }
  lambda_optm_a <-
    get_lambda_optm(
      x = x_glmnet_a,
      y = y_glmnet_a
    )
  lambda_optm_b <-
    get_lambda_optm(
      x = x_glmnet_b,
      y = y_glmnet_b
    )
}

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

    terms <-
      fit_glmnet %>%
      broom::tidy() %>%
      filter(term != "(Intercept)") %>%
      mutate_at(vars(term), funs(str_replace(., "^.", ""))) %>%
      mutate_at(vars(term), funs(str_replace_all(., "^[0]+", "") %>% as.integer())) %>%
      arrange(desc(estimate)) %>%
      select(id = term, rapm = estimate) %>%
      write_csv(path)
    if(verbose) {
      msg <- sprintf("Exported one-sided rapm estimates to %s.", path)
      message(msg)
    }
    terms
  }

create_estimates(
  x = x_glmnet_a,
  y = y_glmnet_a,
  lambda = lambda_optm_a,
  path = path_terms_a
)

create_estimates(
  x = x_glmnet_b,
  y = y_glmnet_b,
  lambda = lambda_optm_b,
  path = path_terms_b
)

terms_a <- path_terms_a %>% read_csv()
terms_b <- path_terms_b %>% read_csv()

# Combine the `a` and `b` estimates.
terms <-
  bind_rows(
    terms_a %>% mutate(prefix = "a"),
    terms_b %>% mutate(prefix = "b")
  ) %>%
  spread(prefix, rapm) %>%
  rename_at(vars(a, b), funs(paste0(., "rapm"))) %>%
  mutate(rapm = arapm + brapm) %>%
  arrange(desc(rapm))
terms

terms %>% write_csv(path_terms)
if(verbose) {
  msg <- sprintf("Exported final rapm estimates to %s.", path_terms)
  message(msg)
}
