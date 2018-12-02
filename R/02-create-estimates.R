
cl <- parallel::makeCluster(4)
doParallel::registerDoParallel(cl)

data_wide_o <-
  config$path_cache_o %>%
  teproj::import_path_cleanly()

data_wide_d <-
  config$path_cache_d %>%
  teproj::import_path_cleanly()

# Note that everything from here on down is the same, except `config` <-> `aregv`
# and `message()` <-> `display_info()`.

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

config$optimize <- FALSE
if(config$optimize) {
  message(
    sprintf("It may take some time to optimize...")
  )

  get_lambda_optm <-
    function(x, y, ..., side = c("o", "d")) {
      set.seed(config$seed)
      fit_glmnet_cv <-
        glmnet::cv.glmnet(
          parallel = TRUE,
          x = x,
          y = y,
          alpha = 0
        )

      message(
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
    # lambda = lambda_optm_o
    # lambda = 200
    lambda = 100
  )
estimates_o %>% arrange(desc(rapm))

estimates_d <-
  create_estimates(
    x = x_glmnet_d,
    y = y_glmnet_d,
    # lambda = lambda_optm_d
    # lambda = 200
    lambda = 100
  )
estimates_d %>% arrange(desc(rapm))

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
estimates

players <-
  config$path_players %>%
  teproj::import_path_cleanly()

estimates_pretty <-
  estimates %>%
  mutate_at(vars(matches("rapm")), funs(rnk = row_number(desc(.)))) %>%
  left_join(players, by = "id")
estimates_pretty

teproj::export_path(
  estimates_pretty,
  path = config$path_data_export,
  export = config$export_data
)

message(
  sprintf("Exported final rapm estimates to \"%s\".", config$output)
)

if(exists("cl")) {
  # closeAllConnections()
  # doParallel::stopImplicitCluster()
  parallel::stopCluster(cl)
}

