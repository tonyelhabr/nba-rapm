
# cl <- parallel::makeCluster(4)
# doParallel::registerDoParallel(cl)

data <-
  config$path_data_clean %>%
  teproj::import_path_cleanly()

separate_lineup <-
  function(data, col, prefix = "x", suffix = 1:5, sep = "-") {
    data %>%
      separate(!!enquo(col), into = paste0(prefix, suffix), sep = sep)
  }

data <-
  data %>%
  separate_lineup(lineup1, suffix = 1:5) %>%
  separate_lineup(lineup2, suffix = 6:10) %>%
  mutate_at(vars(matches("^x")), funs(as.integer))
data

data <-
  data %>%
  gather(dummy, id, matches("^x")) %>%
  select(-dummy) %>%
  mutate_at(vars(is_off), funs(if_else(. == 0L, "d", "o"))) %>%
  mutate_at(vars(id), funs(sprintf("%s%010d", is_off, as.integer(.)))) %>%
  select(-is_off) %>%
  mutate(dummy = 1L)

path_cache_o <- "data/data-wide-o.rds"
path_cache_d <- "data/data-wide-d.rds"

create_data_wide <-
  function(data, prefix = c("o", "d"), path) {
    # prefix <- "o"
    sprintf("Trying to export \"%s\" possession data to %s.", prefix, path)
    res <-
      data %>%
      filter(str_detect(id, sprintf("^%s", prefix))) %>%
      mutate(n_poss = 1L) %>%
      group_by_at(vars(-pts, -n_poss)) %>%
      summarise_at(vars(pts, n_poss), funs(sum)) %>%
      ungroup() %>%
      select(pts, n_poss, everything()) %>%
      # filter(n_poss > 1) %>%
      filter(n_poss > 20) %>%
      filter(pts > 0) %>%
      mutate(pts = 100 * pts / n_poss) %>%
       #filter(pts <= 300) %>%
      filter(pts <= 200) %>%
      select(-n_poss) %>%
      spread(id, dummy, fill = 0L)

    res %>% write_rds(path)
    res
  }

data_wide_o <- create_data_wide(data, prefix = "o", path = path_cache_o)
data_wide_d <- create_data_wide(data, prefix = "d", path = path_cache_d)

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
    # lambda = lambda_optm_d * 4
    lambda = 50 # 200
  )
estimates_o %>% arrange(desc(rapm))

estimates_d <-
  create_estimates(
    x = x_glmnet_d,
    y = y_glmnet_d,
    # lambda = lambda_optm_d
    lambda = 50 #200
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

estimates_pretty <-
  estimates %>%
  mutate_at(vars(matches("rapm")), funs(rnk = row_number(desc(.)))) %>%
  left_join(players, by = "id")
estimates_pretty

estimates %>% teproj::export_path(config$path_export)
message(
  sprintf("Exported final rapm estimates to \"%s\".", config$output)
)

if(exists("cl")) {
  # closeAllConnections()
  # doParallel::stopImplicitCluster()
  parallel::stopCluster(cl)
}

