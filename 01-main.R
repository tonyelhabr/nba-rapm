
library("tidyverse")

path_cache <- file.path("data", "data-cache.rds")

library("parallel")
library("doParallel")
# parallel::detectCores()
cl <- parallel::makeCluster(4)
doParallel::registerDoParallel(cl)

if(!file.exists(path_cache)) {
  path <- file.path("data-raw", "rapm_data.csv")
  data_raw <-
    path %>%
    read_csv() %>%
    rename_all(tolower)
  data_raw
  separate_rapm_data <-
    function(data, col, suffix = 1L:5L) {
      data %>%
        separate(!!enquo(col), into = paste0("x", suffix), sep = "\\.")
    }
  # data_raw %>% count(gameid)

  data_sep <-
    data_raw %>%
    # filter(gameid <= 21000500) %>%
    mutate_at(vars(matches("concatlineup")), funs(str_replace_all(., "^\\.|\\.$", ""))) %>%
    separate_rapm_data(concatlineup, suffix = 1L:5L) %>%
    separate_rapm_data(concatlineup_opp, suffix = 6L:10L) %>%
    mutate_at(vars(ishometeam), funs(if_else(. == 0L, -1L, .)))

  data_long <-
    data_sep %>%
    gather(dummy, id, matches("^x[0-9]+$")) %>%
    select(-dummy) %>%
    mutate_at(vars(id), funs(sprintf("x%06d", as.integer(.))))
  data_long

  data_wide <-
    data_long %>%
    # select(-pts) %>%
    spread(id, ishometeam, fill = 0L) %>%
    select(-gameid, -overallpossnum) %>%
    group_by_at(vars(-pts)) %>%
    # summarise_all(funs(n())) %>%
    summarise_at(vars(pts), funs(sum)) %>%
    ungroup() %>%
    select(pts, everything())
  data_wide
  data_wide %>% write_rds(path_cache)
} else {
  data_wide <- path_cache %>% read_rds()
}

# Debugging...
data_wide %>%
  select(1, 2, ncol(.)) %>%
  purrr::set_names(c("pts", "x1", "x2")) %>%
  filter(x1 != 0L) %>%
  group_by(x1, x2) %>%
  summarise_at(
    vars(pts),
    funs(sum, n = n())
  ) %>%
  ungroup()
fmla <- formula(pts ~ . + 0)

fit_glm <- glm(fmla, family = "gaussian", data = data_wide)
fit_glm %>%
  broom::tidy() %>%
  arrange(desc(estimate))

# data_x <-
#   data_wide %>%
#   select(-pts)
# data_wide %>%
#   select(pts) %>%
#   arrange(desc(pts))
# m_x <- data_x %>% data.matrix()
# m_x %>% typeof()
# m_x %*% t(m_x)
# m_x

library("caret")
setup_caret <-
  function() {
    list(
      form = fmla,
      data = data_wide,
      trControl = caret::trainControl(method = "cv", number = 10),
      metric = "RMSE"
    )
  }
get_caret_glmnet_lasso <-
  function() {
    list(
      method = "glmnet",
      family = "gaussian",
      standardize = TRUE,
      tuneGrid =
        expand.grid(
          alpha = 1,
          lambda = 10 ^ seq(3, -2, by = 1),
          stringsAsFactors = FALSE
        )
    )
  }
get_caret_glmnet_rr <-
  function() {
    list(
      method = "glmnet",
      family = "gaussian",
      standardize = TRUE,
      tuneGrid =
        expand.grid(
          alpha = 0,
          lambda = 10 ^ seq(3, -2, by = 1),
          stringsAsFactors = FALSE
        )
    )
  }
get_caret_glmnet_elnet <-
  function() {
    list(
      method = "glmnet",
      family = "gaussian",
      standardize = TRUE,
      tuneGrid = expand.grid(
        alpha = seq(0, 1, by = 0.25),
        lambda = 10 ^ seq(3, -2, by = 1),
        stringsAsFactors = FALSE
      )
    )
  }

fit_caret_method <-
  function(method, preproc = "scale") {
    purrr::invoke(
      caret::train,
      c(
        purrr::invoke(setup_caret),
        purrr::invoke(sprintf("get_caret_%s", method)),
        preProcess = switch(preproc == "none", NULL, preproc)
      )
    )
  }

grid_model_glmnet <-
  tribble(
    ~idx_method, ~method_desc, ~method, ~preproc,
    # 1L, "{glmnet} Lasso (with Scaling)", "glmnet_lasso", "none"
    1L, "{glmnet} Ridge Regression (with Scaling)", "glmnet_rr", "none",
    # 1L, "{glmnet} Elastic Net (with Scaling)", "glment_elnet", "none"
  )

set.seed(42)
# fits_glmnet <-
#   grid_model_glmnet %>%
#   group_by(idx_method, method_desc) %>%
#   nest() %>%
#   mutate(
#     fit =
#       purrr::map(data,
#                  ~fit_caret_method(method = .x$method, preproc = .x$preproc)
#     )) %>%
#   ungroup()
# fits_glmnet

# glmnet ----
data_x_glmnet <-
  fmla %>%
  model.matrix(data_wide)
data_y_glmnet <-
  data_wide %>%
  pull(pts)

partial_fit_glmnet_cv <-
  purrr::partial(
    # glmnet::glmnet,
    glmnet::cv.glmnet,
    x = data_x_glmnet,
    y = data_y_glmnet,
    # family = "gaussian",
    standardize = TRUE
    # standardize = FALSE
  )

partial_fit_glmnet <-
  purrr::partial(
    glmnet::glmnet,
    x = data_x_glmnet,
    y = data_y_glmnet,
    # family = "gaussian",
    standardize = TRUE
    # standardize = FALSE
  )

# glmnet-sandbox ----

# sandbox ----
# fit_glmnet_cv <-
#   glmnet::cv.glmnet(
#     parallel = TRUE,
#     x = data_x_glmnet,
#     y = data_y_glmnet,
#     alpha = 0
#   )
# fit_glmnet_cv
# fit_glmnet_cv$lambda.min
lambda_min <- 33.88806
lambda_min <- 0.0001
fit_glmnet <-
  glmnet::glmnet(
    parallel = TRUE,
    x = data_x_glmnet,
    y = data_y_glmnet,
    alpha = 0,
    lambda = lambda_min
  )
fit_glmnet %>%
  broom::tidy() %>%
  arrange(desc(estimate))

fit_glmnet %>%
  broom::tidy() %>%
  arrange(estimate)

# fit_glmnet_rr_cv ----
lambdas_cv <- 10 ^ seq(2, -3, by = -0.1)
fit_glmnet_rr_cv <-
  partial_fit_glmnet(
    lambda = lambdas_cv
    # alpha = 0
  )
fit_glmnet_rr_cv %>% plot()
fit_glmnet_rr_cv %>% plot(xvar = "lambda", label = TRUE)
fit_glmnet_rr_cv %>% coef()
coef(fit_glmnet_rr_cv)
fit_glmnet_rr_cv %>% coef(s = "lambda.min")

# fit_glmnet_rr_cv_optm ----
fit_glmnet_rr_cv_optm <-
  fit_glmnet_rr_cv %>%
  broom::tidy() %>%
  filter(step == max(step)) %>%
  arrange(estimate) %>%
  slice(1)
lambda_optm <- fit_glmnet_rr_cv_optm %>% pull(lambda)
lambda_optm
alpha_optm <- 0

# fit_glmnet_rr_optm ----
fit_glmnet_rr_optm <-
  partial_fit_glmnet(lambda = lambda_optm, alpha = alpha_optm)
fit_glmnet_rr_optm
fit_glmnet_rr_optm %>%
  broom::tidy() %>%
  arrange(desc(estimate))
fit_glmnet_rr_optm %>%
  coef()

# fit_glmnet_rr ----
fit_glmnet_rr <-
  partial_fit_glmnet(alpha = alpha_optm)
fit_glmnet_rr
