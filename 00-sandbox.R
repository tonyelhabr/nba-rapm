
library("tidyverse")
library("parallel")
library("doParallel")
library("glmnet")

n_core <- 4
cl <- parallel::makeCluster(n_core)
doParallel::registerDoParallel(cl)

path_data_raw <- "rapm_data.csv"
path_cache_a <- "data-cache-a.rds"
path_cache_b <- "data-cache-b.rds"

if(!file.exists(path_cache_a) & !file.exists(path_cache_b)) {
  data_raw <-
    path_data_raw  %>%
    read_csv() %>%
    rename_all(tolower)

  separate_rapm_data <-
    function(data, col, prefix = "x", suffix = 1L:5L) {
      data %>%
        separate(!!enquo(col), into = paste0(prefix, suffix), sep = "\\.")
    }
  # # Debugging...
  # data_raw %>% count(gameid)
  data_raw %>% distinct(gameid)

  data_sep <-
    data_raw %>%
    # filter(gameid <= 21000500) %>%
    mutate_at(vars(matches("concatlineup")), funs(str_replace_all(., "^\\.|\\.$", ""))) %>%
    separate_rapm_data(concatlineup, suffix = 1L:5L) %>%
    separate_rapm_data(concatlineup_opp, suffix = 6L:10L) %>%
    mutate_at(vars(matches("^x")), funs(as.integer))
  data_sep

  data_long <-
    data_sep %>%
    gather(dummy, id, matches("^x")) %>%
    select(-dummy) %>%
    mutate_at(vars(ishometeam), funs(if_else(. == 0L, "b", "a"))) %>%
    mutate_at(vars(id), funs(sprintf("%s%06d", ishometeam, as.integer(.)))) %>%
    select(-ishometeam) %>%
    mutate(dummy = 1L)

  data_long %>%
    count(id, sort = TRUE)
  ids_0 <-
    data_long %>%
    filter(gameid == 21000001L) %>%
    distinct(id) %>%
    pull(id)

  create_data_wide <-
    function(data, prefix = c("a", "b"), cache = TRUE, path = ) {
        data %>%
        filter(str_detect(id, "^a")) %>%
        spread(id, dummy, fill = 0L) %>%
        select(-gameid, -overallpossnum) %>%
        mutate(poss = 1L) %>%
        group_by_at(vars(-pts, -poss)) %>%
        summarise_at(vars(pts, poss), funs(sum)) %>%
        ungroup() %>%
        select(pts, poss, everything())
        filter(poss > 1) %>%
        filter(pts > 0) %>%
        mutate(pts = 100 * pts / poss) %>%
        select(-poss)
    }


  data_wide %>% write_rds(path_cache)
} else {
  data_wide <- path_cache %>% read_rds()
}

fmla <- formula(pts ~ . + 0)
DEBUG <- FALSE
if(DEBUG) {
  data_wide %>%
    select(1, 2, ncol(.)) %>%
    purrr::set_names(c("pts", "x1", "x2")) %>%
    filter(x1 != 0L) %>%
    group_by(x1, x2) %>%
    summarise_at(
      vars(pts),
      funs(sum, poss = n())
    ) %>%
    ungroup()
  path_export_0 <- "rapm_estimates-0.csv"
  if(!file.exists(path_export_0)) {
    ids_0 <-
      paste0(
        "a",
        c(
          "000406",
          "002306" ,
          "002544",
          "000708" ,
          "000951",
          "002067" ,
          "000979",
          "000980" ,
          "002547" ,
          "002548",
          "001718",
          "002605",
          "002592",
          "101126" ,
          "200765",
          "002617",
          "201202",
          "201175"
        )
      )
    data_wide_0 <-
      data_wide %>%
      select(one_of(c("pts", ids_0)))
    fit_glm <- glm(fmla, family = "gaussian", data = data_wide_0)
    id_estimates <-
      fit_glm %>%
      broom::tidy() %>%
      arrange(desc(estimate)) %>%
      mutate_at(vars(term), funs(str_replace(., "^a", ""))) %>%
      select(id = term, estimate)

    id_estimates %>% write_csv(path_export_0)
  }

  path_export_` <- "rapm_estimates-1.csv"
  if(!file.exists(path_export_off)) {
    fit_glm <- glm(fmla, family = "gaussian", data = data_wide)

    id_estimates <-
      fit_glm %>%
      broom::tidy() %>%
      arrange(desc(estimate)) %>%
      mutate_at(vars(term), funs(str_replace(., "^a|^b", ""))) %>%
      select(id = term, estimate)
    id_estimates

    id_estimates %>% write_csv(path_export_1)
  }
}

# All of the rest of this is just experimental.
# glmnet ----
data_x_glmnet <-
  fmla %>%
  model.matrix(data_wide)
data_y_glmnet <-
  data_wide %>%
  pull(pts)

# glmnet-sandbox ----
fit_glmnet_cv <-
  glmnet::cv.glmnet(
    parallel = TRUE,
    x = data_x_glmnet,
    y = data_y_glmnet,
    alpha = 0
  )
fit_glmnet_cv
fit_glmnet_cv$lambda.min
lambda_min_off <- fit_glmnet_cv$lambda.min
lambda_min <- 162.4301
# lambda_min <- 0.0001
fit_glmnet <-
  glmnet::glmnet(
    intercept = TRUE,
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

# caret ----

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
          lambda = 10 ^ seq(3, -2, by = -1),
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
          lambda = 10 ^ seq(3, -2, by = -1),
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
fits_glmnet <-
  grid_model_glmnet %>%
  group_by(idx_method, method_desc) %>%
  nest() %>%
  mutate(
    fit =
      purrr::map(data,
                 ~fit_caret_method(method = .x$method, preproc = .x$preproc)
      )) %>%
  ungroup()
fits_glmnet

