
library("base")
library("methods")
library("datasets")
library("utils")
library("grDevices")
library("graphics")
library("stats")

path_r_profile <- "~/.Rprofile"
if(file.exists(path_r_profile)) {
  source(path_r_profile)
}
rm("path_r_profile")

# suppressWarnings(suppressPackageStartupMessages(library("rlang")))
# suppressWarnings(suppressPackageStartupMessages(library("doParallel")))
# cl <- makeCluster(detectCores())
suppressWarnings(suppressPackageStartupMessages(library("tidyverse")))
suppressWarnings(suppressPackageStartupMessages(library("rlang")))
suppressWarnings(suppressPackageStartupMessages(library("argparser")))
# Don't load this explicitly in order to avoid name conflicts with base R.
# suppressWarnings(suppressPackageStartupMessages(library("config")))
suppressWarnings(suppressPackageStartupMessages(library("nbastatR")))
# suppressWarnings(suppressPackageStartupMessages(library("doParallel")))
# suppressWarnings(suppressPackageStartupMessages(library("glmnet")))
# suppressWarnings(suppressPackageStartupMessages(library("broom")))
# Need to load this for `broom:::fix_data_frame()` to work (although `broom:::tidy.glmnet()` still doesn't work(?)
suppressWarnings(suppressPackageStartupMessages(library("Matrix")))
suppressWarnings(suppressPackageStartupMessages(library("teplot")))

# config <- config::get()

.SKIP <- FALSE
.VERBOSE <- TRUE
.EXPORT <- TRUE
.BACKUP <- FALSE
.CLEAN <- TRUE
.N_KEEP <- 0L
.OVERWRITE <- FALSE

.COLLAPSE <- TRUE
.INTERCEPT <- TRUE
.SCALE <- FALSE
.OPTIMIZE <- FALSE
.SEED <- 42
.LAMBDA <- 200

.SEASONS <- 2015:2017 # 2009:2017
.SEASON <- rev(.SEASONS)[1]
# .ID_GAME_DEBUG <- 21600001
.SEASON_TYPES <- "regular"
.SEASON_TYPE <- .SEASON_TYPES[1]

paths_funcs <-
  list.files(
    path = file.path("R"),
    pattern = "^[0-9]{2}|^func",
    full.names = TRUE
  )
invisible(sapply(paths_funcs, source))
# sapply(paths_funcs, source)
rm("paths_funcs")

# if(interactive()) {
#   config <- import_config()
# }
config <- import_config()

