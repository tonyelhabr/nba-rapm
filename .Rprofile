
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
suppressWarnings(suppressPackageStartupMessages(library("tidyverse")))
suppressWarnings(suppressPackageStartupMessages(library("rlang")))
suppressWarnings(suppressPackageStartupMessages(library("argparser")))
# Don't load this explicitly in order to avoid name conflicts with base R.
# suppressWarnings(suppressPackageStartupMessages(library("config")))
suppressWarnings(suppressPackageStartupMessages(library("nbastatR")))
# suppressWarnings(suppressPackageStartupMessages(library("doParallel")))
# suppressWarnings(suppressPackageStartupMessages(library("glmnet")))


# config <- config::get()

.SKIP <- FALSE
.VERBOSE <- TRUE
.EXPORT <- TRUE
.BACKUP <- FALSE
.CLEAN <- TRUE
.N_KEEP <- 0L
.OPTIMIZE <- FALSE
.SEED <- 42
.LAMBDA <- 200

.SEASONS <- 2009:2017
.SEASON <- .SEASONS[-1]
# .ID_GAME_DEBUG <- 21600001
.SEASON_TYPES <- "regular"
.SEASON_TYPE <- .SEASON_TYPES[1]

paths_funcs <-
  list.files(
    path = file.path("R", "functions"),
    # path = "R",
    pattern = "func",
    full.names = TRUE
  )
invisible(sapply(paths_funcs, source))
# sapply(paths_funcs, source)
rm("paths_funcs")

# if(interactive()) {
#   config <- import_config()
# }
config <- import_config()

