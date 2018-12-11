
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

suppressWarnings(suppressPackageStartupMessages(library("tidyverse")))
suppressWarnings(suppressPackageStartupMessages(library("argparser")))
# Don't load this explicitly in order to avoid name conflicts with base R.
# suppressWarnings(suppressPackageStartupMessages(library("config")))
suppressWarnings(suppressPackageStartupMessages(library("nbastatR")))
# suppressWarnings(suppressPackageStartupMessages(library("doParallel")))
# suppressWarnings(suppressPackageStartupMessages(library("glmnet")))


# config <- config::get()

.SKIP <- FALSE
.DEBUG <- TRUE
.VERBOSE <- TRUE
.EXPORT <- TRUE
.BACKUP <- TRUE
.CLEAN <- TRUE
.N_KEEP <- 1L
.OPTIMIZE <- FALSE
.SEED <- 42
.LAMBDA <- 200

.SEASONS <- 2017L
.SEASON <- .SEASONS[1]
.RAW_DATA_SOURCES <- c("rd","etf")
.RAW_DATA_SOURCE <- .RAW_DATA_SOURCES[1]
.SEASON_TYPES <- "regular"
.SEASON_TYPE <- .SEASON_TYPES[1]
.SIDES <- c("o", "d")

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

if(interactive()) {
  config <- import_config()
}


