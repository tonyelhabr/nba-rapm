
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

..SEASONS <- 2017L
.SEASONS <- purrr::set_names(..SEASONS, as.character(..SEASONS))
.SEASON <- .SEASONS[1]
.RAW_DATA_SOURCES <- c(etf = "eightthirtyfour", rd = "Ryan Davis")
.RAW_DATA_SOURCE <- .RAW_DATA_SOURCES[1]
.SEASON_TYPES <- c(rs = "Regular Season", plyffs = "Playoffs", any = "Any")
.SEASON_TYPE <- .SEASON_TYPES[1]
..SIDES <- c("o", "d")
.SIDES <- purrr::set_names(..SIDES, as.character(..SIDES))

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
  args <-
    get_args(
      description = "A descriptive description.",
      name = "A cool name"
    )
}


