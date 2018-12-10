
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
.VERBOSE <- TRUE
.EXPORT <- TRUE
.BACKUP <- FALSE # TODO
.CLEAN <- FALSE # TODO
.RAW_DATA_SOURCES <- c("eightthirtyfour", "Ryan Davis")
.RAW_DATA_SOURCE <- .RAW_DATA_SOURCES[1]
.SEASON_TYPES <- c("Regular Season", "Playoffs", "Any")
.SEASON_TYPE <- .SEASON_TYPES[1]
.OPTIMIZE <- FALSE
.SEED <- 42
.LAMBDA <- 200
.SIDES <- c("o", "d")

paths_funcs <-
  list.files(
    path = file.path("R", "functions"),
    # path = "R",
    pattern = "func",
    full.names = TRUE
  )
invisible(sapply(paths_funcs, source))
rm("paths_funcs")

# options(tibble.print_min = 20)

if(interactive()) {
  args <-
    get_args(
      description = "A descriptive description.",
      name = "A cool name"
    )
}


