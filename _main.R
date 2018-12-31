
# C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R"
# C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R" --season 2015
# C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R" --verbose FALSE

# Not sure why, but `{doParallel}` does not load inside a project with RStudio 1.2(?)
# pacman::p_unload(char = pacman::p_loaded())
# library(doParallel); cl <- makeCluster(8)

dir_proj <- "C:/Users/aelhabr/Documents/projects/nba-rapm"
if(!dir.exists(dir_proj)) {
  stop("Project directory does not exist!", call. = FALSE)
}

if (!interactive()) {
  setwd(dir_proj)
  # args <- commandArgs(trailingOnly = TRUE)
  # print(args)
  invisible(source(".Rprofile"))
  # print(config)
}

# # pre_auto()
# # setup_cores_auto()
#
# purrr::walk(
#   # .SEASONS[3],
#   .SEASONS,
#   .f = function(x) {
#     # clean_play_by_play_auto(season = x, skip = TRUE)
#     munge_play_by_play_auto(season = x, skip = FALSE)
#     fit_rapm_models_auto(
#       season = x,
#       skip = FALSE,
#       # skip = TRUE,
#       # optimize = TRUE
#       optimize = FALSE
#     )
#     extract_rapm_coefs_auto(season = x, skip = FALSE)
#   }
# )
#
# # desetup_cores_auto()
# # post_auto()

