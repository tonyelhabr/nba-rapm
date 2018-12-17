
# C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R"
# C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R" --season 2015
# C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R" --verbose FALSE

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

pre_auto()

# setup_cores_auto()
#
clean_play_by_play_auto()
#
# munge_play_by_play_auto()
#
# fit_rapm_models_auto()
#
# extract_rapm_estimates_auto()
#
# desetup_cores_auto()
#
post_auto()
