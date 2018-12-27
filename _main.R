
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
setup_cores_auto()
for (s in .SEASONS) {
  # s <- .SEASONS[1]
  clean_play_by_play_auto(season = s, skip = TRUE)
  munge_play_by_play_auto(season = s, skip = FALSE)
  fit_rapm_models_auto(season = s, skip = FALSE, optimize_o = TRUE, optimize_d = TRUE)
  extract_rapm_estimates_auto(season = s, skip = FALSE)
}
desetup_cores_auto()
post_auto()
# .get_config_name(name = config$path_possession_data, side = "o")
# .get_path_from(path = config$path_possession_data, season = 2017, side = "d")
