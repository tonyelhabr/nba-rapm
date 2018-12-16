
# C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R"

dir_proj <- "C:/Users/aelhabr/Documents/projects/nba-rapm"
if(!dir.exists(dir_proj)) {
  stop("Project directory does not exist!", call. = FALSE)
}

if (!interactive()) {
  setwd(dir_proj)
  invisible(source(".Rprofile"))
}

pre_auto()

# auto_setup_cores()

auto_clean_play_by_play()

auto_munge_play_by_play()

auto_fit_rapm_models()

auto_extract_rapm_estimates()

auto_desetup_cores()

post_auto()
