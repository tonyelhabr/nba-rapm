
dir_proj <- "C:/Users/aelhabr/Documents/projects/nba-rapm"
if(!dir.exists(dir_proj)) {
  stop("Project directory does not exist!", call. = FALSE)
}

if (!interactive()) {
  setwd(dir_proj)
  invisible(source(".Rprofile"))

}

pre_auto()

# setup ----
auto_setup_cores()

# debug(auto_clean_raw_play_by_play)
auto_clean_raw_play_by_play()

auto_munge_cleaned_play_by_play()

auto_fit_rapm_models()

auto_extract_rapm_estimates()

post_auto()
