
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
do_setup_cores()

# clean ----
do_clean_raw_data()

# intermediate ----
do_process_cleaned_data()

# final ----
do_fit_rapm_models()

post_auto()
