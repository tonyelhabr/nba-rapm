
dir_proj <- "C:/Users/aelhabr/Documents/projects/nba-rapm"
if(!dir.exists(dir_proj)) {
  stop("Project directory does not exist!", call. = FALSE)
}

if (!interactive()) {
  setwd(wd)
  invisible(source(".Rprofile"))

} else {
  # message("Running this script in interactive mode! (It is not designed to do so.)")
    args <-
      get_args(
      description = "A descriptive description.",
      name = "A cool name"
    )
}

# setup ----
pre_auto()
do_setup_cores()

# clean ----
do_clean_raw_data()

# intermediate ----
do_process_cleaned_data()

# final ----
# do_fit_rapm_model_o()
# do_fit_rapm_model_d()
do_fit_rapm_models()

post_auto()
