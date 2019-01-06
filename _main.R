
# Example command:
# C:/Users/aelhabr/Documents/R/R-3.5.2/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R" --season 2017

# Prevent this from being sourced.
dir_proj <- "C:/Users/aelhabr/Documents/projects/nba-rapm"
if(!dir.exists(dir_proj)) {
  stop("Project directory does not exist!", call. = FALSE)
}

if (!interactive()) {
  setwd(dir_proj)
  invisible(source(".Rprofile"))
}

auto_main()

