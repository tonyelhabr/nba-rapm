
# pre-process ----
# Note that `overwrite = FALSE` is the default, but setting it explciitly here to remind
# the user that it is an option.
debug(download_nbastatr)
download_nbastatr(overwrite = FALSE)
download_rpm_espn(overwrite = FALSE)
download_rapm_basketballanalytics(overwrite = FALSE)
combine_rpm_espn()
combine_rapm_basketballanalytics()

# process ----
# This is the "meat".
download_raw_play_by_play_files(overwrite = FALSE)
# source("_main.R", echo = FALSE)

# analyze ----
# This takes a while, so it's not vectorized across all seasons (yet).
visualize_proj_funcs()
analyze_rapm_coefs_auto()


# post-process ----
# combine_players_summary_compare()
combine_rapm_coefs()




