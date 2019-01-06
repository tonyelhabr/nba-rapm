
if(FALSE) {
  # pre-process ----
  # Note that `overwrite = FALSE` is the default, but setting it explciitly here to remind
  # the user that it is an option.
  download_nbastatr(overwrite = FALSE)
  download_rpm_espn(overwrite = FALSE)
  download_rapm_sz(overwrite = FALSE)
  combine_rpm_espn()
  combine_rapm_sz()

  # process ----
  # This is the "meat".
  download_pbp_raw_files(overwrite = FALSE)
  # main_auto()

  # analyze ----
  # This takes a while, so it's not vectorized across all seasons (yet).
  visualize_proj_profile()
  auto_analyze_rapm_coefs()


  # post-process ----
  # combine_players_summary_compare()
  combine_rapm_coefs()
}



