
if(FALSE) {
  # pre-process ----
  # Note that `overwrite = FALSE` is the default, but setting it explciitly here to remind
  # the user that it is an option.
  download_nbastatr(overwrite = FALSE)
  download_rpm_espn(overwrite = FALSE)
  download_rapm_sz(overwrite = FALSE)
  combine_rpm_espn()
  combine_rapm_sz()

  # main ----
  # This is the "meat".
  download_pbp_raw_files(overwrite = FALSE)
  auto_main()

  # post-process ----
  # visualize_proj_profile()
}



