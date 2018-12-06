

parser <-
  to_argparser(config, description = "A descriptive description.", name = "A cool name")

if(argparser$clean & !file.exists(argparser$path_data_clean)) {
  data_clean <-
    clean_play_by_play_data(
      path_play_by_play_raw_format = argparser$path_play_by_play_raw_format,
      path_game_summary_raw_format = argparser$pathh_game_summary_raw_format,
      season = argparser$season,
      ...,
      verbose = argparser$verbose,
      export = argparser$export_clean,
      path_data_clean_format = argparser$path_data_clean_format
    )
}
