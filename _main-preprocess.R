
# # Do this once.
# download_raw_play_by_play_files()

# (Re-)generate data for a single `nbastatr` function.
# purrr::pmap(
#   list(.SEASONS),
#   ~.try_import_teams_nbastatr(
#     season = ..1
#   )
# )
# Or..., do it all at once.
# Note that its erroneous to use `season = .SEASONS` (because the functions
# will treat `season` as a vector instead of as a scalar), and it doesn't work to
# call `purrr::invoke_map()` from `purrr::map()`, so use a for loop instead.
for(s in .SEASONS) {
  purrr::invoke_map(
    .f = list(
      .try_import_players_nbastatr,
      .try_import_teams_nbastatr,
      .try_import_players_game_logs_nbastatr,
      .try_import_teams_game_logs_nbastatr,
      .try_import_players_summary_nbastatr,
      .try_import_teams_summary_nbastatr
    ),
    .x = list(list(season = s))
  )
}
