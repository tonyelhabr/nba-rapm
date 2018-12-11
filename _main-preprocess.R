
# purrr::map(
#   list(.SEASONS),
#   ~.try_import_players(
#     season = ..1
#   )
# )

# Or..., do it all at once.
purrr::invoke_map(
  .f = list(
    # .try_import_players_nbastatrs,
    # .try_import_teams_nbastatr,
    # .try_import_game_logs_player_nbastatr,
    # .try_import_game_logs_team_nbastatr,
    .try_import_players_summary_nbastatr,
    .try_import_teams_summary_nbastatr
  ),
  .x = list(list(season = .SEASON))
)
