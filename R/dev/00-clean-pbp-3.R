
# Reference: http://eightthirtyfour.com/data
load("~/projects/nba-rapm/data-raw/PbP_17_18.Rda")

play_by_play <-
  pbp %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  # distinct(game_id) %>%
  # count(game_id)
  select(
    game_id,
    period,
    eventnum,
    pctimestring,
    score,
    home_score,
    away_score,
    time,
    type,
    points_scored,
    possession_id,
    period_start,
    period_end,
    matches("^(home|away)_player_id_[1-5]$")
  )
play_by_play
