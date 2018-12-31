
# Reference: https://squared2020.com/2017/09/18/deep-dive-on-regularized-adjusted-plus-minus-ii-basic-application-to-2017-nba-data-with-r/
path <- "data-raw/play_by_play_with_lineup_2017.csv"
# path <- "data-raw/PbP_17_18.Rda"
# load(path)
# play_by_play0 <-
#   pbp %>%
#   as_tibble() %>%
#   mutate_if(is.factor, funs(as.character)) %>%
#   janitor::clean_names() %>%
#   arrange(game_id, period, eventnum)
# play_by_play0 %>% count(rebound_offensive_count, shot_type, sort = TRUE)

raw_shots_all <-
  path %>%
  readr::read_csv() %>%
  janitor::clean_names()

shots_all <-
  raw_shots_all %>%
  # Borrowing some of my data cleaning code.
  filter(season_type == "Regular Season") %>%
  mutate_at(vars(matches("description$")), funs(na_if(., ""))) %>%
  mutate(
    description = coalesce(home_description, away_description)
  ) %>%
  select(
    id_game = game_id,
    period,
    event_num = event_number,
    pc_time_string,
    sec_elapsed = time_elapsed,
    pts_home = away_score,
    pts_away = home_score,
    id_team1 = team_id1,
    id_team2 = team_id2,
    play_type,
    description,
    matches("team1player[1-5]id$"),
    matches("team2player[1-5]id$")
  ) %>%
  mutate(pts_total = pts_home + pts_away) %>%
  arrange(id_game, period, sec_elapsed, pts_total, event_num) # %>%
  # group_by(id_game) %>%
  # fill(pts_home) %>%
  # fill(pts_away) %>%
  # ungroup()
shots_all

.extract_unique_values_at <-
  function(data, rgx, ...) {
    data %>%
      select_at(
        vars(matches(rgx))
      ) %>%
      gather(key, value) %>%
      tetidy::pull_distinctly(value)
  }

id_games <-
  shots_all %>%
  tetidy::pull_distinctly(id_game)
players <-
  shots_all %>%
  .extract_unique_values_at(rgx = "^team[12]player[1-5]id$")
stints <-
  players %>%
  length() %>%
  matrix(, 0, .) %>%
  as_tibble() %>%
  purrr::set_names(players)
stintCount <- 1

shots_all %>% count(play_type, sort = TRUE)
shots_all <-
  shots_all %>%
  filter(play_type %in% c("Make", "Miss", "Rebound", "Turnover", "FreeThrow"))

.compare_shots_at_rows <-
  function(data,
           idx1,
           idx2 = idx1 + 1,
           team_num,
           rgx = glue::glue("^team[{team_num}]player[1-5]id$")) {
    all(
      (
        shots[idx1, ] %>%
          .extract_unique_values_at(rgx = "^team[1]player[1-5]id$")
      ) %in%
        (
          shots[idx2, ] %>%
            .extract_unique_values_at(rgx = "^team[1]player[1-5]id$")
        )
    )
  }

for(id_game in id_games[1]) {
  id_game <- id_games[1]
  shots <- shots_all %>% filter(id_game == !!id_game)

  # Initializing...
  # awayplayersStart <-
  #   shots[1,] %>%
  #   .extract_unique_values_at(rgx = "^team[1]player[1-5]id$")
  # homeplayersStart <-
  #   shots[1,] %>%
  #   .extract_unique_values_at(rgx = "^team[2]player[1-5]id$")
  possessions <- 0
  # What's the difference?
  awayPoints <- 0
  homePoints <- 0
  awayScore <- 0
  homeScore <- 0
  for(i in 1:nrow(shots)) {

    i <- 1
    # awayplayers <-
    #   shots[i,] %>%
    #   .extract_unique_values_at(rgx = "^team[1]player[1-5]id$")
    # homeplayers <-
    #   shots[i,] %>%
    #   .extract_unique_values_at(rgx = "^team[2]player[1-5]id$")
    # bothHome <- all(homeplayersStart %in% homeplayers)
    # bothAway <- all(awayplayersStart %in% awayplayers)
    bothHome <-
      .compare_shots_at_rows(
        data = shots,
        idx1 = 1,
        idx2 = i,
        team_num = 1
      )
    bothAway <-
      .compare_shots_at_rows(
        data = shots,
        idx1 = 1,
        idx2 = i,
        team_num = 2
      )
    if(bothHome & bothAway) {
      play_type <- shots[[i, "play_type"]]
      if(play_type %in% c("Make", "Miss")) {
        possessions <- possessions + 1
        if(awayScore == shots[[i, "pts_away"]]) {
          homeScore <- homeScore + shots[[i, "pts_home"]]
          homePoints <- homePoints + shots[[i, "pts_home"]]
        } else {
          awayScore <- awayScore + shots[[i, "pts_away"]]
          awayPoints <- awayPoints + shots[[i, "pts_away"]]
        }
      } else {
        sameHome <-
          .compare_shots_at_rows(
            data = shots,
            idx1 = 1,
            team_num = 1
          )
        sameAway <-
          .compare_shots_at_rows(
            data = shots,
            idx1 = 1,
            team_num = 2
          )
        if ((play_type == "Rebound" & sameHome & sameAway) |
            play_type == "Turnover") {
          possessions <- possessions + 1
        } else if (play_type == "FreeThrow") {
          midFreeThrow <- shots[[i, "pc_time_string"]]
          shots %>%
            filter(play_type == "FreeThrow") %>%
            mutate(
              freethrow_total = description %>% str_replace_all("(^.*of\\s+)([0-9])(.*$)", "\\2") %>% as.integer()
            ) %>%
            select(description, freethrow_total)
          totalToShoot <- shots[i, "description"]
        }
      }
    }
  }
}

