
library("tidyverse")

season <- 2018
# direct call to `tbastatR::bref_teams_stats()` ----
# teams_summary_nbastatr <-
#   nbastatR::bref_teams_stats(
#     seasons = season,
#     return_message = TRUE,
#     assign_to_environment = TRUE,
#     nest_data = FALSE,
#     join_data = TRUE,
#     widen_data = TRUE
#   )

# internals of `tbastatR::bref_teams_stats()` ----
# df_urls <-
#   nbastatR:::.generate_season_urls(seasons = season) %>%
#   mutate(urlSeasonBREF = as.character(urlSeasonBREF))
#
# .parse_season_url.safe <-
#   purrr::possibly(nbastatR:::.parse_season_url, data_frame())
#
# return_message <- TRUE
# all_data <-
#   df_urls$urlSeasonBREF %>%
#   furrr::future_map_dfr(function(x) {
#     if (return_message) {
#       glue::glue("Parsing {x}") %>% cat(fill = T)
#     }
#     .parse_season_url.safe(url = x)
#   })
#
# nbastatR:::.dictionary_bref_tables()

# internals of `nbastatR:::.parse_season_url()` ----
url <- "https://www.basketball-reference.com/leagues/NBA_2018.html"
page <-
  url %>%
  nbastatR:::.read_page()
page
xml_tables <-
  page %>%
  rvest::html_nodes(xpath = "//*[contains(@class, 'sortable')]")
xml_tables

f <- function(x) {
  # x <- 1
  message(x)
  table_id <-
    xml_tables[x] %>%
    rvest::html_attr("id")

  table_name <-
    xml_tables[[x]] %>%
    rvest::xml_nodes("caption") %>%
    rvest::html_text()

  is_ap <-
    table_id == "all_playoffs"

  is_awards <- table_id == "all_awards"

  if (is_ap) {
    return(invisible())
  }

  data <-
    xml_tables[[x]] %>%
    rvest::html_table(header = F,
                      trim = T,
                      fill = F) %>%
    tibble::as_data_frame()

  team_nodes <-
    xml_tables[x] %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr('href')

  team_slugs <-
    team_nodes %>% str_replace_all("/teams/", "") %>%
    substr(1, 3)

  url_team <-
    team_nodes %>%
    str_c("https://www.basketball-reference.com", .)

  name_team <-
    xml_tables[x] %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()

  df_urls <-
    data_frame(
      slugTeamBREF = team_slugs,
      nameTeam = name_team,
      urlBREFTeamData = url_team
    )

  is_misc <- table_id %>% str_detect("misc_stats")

  is_conf <-
    table_id %>% str_detect("confs_standings")

  is_division <-
    table_id %>% str_detect("divs_standing")

  is_team_stats_pg <-
    table_id %>% str_detect("team-stats-per_game")

  is_opp_stats_pg <-
    table_id %>% str_detect("opponent-stats-per_game")

  is_team_base <-
    table_id %>% str_detect("team-stats-base")

  is_opp_total <-
    table_id %>% str_detect("opponent-stats-base")

  is_team_100 <-
    table_id %>% str_detect("team-stats-per_poss")

  is_opp_100 <-
    table_id %>% str_detect("opponent-stats-per_poss")

  is_team_shooting <-
    table_id %>% str_detect("team_shooting")

  is_opponent_shooting <-
    table_id %>% str_detect("opponent_shooting")

  if (is_team_shooting) {
    table_name <- "Team Shooting"
    table_data <-
      data %>%
      nbastatR:::.parse.bref.team.shooting() %>%
      mutate(typeData = "Team",
             timeframeData = "Shooting") %>%
      select(typeData, timeframeData, everything()) %>%
      filter(!nameTeam %>% str_detect("Average"))
  }

  if (is_opponent_shooting) {
    table_name <- "Opponent Shooting"
    table_data <-
      data %>%
      nbastatR:::.parse.bref.team.shooting() %>%
      mutate(typeData = "Opponent",
             timeframeData = "Shooting") %>%
      select(typeData, timeframeData, everything()) %>%
      filter(!nameTeam %>% str_detect("Average"))
  }

  if (is_awards) {
    df_urls <-
      df_urls[c(F, T), ]

    df_urls <-
      df_urls %>%
      select(-one_of(c("slugTeamBREF"))) %>%
      purrr::set_names(c("namePlayer", "urlPlayer")) %>%
      suppressWarnings()

    table_name <-
      c("Player Awards")

    table_data <-
      data %>%
      slice(-1) %>%
      purrr::set_names(c("nameAward", "namePlayer"))
  }

  if (is_misc) {
    table_name <- "Miscellaneous Stats"
    table_data <-
      data %>%
      nbastatR:::.parse.bref.team.misc() %>%
      filter(!nameTeam %>% str_detect("Average"))
  }

  if (is_opp_100) {
    table_name <- "Opponent Stats Per 100 Possessions"
    table_data <-
      data %>%
      nbastatR:::.parse.bref.team.pg() %>%
      mutate(typeData = "Opponent",
             timeframeData = "Per100Poss") %>%
      select(typeData, timeframeData, everything()) %>%
      filter(!nameTeam %>% str_detect("Average"))
  }

  if (is_opp_stats_pg) {
    table_name <- "Opponent Stats Per Game"
    table_data <-
      data %>%
      nbastatR:::.parse.bref.team.pg.opp() %>%
      mutate(typeData = "Opponent",
             timeframeData = "PerGame") %>%
      select(typeData, timeframeData, everything()) %>%
      filter(!nameTeam %>% str_detect("Average"))
  }

  if (is_team_100) {
    table_name <- "Team Stats Per 100 Possessions"
    table_data <-
      data %>%
      nbastatR:::.parse.bref.team.pg() %>%
      mutate(typeData = "Team",
             timeframeData = "Per100Poss") %>%
      select(typeData, timeframeData, everything()) %>%
      filter(!nameTeam %>% str_detect("Average"))
  }

  if (is_team_base) {
    table_name <- "Team Stats Totals"
    table_data <-
      data %>%
      nbastatR:::.parse.bref.team.pg() %>%
      mutate(typeData = "Team",
             timeframeData = "Total") %>%
      select(typeData, timeframeData, everything()) %>%
      filter(!nameTeam %>% str_detect("Average"))
  }

  if (is_opp_total) {
    table_name <- "Opponent Stats Totals"
    table_data <-
      data %>%
      nbastatR:::.parse.bref.team.pg() %>%
      mutate(typeData = "Opponent",
             timeframeData = "Total") %>%
      select(typeData, timeframeData, everything()) %>%
      filter(!nameTeam %>% str_detect("Average"))
  }

  if (is_team_stats_pg) {
    table_name <- "Team Stats Per Game"
    table_data <-
      data %>%
      nbastatR:::.parse.bref.team.pg() %>%
      mutate(typeData = "Team",
             timeframeData = "PerGame") %>%
      select(typeData, timeframeData, everything()) %>%
      filter(!nameTeam %>% str_detect("Average"))
  }

  if (is_conf) {
    table_name <- "Conference Standings"
    table_data <-
      data %>%
      nbastatR:::.parse.bref.team.conference()
  }

  if (is_division) {
    table_name <- "Division Standings"
    table_data <-
      data %>%
      nbastatR:::.parse.bref.team.division()
  }

  table_data <-
    table_data %>%
    left_join(df_urls) %>%
    suppressMessages()

  data_frame(
    idTable = table_id,
    nameTable = table_name,
    dataTable = list(table_data)
  )
}

# ERROR: Missing NRtg header in table 12 ("Miscellaneous Stats"),
# which causes all columns to the right to be "offset".
all_data <-
  seq_along(xml_tables) %>%
  # seq_along(xml_tables[11:13]) %>%
  # furrr::future_map_dfr(f) %>%
  purrr::map_dfr(f) # %>%
  # mutate(urlSeasonBREF = url)



all_data
# all_data %>% unnest(dataTable)
all_data %>% left_join(df_urls)
