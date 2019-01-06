
# other ----
# game_logs <- nbastatR::get_game_logs(seasons = 2018, result_types = "player")
# game_logs

url <- "https://data.nba.com/data/10s/v2015/json/mobile_teams/nba/2018/scores/data/0021800306_full_data.json"
# json_raw <- url%>% jsonlite::read_json()
resp <- url %>% httr::GET()
resp

cont_raw <- httr::content(resp)
str(cont_raw, max.level = 3, list.len = 4)

data_raw <-
  cont_raw %>%
  unlist() %>%
  enframe()
data_raw
rgx_split <- "\\."
n_cols_max <-
  data_raw %>%
  pull(name) %>%
  str_split(rgx_split) %>%
  map_dbl(~length(.)) %>%
  max()
n_cols_max

nms_sep <- paste0("name", 1:n_cols_max)
data_sep <-
  data_raw %>%
  separate(name, into = nms_sep, sep = rgx_split, fill = "right")
data_sep

data_sep %>% count(name4, sort = TRUE)
data_sep %>% filter(name4 == "opid")
data_sep %>% filter(name4 == "de")
data %>% select(descriptionPlayHome, descriptionPlayVisitor)
