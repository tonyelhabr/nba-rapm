


# Reference: https://stackoverflow.com/questions/39809117/how-to-post-api-in-r-having-header-json-body
# request_body <- data.frame(
#   language = c("en", "en"),
#   id = c("1", "2"),
#   text = c(
#     "This is wasted! I'm angry",
#     "This is awesome! Good Job Team! appreciated"
#   )
# )
# request_body_json <-
#   jsonlite::toJSON(
#     list(documents = request_body),
#     auto_unbox = TRUE
#   )


.download_rapm_rd <-
  function(..., path) {

    req_body <- data.frame(
      # players = list(),
      # teams = list(),
      seasons = c("2017-18")
    )
    req_body_json <-
      jsonlite::toJSON(
        x = list(params = req_body),
        # pretty = TRUE
        pretty = FALSE
      )
    req_body_json

    resp <-
      httr::POST(
        url = "https://7enmqppfr7.execute-api.us-east-1.amazonaws.com/dev/fourfactors",
        encode = "json",
        httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/537.36"),
        # httr::add_headers(
        #   `Referer` = "http://nbashotcharts.com/rapm?id=-1106328546"
        # ),
        body = req_body_json
      )
    # This works....
    # resp <-
    #   "https://7enmqppfr7.execute-api.us-east-1.amazonaws.com/dev/players" %>%
    #   httr::GET()
    httr::warn_for_status(resp)
    cont_raw <-
      resp %>%
      httr::content()
    data_raw <-
      cont_raw %>%
      unlist() %>%
      tibble::enframe()
  }
