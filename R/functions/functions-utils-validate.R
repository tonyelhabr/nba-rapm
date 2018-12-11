
.validate_raw_data_source <-
  function(x = .RAW_DATA_SOURCES) {
    # x <- match.arg(x)
    # .display_error(
    #   sprintf("\"%s\" is Not a valid `raw_data_source`.", raw_data_source)
    # )
    match.arg(x)
  }

.validate_season <-
  function(x = .SEASONS) {
    # match.arg(x) %>% names()
    stopifnot(any(x == .SEASONS))
    x
  }

.validate_season_type <-
  function(x = .SEASON_TYPES) {
    match.arg(x)
  }

.convert_season_type <-
  function(x = .SEASON_TYPES) {
    # switch(
    #   regular = "Regular Season",
    #   playoffs = "Playoffs",
    #   all = "All"
    # )
    case_when(
      x == "regular" ~ "Regular Season",
      x == "playoffs" ~ "Playoffs",
      TRUE ~ "All"
    )
  }

# Note that `side` isn't actually exposed to the user, so this validation
# is purely for the developer.
.validate_side <-
  function(x = .SIDES) {
    match.arg(x)
  }
