
.validate_season <-
  function(x = .SEASONS) {
    stopifnot(length(x) == 1)
    stopifnot(any(x == .SEASONS))
    x
  }

.validate_season_type <-
  function(x = .SEASON_TYPES) {
    stopifnot(length(x) == 1)
    match.arg(x)
  }

.convert_season_type <-
  function(x = .SEASON_TYPES) {
    stopifnot(length(x) == 1)
    case_when(
      x == "regular" ~ "Regular Season",
      x == "playoffs" ~ "Playoffs",
      TRUE ~ "All"
    )
  }
