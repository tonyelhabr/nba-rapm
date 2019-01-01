

# WARNING: This is experimental!
# Reference: https://stackoverflow.com/questions/19253820/how-to-implement-coalesce-efficiently-in-r
.coalesce <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
    list(...))
}
.coalesce_multi <- function(...) {
  Reduce(.coalesce, list(...))
}
# Reference: https://stackoverflow.com/questions/12232041/how-to-reorder-data-table-columns-without-copying
.move_col_to_first <- function(dt, col) {
  setcolorder(dt, neworder = c(col, setdiff(colnames(dt), col)))
}

# data.table ----
# .separate_lineup.dt <- function(dt, col, prefix = "x", suffix = 1:5, sep = "-") {
#   col <- enquo(col)
#   cols_new <- paste0(prefix, suffix)
#   dt[, (cols_new) := tstrsplit(lineup_o, "-", fixed = FALSE)]
# }
# .separate_lineup.dt(dt)[]

.munge_pbp.data.table <-
  function(pbp, ...) {

    library("data.table")
    dt <- .import_data_from_path(season = .SEASON, path = config$path_pbp)
    # dt <- setkey(as.data.table(dt), rn)
    dt <- as.data.table(dt)
    # class(dt)
    dt <- dt[id_game == 21700001]
    dt <-
      merge(
        dt[
          is_off1 == TRUE,
          .(rn1 = rn,
            id_game,
            is_home = is_home1,
            # is_off = is_off1,
            pts = pts1,
            mp,
            id_team_o = id_team1,
            id_team_d = id_team2,
            slug_o = slug_team1,
            slug_d = slug_team2,
            lineup_o = lineup1,
            lineup_d = lineup2
          )],
        dt[,c("is_home2", "is_off2") := .(!is_home1, !is_off1)]
        [is_off1 == FALSE,
          .(rn2 = rn,
            id_game,
            is_home = is_home2,
            # is_off = is_off2,
            pts = pts2,
            mp,
            id_team_o = id_team2,
            id_team_d = id_team1,
            slug_o = slug_team2,
            slug_d = slug_team1,
            lineup_o = lineup2,
            lineup_d = lineup1
          )],
        all = TRUE
      )

    dt <-
      dt[,
         rn := .coalesce(rn1, rn2)
         ][,
           `:=`(rn1 = NULL, rn2 = NULL)
           ][
             order(rn)
             ]
    # dt <- .move_col_to_first(dt, "rn")
    dt <- setkey(dt, rn)
    # dt[, .I]
    dt <- dt[, (paste0("x", 1:5)) := tstrsplit(lineup_o, "-", fixed = FALSE)]
    dt <- dt[, (paste0("x", 6:10)) := tstrsplit(lineup_d, "-", fixed = FALSE)]
    dt <- dt[,`:=`(lineup_o = NULL, lineup_d = NULL)]
    dt[]
    dt <- setDT(dt)
  }
