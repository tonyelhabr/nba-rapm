
paths_rename <-
  list.files(
    path = "data",
    pattern = "szou",
    recursive = TRUE,
    full.names = TRUE
  )
paths_rename
paths_rename %>%
  purrr::walk(
    ~file.rename(from = .x, to = str_replace(.x, "szou", "sz"))
  )
