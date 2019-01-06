

# helfRlein::clean_gc()
# https://drive.google.com/drive/folders/1GMiP-3Aoh2AKFCoGZ8f0teMYNlkm87dm
tempfile <- tempfile(fileext = ".zip")
dir_game_summary_raw <-
  "https://drive.google.com/uc?export=download&id=1_DvoCC-p5vCCOB4q379AFXJTszeFpKie" %>%
  # archive::archive_read()
  download.file(destfile = tempfile)
tempfile_unzipped <- utils::unzip(tempfile, exdir = tempdir())

tempfile2 <- tempfile(fileext = ".zip")
dribble_raw <-
  "1_DvoCC-p5vCCOB4q379AFXJTszeFpKie" %>%
  googledrive::as_id() %>%
  googledrive::drive_download(path = tempfile2, overwrite = TRUE)
tempfile_unzipped <- utils::unzip(tempfile2, exdir = tempdir())
