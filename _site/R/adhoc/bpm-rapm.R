
players_summary <-
  .try_import_players_summary_nbastatr(season = .SEASON)
players_summary
bpm_raw <-
  players_summary %>%
  select(id = id_player_nba, name = name_player, matches("ratio.*pm$")) %>%
  rename_at(vars(matches("ratio.*pm$")), funs(str_remove(., "ratio_")))
bpm_raw %>% arrange(desc(bpm))
bpm_raw %>% arrange(bpm)
bpm_filt <-
  bpm_raw %>%
  filter(bpm > -8, bpm < 15)

rapm_coefs <- .try_import_rapm_coefs(season = .SEASON)

# rapm_coefs %>% anti_join(bpm_filt)
# bpm_filt %>% anti_join(rapm_coefs)

rapm_bpm_join <-
  rapm_coefs %>%
  left_join(bpm_filt, by = c("id", "name"))
rapm_bpm_join

rpm_espn_combined <- .try_import_rpm_espn_combined()
rapm_szou_combined <- .try_import_rapm_szou_combined(season = .SEASON)

fmla <- formula(rapm ~ bpm + 0)
rapm_bpm_join %>%
  lm(formula(rapm ~ bpm + 0), data = .) %>%
  broom::tidy()
rapm_szou %>%
  left_join(bpm_filt, by = "name") %>%
  lm(formula(rapm ~ bpm + 0), data = .) %>%
  broom::tidy()
rpm_espn %>%
  left_join(bpm_filt, by = "name") %>%
  lm(formula(rpm ~ bpm + 0), data = .) %>%
  broom::tidy()
