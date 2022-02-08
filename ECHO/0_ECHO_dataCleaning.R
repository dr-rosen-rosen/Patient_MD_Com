#############
#### ECHO Scripts for data cleaning


#open file
ECHO_LSM_MLM <- read_csv(here(config$ECHO_LSM_MLM_path, config$ECHO_LSM_MLM_name))

#making a race concordance variable "raceconc" between patient and provider race
ECHO_LSM_MLM <- ECHO_LSM_MLM %>%
  rowwise() %>%
  mutate(raceconc = if_else(racecat2== provrace, 1, 0))
# Get complete cases

Ha_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, racecat2, raceconc, 
                cultdissmd, cultdiss, 
                cdspeak,cdreason,cdstyle,cdvalue,cdspirit,cdethnic,cdtype,cdrace,cdculture,cdskin,
                cultdissmd1, cultdissmd2, cultdissmd3, cultdissmd4, cultdissmd5, cultdissmd6, cultdissmd7, cultdissmd8, cultdissmd9, cultdissmd10) %>%
  tidyr::drop_na() %>% # take only rows with no NA;
  mutate(
    cdAvg_dist = abs(cultdiss - cultdissmd),
    cdspeak_dist = abs(cdspeak - cultdissmd1),
    cdreason_dist = abs(cdreason - cultdissmd2),
    cdstyle_dist = abs(cdstyle - cultdissmd3),
    cdvalue_dist = abs(cdvalue - cultdissmd4),
    cdspirit_dist = abs(cdspirit - cultdissmd5),
    cdethnic_dist = abs(cdethnic - cultdissmd6),
    cdtype_dist = abs(cdtype - cultdissmd7),
    cdrace_dist = abs(cdrace - cultdissmd8),
    cdculture_dist = abs(cdculture - cultdissmd9),
    cdskin_dist = abs(cdskin - cultdissmd10)
  ) %>%
  # mutate(across(!provider_id & !racecat2 & !raceconc, ~scale(.,center = TRUE, scale = TRUE))) %>%
  mutate(provider_id = factor(provider_id)) %>%
  mutate(racecat2 = factor(racecat2)) %>%
  mutate(raceconc = factor(raceconc))

# drop physicians with < n cases
Ha_df <- Ha_df %>%
  group_by(provider_id) %>%
  filter(n() > 5) %>%
  ungroup() %>%
  gdata::drop.levels(.)

nrow(Ha_df)
hist(Ha_df$cdspeak_dichoto)