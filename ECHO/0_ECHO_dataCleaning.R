#############
#### ECHO Scripts for data cleaning

#open file
ECHO_LSM_MLM <- read_csv(here(config$ECHO_LSM_MLM_path, config$ECHO_LSM_MLM_name))

#making a race concordance variable "raceconc" between patient and provider race
ECHO_LSM_MLM <- ECHO_LSM_MLM %>%
  rowwise() %>%
  mutate(raceconc = if_else(racecat2== provrace, 1, 0)) #%>%
 #running the code below resulted in "NaN" in all cells...tried fixing it but was unsuccessful
 #mutate(LSM_function_mean_scaled = scale(LSM_function_mean, center = TRUE, scale = TRUE))
  
#Instead of attempt above, did scaling for LSM_function_mean using this method below...the values seem to be right
#when I calculate values manually ((x- mean(x))/ sd(x))
ECHO_LSM_MLM$LSM_function_mean_scaled <- scale(ECHO_LSM_MLM$LSM_function_mean)



H1.1_df <- ECHO_LSM_MLM %>%
  dplyr::select(provider_id, LSM_function_mean, racecat2) %>%
  dplyr::filter(racecat2 !=4) %>%
  #added levels to the factor because the order without it was 2, 1, 3
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  #mutate(across(!provider_id & !racecat2, ~scale(.,center = TRUE, scale = TRUE))) %>%
  tidyr::drop_na()
  
H1.2_df <- ECHO_LSM_MLM %>%
  dplyr::select(provider_id, LSM_function_mean, raceconc) %>%
  mutate(raceconc = factor(raceconc)) %>%
  tidyr::drop_na()

#need to add the dissimilarity distance variable
H1.3_df <- ECHO_LSM_MLM %>%
  dplyr::select(provider_id, LSM_function_mean, 
                cultdiss, cultdissmd,
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
  )

H3a.1_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, provcomm) %>%
  tidyr::drop_na()
  

H3a.2_df <- ECHO_LSM_MLM %>%
  dplyr::select( provider_id, LSM_function_mean, overcomm) %>%
  tidyr::drop_na()


H3a.3_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, ipstyle) %>%
  tidyr::drop_na()


H3a.4_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, iptrust) %>%
  tidyr::drop_na()


H3a.5_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, provknowcat) %>%
  tidyr::drop_na()


H3a.6_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, overallsat) %>%
  tidyr::drop_na()


H3b.1 <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, vlsup75) %>%
  tidyr::drop_na()
