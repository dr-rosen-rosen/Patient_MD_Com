#########################################
#### turn by turn LSM
#########################################
library(tidyverse)

#calculating rLSM without excluding any of the turns

# read in turn by turn LIWC file
df_tbyt <- read_csv(here(config$ECHO_LSM_TbyT_Smoothed_path, config$ECHO_LSM_TbyT_Smoothed_name))

df_tbyt <- df_tbyt %>%
  rename(output_order = A) %>%
  rename(File = B) %>%
  rename(Speaker = C) %>%
  rename(Text = D) %>%
  select(-E) %>%
  select(-F)

# test <- smoothed_tByT_df %>%
#   dplyr::select(File, Speaker) %>%
#   group_by(File) %>%
#   mutate(
#     Speaker.lag = lag(Speaker)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     speaker_match = if_else(Speaker == Speaker.lag,1,0)
#   )
# table(test$speaker_match)

#SK: had to delete sequence from the list right below with select()
rLSM_df_allturns <- df_tbyt %>%
  dplyr::select(File, Text, Speaker, WC, WPS, auxverb, article, adverb, ipron, 
                prep, negate, conj, quant, ppron) %>%
  # Adding quick way to drop turn by turns from the same speaker
  group_by(File) %>%
  mutate(
    Speaker.lag = lag(Speaker)
  ) %>%
  ungroup() %>%
  mutate(
    speaker_match = if_else(Speaker == Speaker.lag,1,0)
  ) %>%
  ungroup() %>%
  #filter(speaker_match == 0) %>%
  # End dropping same speaker turns
  
  # group_by(File) %>%
  # mutate(
  #   auxverb.orig = auxverb,
  #   article.orig = article,
  #   adverb.orig = adverb,
  #   ipron.orig = ipron,
  #   prep.orig = prep,
  #   negate.orig = negate,
  #   conj.orig = conj,
  #   quant.orig = quant,
  #   ppron.orig = ppron,
  #   WC.orig = WC) %>%
  # ungroup() %>%
  #rowwise() %>%
  # This puts the turn before the current one on the same row with a .lag suffix
  #dplyr::mutate(across(.cols=everything(), .funs = ~ dplyr::lead(.x,order_by=File,n = 1, default = NA), .names = '{.col}_lead')) %>%
  group_by(File) %>%
  mutate(
    auxverb.lag = lag(auxverb),
    article.lag = lag(article),
    adverb.lag = lag(adverb),
    ipron.lag = lag(ipron),
    prep.lag = lag(prep),
    negate.lag = lag(negate),
    conj.lag = lag(conj),
    quant.lag = lag(quant),
    ppron.lag = lag(ppron),
    WC.lag = lag(WC)) %>%
  ungroup() %>%
  # filter(!(WC == 1 | WC.lag == 1)) %>% # drops all exchanges with one word utterances
  # This makes sure that only liwc categories prersent in the first statement are used for rlsm
  mutate(across(c(auxverb.lag, article.lag, adverb.lag, ipron.lag, 
                  prep.lag, negate.lag, conj.lag, quant.lag, ppron.lag), 
                ~ if_else(. > 0,.,as.numeric(NA)))) %>%
  mutate(
    auxverb = if_else(is.na(auxverb.lag),as.numeric(NA),auxverb),
    article = if_else(is.na(article.lag),as.numeric(NA),article),
    adverb = if_else(is.na(adverb.lag),as.numeric(NA),adverb),
    ipron = if_else(is.na(ipron.lag),as.numeric(NA),ipron),
    prep = if_else(is.na(prep.lag),as.numeric(NA),prep),
    negate = if_else(is.na(negate.lag),as.numeric(NA),negate),
    conj = if_else(is.na(conj.lag),as.numeric(NA),conj),
    quant = if_else(is.na(quant.lag),as.numeric(NA),quant),
    ppron = if_else(is.na(ppron.lag),as.numeric(NA),ppron)
  ) %>%
  rowwise() %>%
  mutate(
    auxverb.rLSM = 1 - (abs(auxverb - auxverb.lag) / (auxverb + auxverb.lag + .0001)),
    article.rLSM = 1 - (abs(article - article.lag) / (article + article.lag + .0001)),
    adverb.rLSM = 1 - (abs(adverb - adverb.lag) / (adverb + adverb.lag + .0001)),
    ipron.rLSM = 1 - (abs(ipron - ipron.lag) / (ipron + ipron.lag + .0001)),
    prep.rLSM = 1 - (abs(prep - prep.lag) / (prep + prep.lag + .0001)),
    negate.rLSM = 1 - (abs(negate - negate.lag) / (negate + negate.lag + .0001)),
    conj.rLSM = 1 - (abs(conj - conj.lag) / (conj + conj.lag + .0001)),
    quant.rLSM = 1 - (abs(quant - quant.lag) / (quant + quant.lag + .0001)),
    ppron.rLSM = 1 - (abs(ppron - ppron.lag) / (ppron + ppron.lag + .0001))
  ) %>%
  ungroup() %>%
  # creates an average rLSM across separrate featurers
  #mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
  # mutate(rLSM = ifelse(is.na(rLSM), 0, rLSM)) %>% # replaces NAs with 0
  # group_by(File,Speaker) %>%
  # summarize(rLSM = mean(rLSM)) %>%
  # ungroup() %>%
  group_by(File,Speaker) %>%
  summarize(
    across(contains('.rLSM'), .fns = ~ mean(.x,na.rm=TRUE)),
    WC_sum = sum(WC),
    WPS_avg = mean(WPS)) %>%
  ungroup() %>%
  mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
  pivot_wider(
    id_cols = File, 
    names_from = Speaker, 
    values_from = c(rLSM,WPS_avg,WC_sum), 
    # names_prefix = 'rLSM.'
    names_glue = "{.value}.{Speaker}"
  ) %>%
  drop_na() %>% # find out where these are coming from
  mutate(
    mean.rLSM = rowMeans(select(.,contains('rLSM')),na.rm = TRUE),
    ratio.rLSM = rLSM.D / rLSM.P,
    verb_dom = WC_sum.D / WC_sum.P
  ) %>%
  filter(
    WC_sum.D >= 50 & WC_sum.P >= 50
  )


# # Transcript OC08P06 moved from 5th highest rLSM.D to 6th place
# ECHO_transcript_topbottom5_rLSM <- c("JC02P02", "SC24P107", "JC05P05", 
#                                      "OC02P04", "JC06P01", "SC19P045", 
#                                      "JC14P02", "OC06P01", "OC06P10", 
#                                      "JC08P08")
# 
# 
# #filtering out the list of transcripts in "excluded_transcript" df
# rLSM_df_allturns <-rLSM_df_allturns %>% 
#   filter(File %in% ECHO_transcript_topbottom5_rLSM)
# 
# 
# rLSM_df_allturns <- rLSM_df_allturns %>%
#   select(-(WPS)) %>%
#   relocate(auxverb.orig:ppron.orig, .before = auxverb)
# 
# 
# write.csv(rLSM_df_allturns, "rLSM_df_allturns.csv")



####################################################################################################
#turnbyturn code for removing one and two word turns
  
  df_tbyt_V2 <- read_csv(here(config$ECHO_LSM_TbyT_Smoothed_V2_path, config$ECHO_LSM_TbyT_Smoothed_V2_name))
  
  df_tbyt_V2 <- df_tbyt_V2 %>%
    rename(output_order = A) %>%
    rename(File = B) %>%
    rename(Speaker = C) %>%
    rename(Text = D) %>%
    select(-E) %>%
    select(-F)
  
  # test <- smoothed_tByT_df %>%
  #   dplyr::select(File, Speaker) %>%
  #   group_by(File) %>%
  #   mutate(
  #     Speaker.lag = lag(Speaker)
  #   ) %>%
  #   ungroup() %>%
  #   mutate(
  #     speaker_match = if_else(Speaker == Speaker.lag,1,0)
  #   )
  # table(test$speaker_match)
  

  ECHO_turn_exclusions_rLSM <- df_tbyt_V2 %>%
    dplyr::select(File, Text, Speaker, WC, WPS, auxverb, article, adverb, ipron, 
                  prep, negate, conj, quant, ppron) %>%
    # Adding quick way to drop turn by turns from the same speaker
    ### WE NEED TO THINK ABOUT FLOW. WHERE WE"RE DOING SMOOTHING, ETC> (here or in cleaning)
    group_by(File) %>%
    mutate(
      Speaker.lag = lag(Speaker)
    ) %>%
    ungroup() %>%
    mutate(
      speaker_match = if_else(Speaker == Speaker.lag,1,0)
    ) %>%
    ungroup() %>%
    #filter(speaker_match == 0) %>%
    # End dropping same speaker turns
    
    group_by(File) %>%
    mutate(
      auxverb.orig = auxverb,
      article.orig = article,
      adverb.orig = adverb,
      ipron.orig = ipron,
      prep.orig = prep,
      negate.orig = negate,
      conj.orig = conj,
      quant.orig = quant,
      ppron.orig = ppron,
      WC.orig = WC) %>%
    ungroup() %>%
    #rowwise() %>%
    # This puts the turn before the current one on the same row with a .lag suffix
    #dplyr::mutate(across(.cols=everything(), .funs = ~ dplyr::lead(.x,order_by=File,n = 1, default = NA), .names = '{.col}_lead')) %>%
    group_by(File) %>%
    mutate(
      auxverb.lag = lag(auxverb),
      article.lag = lag(article),
      adverb.lag = lag(adverb),
      ipron.lag = lag(ipron),
      prep.lag = lag(prep),
      negate.lag = lag(negate),
      conj.lag = lag(conj),
      quant.lag = lag(quant),
      ppron.lag = lag(ppron),
      WC.lag = lag(WC)) %>%
    ungroup() %>%
    # filter(WC > 1 & WC.lag >1) %>% # drops all exchanges with one word utterances
    # This makes sure that only liwc categories prersent in the first statement are used for rlsm
    mutate(across(c(auxverb.lag, article.lag, adverb.lag, ipron.lag, 
                    prep.lag, negate.lag, conj.lag, quant.lag, ppron.lag), 
                  ~ if_else(. > 0,.,as.numeric(NA)))) %>%
    # This sets liwc categories in the responders speech to NA if that category was NA in the first person's speech
    # Per the rLSM paper
    group_by(File) %>%
    mutate(
      auxverb = if_else(is.na(auxverb.lag),as.numeric(NA),auxverb),
      article = if_else(is.na(article.lag),as.numeric(NA),article),
      adverb = if_else(is.na(adverb.lag),as.numeric(NA),adverb),
      ipron = if_else(is.na(ipron.lag),as.numeric(NA),ipron),
      prep = if_else(is.na(prep.lag),as.numeric(NA),prep),
      negate = if_else(is.na(negate.lag),as.numeric(NA),negate),
      conj = if_else(is.na(conj.lag),as.numeric(NA),conj),
      quant = if_else(is.na(quant.lag),as.numeric(NA),quant),
      ppron = if_else(is.na(ppron.lag),as.numeric(NA),ppron)
    ) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      auxverb.rLSM = 1 - (abs(auxverb - auxverb.lag) / (auxverb + auxverb.lag + .0001)),
      article.rLSM = 1 - (abs(article - article.lag) / (article + article.lag + .0001)),
      adverb.rLSM = 1 - (abs(adverb - adverb.lag) / (adverb + adverb.lag + .0001)),
      ipron.rLSM = 1 - (abs(ipron - ipron.lag) / (ipron + ipron.lag + .0001)),
      prep.rLSM = 1 - (abs(prep - prep.lag) / (prep + prep.lag + .0001)),
      negate.rLSM = 1 - (abs(negate - negate.lag) / (negate + negate.lag + .0001)),
      conj.rLSM = 1 - (abs(conj - conj.lag) / (conj + conj.lag + .0001)),
      quant.rLSM = 1 - (abs(quant - quant.lag) / (quant + quant.lag + .0001)),
      ppron.rLSM = 1 - (abs(ppron - ppron.lag) / (ppron + ppron.lag + .0001))
    ) %>%
    ungroup() %>%
    #creates an average rLSM across separrate featurers
    #mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
    # mutate(rLSM = ifelse(is.na(rLSM), 0, rLSM)) %>% # replaces NAs with 0
    # group_by(File,Speaker) %>%
    # summarize(rLSM = mean(rLSM)) %>%
    # ungroup() %>%
    group_by(File,Speaker) %>%
    summarize(
      across(contains('.rLSM'), .fns = ~ mean(.x,na.rm=TRUE)),
      WC_sum = sum(WC),
      WPS_avg = mean(WPS)) %>%
    ungroup() %>%
    mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
    pivot_wider(
      id_cols = File,
      names_from = Speaker,
      values_from = c(rLSM,WPS_avg,WC_sum),
      # names_prefix = 'rLSM.'
      names_glue = "{.value}.{Speaker}"
    ) %>%
    drop_na() %>% # find out where these are coming from
    mutate(
      mean.rLSM = rowMeans(select(.,contains('rLSM')),na.rm = TRUE),
      ratio.rLSM = rLSM.D / rLSM.P,
      verb_dom = WC_sum.D / WC_sum.P
    ) %>%
    filter(
      WC_sum.D >= 50 & WC_sum.P >= 50
    )
  
  
  # ECHO_transcript_topbottom5_rLSM_V2 <- c("JC02P03", "JC06P01", "JC05P05", 
  #                                      "OC08P10", "SC12P062", "SC15P050", 
  #                                      "SC23P064", "OC06P10", "SC15P071", 
  #                                      "JC08P08")
  # 
  # 
  # #filtering out the list of transcripts in "excluded_transcript" df
  # rLSM_df_turnexclusions <-rLSM_df_V2 %>% 
  #   filter(File %in% ECHO_transcript_topbottom5_rLSM_V2)
  # 
  # 
  # rLSM_df_turnexclusions <- rLSM_df_turnexclusions %>%
  #   select(-(WPS)) %>%
  #   relocate(auxverb.orig:ppron.orig, .before = auxverb)
  # 
  # readr::write_csv(rLSM_df_turnexclusions , 'rLSM_df_turnexclusions.csv')
  
  
  
  #################################################################################################################
  ##creating tbyt matching score for LIWC variables  
  ################################################################################################################# 
  
  
  
  df_tbyt_LIWC <- read_csv(here(config$ECHO_LSM_TbyT_Smoothed_V2_path, config$ECHO_LSM_TbyT_Smoothed_V2_name))
  
  df_tbyt_LIWC <- df_tbyt_LIWC %>%
    rename(output_order = A) %>%
    rename(File = B) %>%
    rename(Speaker = C) %>%
    rename(Text = D) %>%
    select(-E) %>%
    select(-F)
  
  # test <- smoothed_tByT_df %>%
  #   dplyr::select(File, Speaker) %>%
  #   group_by(File) %>%
  #   mutate(
  #     Speaker.lag = lag(Speaker)
  #   ) %>%
  #   ungroup() %>%
  #   mutate(
  #     speaker_match = if_else(Speaker == Speaker.lag,1,0)
  #   )
  # table(test$speaker_match)
  
  #SK: had to delete sequence from the list right below with select()
  ECHO_tbyt_LIWC_matching <- df_tbyt_LIWC %>%
    dplyr::select(File, Text, Speaker, affect, social, cogproc, negemo, percept,
                  bio, drives, relativ, informal) %>%
    # Adding quick way to drop turn by turns from the same speaker
    ### WE NEED TO THINK ABOUT FLOW. WHERE WE"RE DOING SMOOTHING, ETC> (here or in cleaning)
    group_by(File) %>%
    mutate(
      Speaker.lag = lag(Speaker)
    ) %>%
    ungroup() %>%
    mutate(
      speaker_match = if_else(Speaker == Speaker.lag,1,0)
    ) %>%
    ungroup() %>%
    filter(speaker_match == 0) %>%
    # End dropping same speaker turns
    
    # group_by(File) %>%
    # mutate(
    #   auxverb.orig = auxverb,
    #   article.orig = article,
    #   adverb.orig = adverb,
    #   ipron.orig = ipron,
    #   prep.orig = prep,
    #   negate.orig = negate,
    #   conj.orig = conj,
    #   quant.orig = quant,
    #   ppron.orig = ppron,
    #   WC.orig = WC) %>%
    # ungroup() %>%
    #rowwise() %>%
    # This puts the turn before the current one on the same row with a .lag suffix
    #dplyr::mutate(across(.cols=everything(), .funs = ~ dplyr::lead(.x,order_by=File,n = 1, default = NA), .names = '{.col}_lead')) %>%
    group_by(File) %>%
    mutate(
      affect.lag = lag(affect), 
      social.lag = lag(social),
      cogproc.lag = lag(cogproc),
      negemo.lag = lag(negemo),
      percept.lag = lag(percept),
      bio.lag = lag(bio),
      drives.lag = lag(drives),
      relativ.lag = lag(relativ),
      informal.lag = lag(informal)
      ) %>%
    ungroup() %>%
    # filter(WC > 1 & WC.lag >1) %>% # drops all exchanges with one word utterances
    # This makes sure that only liwc categories prersent in the first statement are used for rlsm
    mutate(across(c(affect.lag, social.lag, cogproc.lag, negemo.lag, percept.lag,
                    bio.lag, drives.lag, relativ.lag, informal.lag), 
                  ~ if_else(. > 0,.,as.numeric(NA)))) %>%
    # This sets liwc categories in the responders speech to NA if that category was NA in the first person's speech
    # Per the rLSM paper
    group_by(File) %>%
    mutate(
      affect = if_else(is.na(affect.lag),as.numeric(NA),affect),
      social = if_else(is.na(social.lag),as.numeric(NA),social),
      cogproc = if_else(is.na(cogproc.lag),as.numeric(NA),cogproc),
      percept = if_else(is.na(percept.lag),as.numeric(NA),percept),
      negemo = if_else(is.na(negemo.lag),as.numeric(NA),negemo),
      bio = if_else(is.na(bio.lag),as.numeric(NA),bio),
      drives = if_else(is.na(drives.lag),as.numeric(NA),drives),
      relativ = if_else(is.na(relativ.lag),as.numeric(NA),relativ),
      informal = if_else(is.na(informal.lag),as.numeric(NA),informal)
    ) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      affect.tbytmatch = 1 - (abs(affect - affect.lag) / (affect + affect.lag + .0001)),
      social.tbytmatch = 1 - (abs(social - social.lag) / (social + social.lag + .0001)),
      cogproc.tbytmatch = 1 - (abs(cogproc - cogproc.lag) / (cogproc + cogproc.lag + .0001)),
      percept.tbytmatch = 1 - (abs(percept - percept.lag) / (percept + percept.lag + .0001)),
      negemo.tbytmatch = 1 - (abs(negemo - negemo.lag) / (negemo + negemo.lag + .0001)),
      bio.tbytmatch = 1 - (abs(bio - bio.lag) / (bio + bio.lag + .0001)),
      drives.tbytmatch = 1 - (abs(drives - drives.lag) / (drives + drives.lag + .0001)),
      relativ.tbytmatch = 1 - (abs(relativ - relativ.lag) / (relativ + relativ.lag + .0001)),
      informal.tbytmatch = 1 - (abs(informal - informal.lag) / (informal + informal.lag + .0001))
    ) %>%
    ungroup() %>%
    group_by(File,Speaker) %>%
    summarize(
      across(contains('.tbytmatch'), .fns = ~ mean(.x,na.rm=TRUE))) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = File,
      names_from = Speaker,
      values_from = c(affect.tbytmatch, social.tbytmatch, cogproc.tbytmatch, percept.tbytmatch,
                      negemo.tbytmatch, bio.tbytmatch, drives.tbytmatch, relativ.tbytmatch, informal.tbytmatch),
      names_glue = "{.value}.{Speaker}"
    )
    
  
#################################################################################################################
##creating tbyt matching score for chunks
################################################################################################################# 
ECHO_smoothed_chunks_wc_rLSM <- ECHO_smoothed_chunks_wc

  # test <- ECHO_smoothed_chunks_wc_rLSM %>%
  #   dplyr::select(File, Speaker) %>%
  #   group_by(File) %>%
  #   mutate(
  #     Speaker.lag = lag(Speaker)
  #   ) %>%
  #   ungroup() %>%
  #   mutate(
  #     speaker_match = if_else(Speaker == Speaker.lag,1,0)
  #   )
  # table(test$speaker_match)
  
  #SK: had to delete sequence from the list right below with select()
  ECHO_smoothed_chunks_wc_rLSM <- ECHO_smoothed_chunks_wc_rLSM %>%
    dplyr::select(File, Speaker, chunk, WC, auxverb, article, adverb, ipron, 
                  prep, negate, conj, quant, ppron) %>%
    # Adding quick way to drop turn by turns from the same speaker
    ### WE NEED TO THINK ABOUT FLOW. WHERE WE"RE DOING SMOOTHING, ETC> (here or in cleaning)
    # group_by(File) %>%
    # mutate(
    #   Speaker.lag = lag(Speaker)
    # ) %>%
    # ungroup() %>%
    # mutate(
    #   speaker_match = if_else(Speaker == Speaker.lag,1,0)
    # ) %>%
    # ungroup() %>%
    # filter(speaker_match == 0) %>%
    # End dropping same speaker turns
    
    # group_by(File, Chunk) %>%
    # mutate(
    #   auxverb.orig = auxverb,
    #   article.orig = article,
    #   adverb.orig = adverb,
    #   ipron.orig = ipron,
    #   prep.orig = prep,
    #   negate.orig = negate,
    #   conj.orig = conj,
    #   quant.orig = quant,
    #   ppron.orig = ppron,
    #   WC.orig = WC) %>%
    # ungroup() %>%
    #rowwise() %>%
    # This puts the turn before the current one on the same row with a .lag suffix
    #dplyr::mutate(across(.cols=everything(), .funs = ~ dplyr::lead(.x,order_by=File,n = 1, default = NA), .names = '{.col}_lead')) %>%
    group_by(File, chunk) %>%
    mutate(
      auxverb.lag = lag(auxverb),
      article.lag = lag(article),
      adverb.lag = lag(adverb),
      ipron.lag = lag(ipron),
      prep.lag = lag(prep),
      negate.lag = lag(negate),
      conj.lag = lag(conj),
      quant.lag = lag(quant),
      ppron.lag = lag(ppron),
      WC.lag = lag(WC)) %>%
    ungroup() %>%
    # filter(WC > 1 & WC.lag >1) %>% # drops all exchanges with one word utterances
    # This makes sure that only liwc categories prersent in the first statement are used for rlsm
    mutate(across(c(auxverb.lag, article.lag, adverb.lag, ipron.lag, 
                    prep.lag, negate.lag, conj.lag, quant.lag, ppron.lag), 
                  ~ if_else(. > 0,.,as.numeric(NA)))) %>%
    # This sets liwc categories in the responders speech to NA if that category was NA in the first person's speech
    # Per the rLSM paper
    group_by(File, chunk) %>%
    mutate(
      auxverb = if_else(is.na(auxverb.lag),as.numeric(NA),auxverb),
      article = if_else(is.na(article.lag),as.numeric(NA),article),
      adverb = if_else(is.na(adverb.lag),as.numeric(NA),adverb),
      ipron = if_else(is.na(ipron.lag),as.numeric(NA),ipron),
      prep = if_else(is.na(prep.lag),as.numeric(NA),prep),
      negate = if_else(is.na(negate.lag),as.numeric(NA),negate),
      conj = if_else(is.na(conj.lag),as.numeric(NA),conj),
      quant = if_else(is.na(quant.lag),as.numeric(NA),quant),
      ppron = if_else(is.na(ppron.lag),as.numeric(NA),ppron)
    ) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      auxverb.rLSM = 1 - (abs(auxverb - auxverb.lag) / (auxverb + auxverb.lag + .0001)),
      article.rLSM = 1 - (abs(article - article.lag) / (article + article.lag + .0001)),
      adverb.rLSM = 1 - (abs(adverb - adverb.lag) / (adverb + adverb.lag + .0001)),
      ipron.rLSM = 1 - (abs(ipron - ipron.lag) / (ipron + ipron.lag + .0001)),
      prep.rLSM = 1 - (abs(prep - prep.lag) / (prep + prep.lag + .0001)),
      negate.rLSM = 1 - (abs(negate - negate.lag) / (negate + negate.lag + .0001)),
      conj.rLSM = 1 - (abs(conj - conj.lag) / (conj + conj.lag + .0001)),
      quant.rLSM = 1 - (abs(quant - quant.lag) / (quant + quant.lag + .0001)),
      ppron.rLSM = 1 - (abs(ppron - ppron.lag) / (ppron + ppron.lag + .0001))
    ) %>%
    ungroup() %>%
    #creates an average rLSM across separrate featurers
    #mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
    # mutate(rLSM = ifelse(is.na(rLSM), 0, rLSM)) %>% # replaces NAs with 0
    # group_by(File,Speaker) %>%
    # summarize(rLSM = mean(rLSM)) %>%
    # ungroup() %>%
    group_by(File, Speaker, chunk) %>%
    summarize(
      across(contains('.rLSM'), .fns = ~ mean(.x,na.rm=TRUE)),
      WC_sum = sum(WC)) %>%
    ungroup() %>%
    mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
    pivot_wider(
      id_cols = File,
      names_from = c(chunk,Speaker),
      values_from = c(rLSM,WC_sum),
      # names_prefix = 'rLSM.'
      names_glue = "{.value}.{Speaker}.{chunk}"
    )  %>%
    rowwise()%>%
    mutate(
      mean.rLSM.1 = mean(c(rLSM.D.1, rLSM.P.1)),
      ratio.rLSM.1 = rLSM.D.1 / rLSM.P.1,
      verb_dom.1 = WC_sum.D.1 / WC_sum.P.1,
      mean.rLSM.2 = mean(c(rLSM.D.2, rLSM.P.2)),
      ratio.rLSM.2 = rLSM.D.2 / rLSM.P.2,
      verb_dom.2 = WC_sum.D.2 / WC_sum.P.2,
      mean.rLSM.3 = mean(c(rLSM.D.3, rLSM.P.3)),
      ratio.rLSM.3 = rLSM.D.3 / rLSM.P.3,
      verb_dom.3 = WC_sum.D.3 / WC_sum.P.3) %>%
  # %>%
  #   filter(
  #     WC_sum.D >= 50 & WC_sum.P >= 50
  #   )
    #adding "_wc" at the end of variables since this chunking was based on word count
    rename_at(vars(-(File)), ~ paste0(., '_wc'))
    
 

  
  
  
  ECHO_smoothed_chunks_turns_rLSM <- ECHO_smoothed_chunks_turns
  
  # test <- ECHO_smoothed_chunks_wc_rLSM %>%
  #   dplyr::select(File, Speaker) %>%
  #   group_by(File) %>%
  #   mutate(
  #     Speaker.lag = lag(Speaker)
  #   ) %>%
  #   ungroup() %>%
  #   mutate(
  #     speaker_match = if_else(Speaker == Speaker.lag,1,0)
  #   )
  # table(test$speaker_match)
  
  #SK: had to delete sequence from the list right below with select()
  ECHO_smoothed_chunks_turns_rLSM <- ECHO_smoothed_chunks_turns_rLSM %>%
    dplyr::select(File, Speaker, chunk, WC, auxverb, article, adverb, ipron, 
                  prep, negate, conj, quant, ppron) %>%
    # Adding quick way to drop turn by turns from the same speaker
    ### WE NEED TO THINK ABOUT FLOW. WHERE WE"RE DOING SMOOTHING, ETC> (here or in cleaning)
    # group_by(File) %>%
    # mutate(
    #   Speaker.lag = lag(Speaker)
    # ) %>%
    # ungroup() %>%
    # mutate(
    #   speaker_match = if_else(Speaker == Speaker.lag,1,0)
    # ) %>%
    # ungroup() %>%
  # filter(speaker_match == 0) %>%
  # End dropping same speaker turns
  
  # group_by(File, Chunk) %>%
  # mutate(
  #   auxverb.orig = auxverb,
  #   article.orig = article,
  #   adverb.orig = adverb,
  #   ipron.orig = ipron,
  #   prep.orig = prep,
  #   negate.orig = negate,
  #   conj.orig = conj,
  #   quant.orig = quant,
  #   ppron.orig = ppron,
  #   WC.orig = WC) %>%
  # ungroup() %>%
  #rowwise() %>%
  # This puts the turn before the current one on the same row with a .lag suffix
  #dplyr::mutate(across(.cols=everything(), .funs = ~ dplyr::lead(.x,order_by=File,n = 1, default = NA), .names = '{.col}_lead')) %>%
  group_by(File, chunk) %>%
    mutate(
      auxverb.lag = lag(auxverb),
      article.lag = lag(article),
      adverb.lag = lag(adverb),
      ipron.lag = lag(ipron),
      prep.lag = lag(prep),
      negate.lag = lag(negate),
      conj.lag = lag(conj),
      quant.lag = lag(quant),
      ppron.lag = lag(ppron),
      WC.lag = lag(WC)) %>%
    ungroup() %>%
    # filter(WC > 1 & WC.lag >1) %>% # drops all exchanges with one word utterances
    # This makes sure that only liwc categories prersent in the first statement are used for rlsm
    mutate(across(c(auxverb.lag, article.lag, adverb.lag, ipron.lag, 
                    prep.lag, negate.lag, conj.lag, quant.lag, ppron.lag), 
                  ~ if_else(. > 0,.,as.numeric(NA)))) %>%
    # This sets liwc categories in the responders speech to NA if that category was NA in the first person's speech
    # Per the rLSM paper
    group_by(File, chunk) %>%
    mutate(
      auxverb = if_else(is.na(auxverb.lag),as.numeric(NA),auxverb),
      article = if_else(is.na(article.lag),as.numeric(NA),article),
      adverb = if_else(is.na(adverb.lag),as.numeric(NA),adverb),
      ipron = if_else(is.na(ipron.lag),as.numeric(NA),ipron),
      prep = if_else(is.na(prep.lag),as.numeric(NA),prep),
      negate = if_else(is.na(negate.lag),as.numeric(NA),negate),
      conj = if_else(is.na(conj.lag),as.numeric(NA),conj),
      quant = if_else(is.na(quant.lag),as.numeric(NA),quant),
      ppron = if_else(is.na(ppron.lag),as.numeric(NA),ppron)
    ) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      auxverb.rLSM = 1 - (abs(auxverb - auxverb.lag) / (auxverb + auxverb.lag + .0001)),
      article.rLSM = 1 - (abs(article - article.lag) / (article + article.lag + .0001)),
      adverb.rLSM = 1 - (abs(adverb - adverb.lag) / (adverb + adverb.lag + .0001)),
      ipron.rLSM = 1 - (abs(ipron - ipron.lag) / (ipron + ipron.lag + .0001)),
      prep.rLSM = 1 - (abs(prep - prep.lag) / (prep + prep.lag + .0001)),
      negate.rLSM = 1 - (abs(negate - negate.lag) / (negate + negate.lag + .0001)),
      conj.rLSM = 1 - (abs(conj - conj.lag) / (conj + conj.lag + .0001)),
      quant.rLSM = 1 - (abs(quant - quant.lag) / (quant + quant.lag + .0001)),
      ppron.rLSM = 1 - (abs(ppron - ppron.lag) / (ppron + ppron.lag + .0001))
    ) %>%
    ungroup() %>%
    #creates an average rLSM across separrate featurers
    #mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
    # mutate(rLSM = ifelse(is.na(rLSM), 0, rLSM)) %>% # replaces NAs with 0
    # group_by(File,Speaker) %>%
    # summarize(rLSM = mean(rLSM)) %>%
    # ungroup() %>%
    group_by(File, Speaker, chunk) %>%
    summarize(
      across(contains('.rLSM'), .fns = ~ mean(.x,na.rm=TRUE)),
      WC_sum = sum(WC)) %>%
    ungroup() %>%
    mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
    pivot_wider(
      id_cols = File,
      names_from = c(chunk,Speaker),
      values_from = c(rLSM,WC_sum),
      # names_prefix = 'rLSM.'
      names_glue = "{.value}.{Speaker}.{chunk}"
    )  %>%
    rowwise()%>%
    mutate(
      mean.rLSM.1 = mean(c(rLSM.D.1, rLSM.P.1)),
      ratio.rLSM.1 = rLSM.D.1 / rLSM.P.1,
      verb_dom.1 = WC_sum.D.1 / WC_sum.P.1,
      mean.rLSM.2 = mean(c(rLSM.D.2, rLSM.P.2)),
      ratio.rLSM.2 = rLSM.D.2 / rLSM.P.2,
      verb_dom.2 = WC_sum.D.2 / WC_sum.P.2,
      mean.rLSM.3 = mean(c(rLSM.D.3, rLSM.P.3)),
      ratio.rLSM.3 = rLSM.D.3 / rLSM.P.3,
      verb_dom.3 = WC_sum.D.3 / WC_sum.P.3) %>%
    # %>%
    #   filter(
    #     WC_sum.D >= 50 & WC_sum.P >= 50
    #   )
    #adding "_turns" at the end of variables since this chunking was based on turns
    rename_at(vars(-(File)), ~ paste0(., '_turns'))
#################################################################################################################
##creating tbyt matching score for VADER scores  
################################################################################################################# 
  
  ECHO_tbyt_matching_VADER <- smoothed_tByT_VADER_complete_df
  
  
  # test <- ECHO_tbyt_matching_VADER  %>%
  #   dplyr::select(File, Speaker) %>%
  #   group_by(File) %>%
  #   mutate(
  #     Speaker.lag = lag(Speaker)
  #   ) %>%
  #   ungroup() %>%
  #   mutate(
  #     speaker_match = if_else(Speaker == Speaker.lag,1,0)
  #   )
  # table(test$speaker_match)
  
  
  ECHO_tbyt_matching_VADER <- ECHO_tbyt_matching_VADER %>%
    dplyr::select(File, Speaker, compound, pos, neu, neg) %>%
    # Adding quick way to drop turn by turns from the same speaker
    ### WE NEED TO THINK ABOUT FLOW. WHERE WE"RE DOING SMOOTHING, ETC> (here or in cleaning)
    group_by(File) %>%
    mutate(
      Speaker.lag = lag(Speaker)
    ) %>%
    ungroup() %>%
    mutate(
      speaker_match = if_else(Speaker == Speaker.lag,1,0)
    ) %>%
    ungroup() %>%
    #filter(speaker_match == 0) %>%
    # End dropping same speaker turns
  
  #rowwise() %>%
  # This puts the turn before the current one on the same row with a .lag suffix
  #dplyr::mutate(across(.cols=everything(), .funs = ~ dplyr::lead(.x,order_by=File,n = 1, default = NA), .names = '{.col}_lead')) %>%
  group_by(File) %>%
    mutate(
      compound.lag = lag(compound),
      pos.lag = lag(pos),
      neu.lag = lag(neu),
      neg.lag = lag(neg)) %>%
    ungroup() %>%
   
    # This makes sure that only liwc categories prersent in the first statement are used for rlsm
    mutate(across(c(compound.lag, pos.lag, neu.lag, neg.lag), 
                  ~ if_else(. > 0,.,as.numeric(NA)))) %>%
    # This sets liwc categories in the responders speech to NA if that category was NA in the first person's speech
    # Per the rLSM paper
    group_by(File) %>%
    mutate(
      compound = if_else(is.na(compound.lag),as.numeric(NA),compound),
      pos = if_else(is.na(pos.lag),as.numeric(NA),pos),
      neu = if_else(is.na(neu.lag),as.numeric(NA),neu),
      neg = if_else(is.na(neg.lag),as.numeric(NA),neg)
    ) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      compound.tbytmatch = 1 - (abs(compound - compound.lag) / (compound + compound.lag + .0001)),
      pos.tbytmatch = 1 - (abs(pos - pos.lag) / (pos + pos.lag + .0001)),
      neu.tbytmatch = 1 - (abs(neu - neu.lag) / (neu + neu.lag + .0001)),
      neg.tbytmatch = 1 - (abs(neg - neg.lag) / (neg + neg.lag + .0001))
    ) %>%
    ungroup() %>%
    group_by(File,Speaker) %>%
    summarize(
      across(contains('.tbytmatch'), .fns = ~ mean(.x,na.rm=TRUE)),
    ) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = File,
      names_from = Speaker,
      values_from = c(compound.tbytmatch, pos.tbytmatch, neu.tbytmatch, neg.tbytmatch),
      # names_prefix = 'rLSM.'
      names_glue = "{.value}.{Speaker}"
    )
  
  