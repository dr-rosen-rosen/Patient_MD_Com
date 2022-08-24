#########################################
#### turn by turn LSM
#########################################
library(tidyverse)




####################################################################################################
####################################################################################################
####################################################################################################
# USE THIS SCRIPT SECTION FOR TURN-BY-TURN ANALYSIS OF ROLLING WINDOW ON WHOLE CONVERSATION

#5 TURN rolling window analysis for TbyT
df_tbyt_V3 <- read_csv(here(config$ECHO_rolling_window_5_LIWC_path, config$ECHO_rolling_window_5_LIWC_name))

#8 TURN rolling window analysis 
# df_tbyt_V3 <- read_csv(here(config$ECHO_rolling_window_8_LIWC_path, config$ECHO_rolling_window_8_LIWC_name))

#Removing unnecessary columns
df_tbyt_V3 <- df_tbyt_V3 %>%
  select(-'...1') %>%
  select(-Text) %>%
  select(-Sequence) %>%
  select(-overall_sequence) %>%
  select(-Word_count) %>%
  select(-text_agg_wc) %>%
  select(-Segment) 

# test <- df_tbyt_V3 %>%
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


ECHO_smoothed_rLSM <- df_tbyt_V3 %>%
  dplyr::select(File, text_agg, Speaker, WC, WPS, auxverb, article, adverb, ipron, 
                prep, negate, conj, quantity, ppron) %>%
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
    quantity.orig = quantity,
    ppron.orig = ppron,
    WC.orig = WC,
    WPS.orig = WPS) %>%
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
    quantity.lag = lag(quantity),
    ppron.lag = lag(ppron),
    WC.lag = lag(WC),
    WPS.lag = lag(WPS)) %>%
  ungroup() %>%
  # filter(WC > 1 & WC.lag >1) %>% # drops all exchanges with one word utterances
  # This makes sure that only liwc categories prersent in the first statement are used for rlsm
  mutate(across(c(auxverb.lag, article.lag,adverb.lag, ipron.lag,
                  prep.lag, negate.lag, conj.lag, quantity.lag, ppron.lag
  ), 
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
    quantity = if_else(is.na(quantity.lag),as.numeric(NA),quantity),
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
    quantity.rLSM = 1 - (abs(quantity - quantity.lag) / (quantity + quantity.lag + .0001)),
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
  # drop_na() %>% # find out where these are coming from
  mutate(
    mean.rLSM = rowMeans(select(.,contains('rLSM')),na.rm = TRUE),
    ratio.rLSM.P = rLSM.P / rLSM.D,
    ratio.rLSM.D = rLSM.D / rLSM.P,
    verb_dom = WC_sum.D / WC_sum.P
  ) %>%
  filter(
    WC_sum.D >= 50 & WC_sum.P >= 50
  ) %>%
  select(-c(WPS_avg.D, WPS_avg.P))





####################################################################################################
####################################################################################################
####################################################################################################
# USE THIS SCRIPT SECTION FOR TURN-BY-TURN ANALYSIS OF ROLLING WINDOW ON CHUNKS OF CONVERSATION


ECHO_smoothed_chunks <- read_csv(here(config$ECHO_rolling_window_5_LIWC_path, config$ECHO_rolling_window_5_LIWC_name))

#Removing unnecessary columns
ECHO_smoothed_chunks <-ECHO_smoothed_chunks %>%
  select(-'...1') %>%
  select(-Text) %>%
  select(-Sequence) %>%
  select(-overall_sequence) %>%
  select(-Word_count) %>%
  select(-text_agg_wc) %>%
  select(-Segment) 


#Dropping where WC== 0 since doing the rolling window creates some 
#turns at beginning & end of conversation that included no text (text_agg)
ECHO_smoothed_chunks <-ECHO_smoothed_chunks %>%
  filter(!(WC == 0))



#creating 5 chunks per conversation based on word count
ECHO_smoothed_chunks_wc <- ECHO_smoothed_chunks %>%
  group_by(File) %>%
  mutate(word_count = str_count(text_agg,"\\w+"),
         cumulative = cumsum(word_count),
         chunk = case_when(cumulative < (max(cumulative)/5) ~ 1,
                           cumulative < (max(cumulative/5))*2 ~ 2,
                           cumulative < (max(cumulative/5))*3 ~ 3,
                           cumulative < (max(cumulative/5))*4 ~ 4,
                           TRUE ~ 5)
  ) %>%
  ungroup() %>%
  relocate(chunk, .before = text_agg) %>%
  select(-c(word_count, cumulative))





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


ECHO_smoothed_chunks_wc_rLSM <- ECHO_smoothed_chunks_wc_rLSM %>%
  dplyr::select(File, Speaker, chunk, WC, auxverb, article, adverb, ipron, 
                prep, negate, conj, quantity, ppron) %>%
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
    quantity.lag = lag(quantity),
    ppron.lag = lag(ppron),
    WC.lag = lag(WC)) %>%
  ungroup() %>%
  # filter(WC > 1 & WC.lag >1) %>% # drops all exchanges with one word utterances
  # This makes sure that only liwc categories prersent in the first statement are used for rlsm
  mutate(across(c(auxverb.lag, article.lag, adverb.lag, ipron.lag, 
                  prep.lag, negate.lag, conj.lag, quantity.lag, ppron.lag), 
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
    quantity = if_else(is.na(quantity.lag),as.numeric(NA),quantity),
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
    quantity.rLSM = 1 - (abs(quantity - quantity.lag) / (quantity + quantity.lag + .0001)),
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
    ratio.rLSM.D.1 = rLSM.D.1 / rLSM.P.1,
    ratio.rLSM.P.1 = rLSM.P.1 / rLSM.D.1,
    verb_dom.1 = WC_sum.D.1 / WC_sum.P.1,
    mean.rLSM.2 = mean(c(rLSM.D.2, rLSM.P.2)),
    ratio.rLSM.D.2 = rLSM.D.2 / rLSM.P.2,
    ratio.rLSM.P.2 = rLSM.P.2 / rLSM.D.2,
    verb_dom.2 = WC_sum.D.2 / WC_sum.P.2,
    mean.rLSM.3 = mean(c(rLSM.D.3, rLSM.P.3)),
    ratio.rLSM.D.3 = rLSM.D.3 / rLSM.P.3,
    ratio.rLSM.P.3 = rLSM.P.3 / rLSM.D.3,
    verb_dom.3 = WC_sum.D.3 / WC_sum.P.3,
    mean.rLSM.4 = mean(c(rLSM.D.4, rLSM.P.4)),
    ratio.rLSM.D.4 = rLSM.D.4 / rLSM.P.4,
    ratio.rLSM.P.4 = rLSM.P.4 / rLSM.D.4,
    verb_dom.4 = WC_sum.D.4 / WC_sum.P.4,
    mean.rLSM.5 = mean(c(rLSM.D.5, rLSM.P.5)),
    ratio.rLSM.D.5 = rLSM.D.5 / rLSM.P.5,
    ratio.rLSM.P.5 = rLSM.P.5 / rLSM.D.5,
    verb_dom.5 = WC_sum.D.5 / WC_sum.P.5,
    rLSM_Chunk_Ratio.D = rLSM.D.5/ rLSM.D.1,
    rLSM_Chunk_Ratio.P = rLSM.P.5/ rLSM.P.1) %>%
  # %>%
  #   filter(
  #     WC_sum.D >= 50 & WC_sum.P >= 50
  #   )
  #adding "_wc" at the end of variables since this chunking was based on word count
  rename_at(vars(-(File)), ~ paste0(., '_wc')) 






#################################
##OLD CODE FROM THIS POINT FORWARD
####################################################################################################
####################################################################################################
####################################################################################################

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
  

  ECHO_smoothed_rLSM <- df_tbyt_V2 %>%
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
  ECHO_smoothed_LIWC_matching <- df_tbyt_LIWC %>%
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
##creating tbyt matching score for chunks for linguistic style
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
      verb_dom.3 = WC_sum.D.3 / WC_sum.P.3,
      rLSM_Chunk_Ratio.D = rLSM.D.3/ rLSM.D.1,
      rLSM_Chunk_Ratio.P = rLSM.P.3/ rLSM.P.1) %>%
  # %>%
  #   filter(
  #     WC_sum.D >= 50 & WC_sum.P >= 50
  #   )
    #adding "_wc" at the end of variables since this chunking was based on word count
    rename_at(vars(-(File)), ~ paste0(., '_wc')) 
    


  
  #RUNNING rLSM ON CHUNKS MADE BASED ON TURNS
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
      verb_dom.3 = WC_sum.D.3 / WC_sum.P.3,
      rLSM_Chunk_Ratio.D = rLSM.D.3/ rLSM.D.1,
      rLSM_Chunk_Ratio.P = rLSM.P.3/ rLSM.P.1) %>%
    # %>%
    #   filter(
    #     WC_sum.D >= 50 & WC_sum.P >= 50
    #   )
    #adding "_turns" at the end of variables since this chunking was based on turns
    rename_at(vars(-(File)), ~ paste0(., '_turns'))
  
  
  
  
  #################################################################################################################
  ##creating tbyt matching score for chunks for linguistic content
  ################################################################################################################# 
  ECHO_smoothed_chunks_wc_LIWC_matching <- ECHO_smoothed_chunks_wc
  
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
  ECHO_smoothed_chunks_wc_LIWC_matching <- ECHO_smoothed_chunks_wc_LIWC_matching %>%
    dplyr::select(File, chunk, Text, Speaker, affect, social, cogproc, negemo, percept,
                  bio, drives, relativ, informal) %>%
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
      affect.lag = lag(affect), 
      social.lag = lag(social),
      cogproc.lag = lag(cogproc),
      negemo.lag = lag(negemo),
      percept.lag = lag(percept),
      bio.lag = lag(bio),
      drives.lag = lag(drives),
      relativ.lag = lag(relativ),
      informal.lag = lag(informal)) %>%
    ungroup() %>%
    # filter(WC > 1 & WC.lag >1) %>% # drops all exchanges with one word utterances
    # This makes sure that only liwc categories prersent in the first statement are used for rlsm
    mutate(across(c(affect.lag, social.lag, cogproc.lag, negemo.lag, percept.lag,
                    bio.lag, drives.lag, relativ.lag, informal.lag), 
                  ~ if_else(. > 0,.,as.numeric(NA)))) %>%
    # This sets liwc categories in the responders speech to NA if that category was NA in the first person's speech
    # Per the rLSM paper
    group_by(File, chunk) %>%
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
    group_by(File, Speaker, chunk) %>%
    summarize(
      across(contains('.tbytmatch'), .fns = ~ mean(.x,na.rm=TRUE))) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = File,
      names_from = c(chunk,Speaker),
      values_from = c(affect.tbytmatch, social.tbytmatch, cogproc.tbytmatch, percept.tbytmatch,
                      negemo.tbytmatch, bio.tbytmatch, drives.tbytmatch, relativ.tbytmatch, informal.tbytmatch),
      names_glue = "{.value}.{Speaker}.{chunk}") %>%
    rename_at(vars(-(File)), ~ paste0(., '_wc')) %>%
    mutate(affect_chunkratio_D_wc = affect.tbytmatch.D.3_wc/affect.tbytmatch.D.1_wc,
           affect_chunkratio_P_wc = affect.tbytmatch.P.3_wc/affect.tbytmatch.P.1_wc,
           social_chunkratio_D_wc = social.tbytmatch.D.3_wc/social.tbytmatch.D.1_wc,
           social_chunkratio_P_wc =  social.tbytmatch.P.3_wc/social.tbytmatch.P.1_wc,
           cogproc_chunkratio_D_wc =  cogproc.tbytmatch.D.3_wc/cogproc.tbytmatch.D.1_wc,
           cogproc_chunkratio_P_wc = cogproc.tbytmatch.P.3_wc/cogproc.tbytmatch.P.1_wc,
           percept_chunkratio_D_wc = percept.tbytmatch.D.3_wc/percept.tbytmatch.D.1_wc,
           percept_chunkratio_P_wc = percept.tbytmatch.P.3_wc/percept.tbytmatch.P.1_wc,
           negemo_chunkratio_D_wc = negemo.tbytmatch.D.3_wc/negemo.tbytmatch.D.1_wc,
           negemo_chunkratio_P_wc =  negemo.tbytmatch.P.3_wc/negemo.tbytmatch.P.1_wc,
           bio_chunkratio_D_wc = bio.tbytmatch.D.3_wc/bio.tbytmatch.D.1_wc,
           bio_chunkratio_P_wc = bio.tbytmatch.P.3_wc/bio.tbytmatch.P.1_wc,
           drives_chunkratio_D_wc = drives.tbytmatch.D.3_wc/drives.tbytmatch.D.1_wc,
           drives_chunkratio_P_wc = drives.tbytmatch.P.3_wc/drives.tbytmatch.P.1_wc,
           relativ_chunkratio_D_wc = relativ.tbytmatch.D.3_wc/relativ.tbytmatch.D.1_wc,
           relativ_chunkratio_P_wc = relativ.tbytmatch.P.3_wc/relativ.tbytmatch.P.1_wc,
           informal_chunkratio_D_wc = informal.tbytmatch.D.3_wc/informal.tbytmatch.D.1_wc,
           informal_chunkratio_P_wc = informal.tbytmatch.P.3_wc/informal.tbytmatch.P.1_wc)
    
    
  

  
  
  
  #RUNNING rLSM ON CHUNKS MADE BASED ON TURNS
  ECHO_smoothed_chunks_turns_LIWC_matching <- ECHO_smoothed_chunks_turns
  
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
  ECHO_smoothed_chunks_turns_LIWC_matching <- ECHO_smoothed_chunks_turns_LIWC_matching %>%
    dplyr::select(File, chunk, Text, Speaker, affect, social, cogproc, negemo, percept,
                  bio, drives, relativ, informal) %>%
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
      affect.lag = lag(affect), 
      social.lag = lag(social),
      cogproc.lag = lag(cogproc),
      negemo.lag = lag(negemo),
      percept.lag = lag(percept),
      bio.lag = lag(bio),
      drives.lag = lag(drives),
      relativ.lag = lag(relativ),
      informal.lag = lag(informal)) %>%
    ungroup() %>%
    # filter(WC > 1 & WC.lag >1) %>% # drops all exchanges with one word utterances
    # This makes sure that only liwc categories prersent in the first statement are used for rlsm
    mutate(across(c(affect.lag, social.lag, cogproc.lag, negemo.lag, percept.lag,
                    bio.lag, drives.lag, relativ.lag, informal.lag), 
                  ~ if_else(. > 0,.,as.numeric(NA)))) %>%
    # This sets liwc categories in the responders speech to NA if that category was NA in the first person's speech
    # Per the rLSM paper
    group_by(File, chunk) %>%
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
    group_by(File, Speaker, chunk) %>%
    summarize(
      across(contains('.tbytmatch'), .fns = ~ mean(.x,na.rm=TRUE))) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = File,
      names_from = c(chunk,Speaker),
      values_from = c(affect.tbytmatch, social.tbytmatch, cogproc.tbytmatch, percept.tbytmatch,
                      negemo.tbytmatch, bio.tbytmatch, drives.tbytmatch, relativ.tbytmatch, informal.tbytmatch),
      names_glue = "{.value}.{Speaker}.{chunk}") %>%
    rename_at(vars(-(File)), ~ paste0(., '_turns'))  %>%
    mutate(affect_chunkratio_D_turns = affect.tbytmatch.D.3_turns/affect.tbytmatch.D.1_turns,
           affect_chunkratio_P_turns = affect.tbytmatch.P.3_turns/affect.tbytmatch.P.1_turns,
           social_chunkratio_D_turns = social.tbytmatch.D.3_turns/social.tbytmatch.D.1_turns,
           social_chunkratio_P_turns =  social.tbytmatch.P.3_turns/social.tbytmatch.P.1_turns,
           cogproc_chunkratio_D_turns =  cogproc.tbytmatch.D.3_turns/cogproc.tbytmatch.D.1_turns,
           cogproc_chunkratio_P_turns = cogproc.tbytmatch.P.3_turns/cogproc.tbytmatch.P.1_turns,
           percept_chunkratio_D_turns = percept.tbytmatch.D.3_turns/percept.tbytmatch.D.1_turns,
           percept_chunkratio_P_turns = percept.tbytmatch.P.3_turns/percept.tbytmatch.P.1_turns,
           negemo_chunkratio_D_turns = negemo.tbytmatch.D.3_turns/negemo.tbytmatch.D.1_turns,
           negemo_chunkratio_P_turns =  negemo.tbytmatch.P.3_turns/negemo.tbytmatch.P.1_turns,
           bio_chunkratio_D_turns = bio.tbytmatch.D.3_turns/bio.tbytmatch.D.1_turns,
           bio_chunkratio_P_turns = bio.tbytmatch.P.3_turns/bio.tbytmatch.P.1_turns,
           drives_chunkratio_D_turns = drives.tbytmatch.D.3_turns/drives.tbytmatch.D.1_turns,
           drives_chunkratio_P_turns = drives.tbytmatch.P.3_turns/drives.tbytmatch.P.1_turns,
           relativ_chunkratio_D_turns = relativ.tbytmatch.D.3_turns/relativ.tbytmatch.D.1_turns,
           relativ_chunkratio_P_turns = relativ.tbytmatch.P.3_turns/relativ.tbytmatch.P.1_turns,
           informal_chunkratio_D_turns = informal.tbytmatch.D.3_turns/informal.tbytmatch.D.1_turns,
           informal_chunkratio_P_turns = informal.tbytmatch.P.3_turns/informal.tbytmatch.P.1_turns)
#################################################################################################################
##creating tbyt matching score for VADER scores  
################################################################################################################# 
  
  ECHO_smoothed_VADER_matching <- read_csv(here(config$smoothed_tByT_VADER_complete_df_path, config$smoothed_tByT_VADER_complete_df_name))
  
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
  
  
  ECHO_smoothed_VADER_matching <- ECHO_smoothed_VADER_matching %>%
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
  
  