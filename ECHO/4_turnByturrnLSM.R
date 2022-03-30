#########################################
#### tur by turn LSM
#########################################
library(tidyverse)
# read in turn by turn LIWC file
df_tbyt <- read.csv('/Users/mrosen44/Johns Hopkins/Salar Khaleghzadegan - Patient _Provider_Communication_Projects/ECHO1_Study/ECHO_Transcripts_Complete_TbyT_LIWC_031222.csv')


# test <- df_tbyt %>%
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

rLSM_df <- df_tbyt %>%
  dplyr::select(File, Speaker, WC, WPS, Sequence, auxverb, article, adverb, ipron, 
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
  filter(speaker_match == 0) %>%
  # End dropping same speaker turns
  
  group_by(File) %>%
  #rowwise() %>%
  # This puts the turn before the current one on the same row with a .lag suffix
  #dplyr::mutate(across(.cols=everything(), .funs = ~ dplyr::lead(.x,order_by=File,n = 1, default = NA), .names = '{.col}_lead')) %>%
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
  filter(WC > 1 & WC.lag >1) %>% # drops all exchanges with one word utterances
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
  # mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
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
    WC_sum.D >= 100 & WC_sum.P >= 100
  )

readr::write_csv(rLSM_df , 'rLSM.csv')



