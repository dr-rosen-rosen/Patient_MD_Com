#############
#### Blackbox Scripts for trying gimmee
library(tidyverse)

# start with the rw.LSM values
accom_df <- read.csv("/Users/mrosen44/Johns\ Hopkins/Salar\ Khaleghzadegan\ -\ Patient\ _Provider_Communication_Projects/blackbox_study/blackbox_rolling_window_8_liwc.csv")

accom_df <- accom_df %>%
  dplyr::select(File, text_agg, Speaker, WC, WPS, auxverb, article, adverb, ipron, 
                prep, negate, conj, quantity, ppron) %>%
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
  # This puts the turn before the current one on the same row with a .lag suffix
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
    auxverb.rw.rLSM = 1 - (abs(auxverb - auxverb.lag) / (auxverb + auxverb.lag + .0001)),
    article.rw.rLSM = 1 - (abs(article - article.lag) / (article + article.lag + .0001)),
    adverb.rw.rLSM = 1 - (abs(adverb - adverb.lag) / (adverb + adverb.lag + .0001)),
    ipron.rw.rLSM = 1 - (abs(ipron - ipron.lag) / (ipron + ipron.lag + .0001)),
    prep.rw.rLSM = 1 - (abs(prep - prep.lag) / (prep + prep.lag + .0001)),
    negate.rw.rLSM = 1 - (abs(negate - negate.lag) / (negate + negate.lag + .0001)),
    conj.rw.rLSM = 1 - (abs(conj - conj.lag) / (conj + conj.lag + .0001)),
    quantity.rw.rLSM = 1 - (abs(quantity - quantity.lag) / (quantity + quantity.lag + .0001)),
    ppron.rw.rLSM = 1 - (abs(ppron - ppron.lag) / (ppron + ppron.lag + .0001))
  ) %>%
  ungroup() %>%
  #creates an average rLSM across separrate featurers
  mutate(
    rLSM = rowMeans(dplyr::select(.,contains('.rLSM')),na.rm = TRUE),
  ) %>%
  mutate(
    rLSM = ifelse(is.na(rLSM), 0, rLSM)
  ) %>%
  # filter(rLSM > 0) # drops leading rows w/o rLSM calculations
  group_by(File) %>%
  slice(15:n()) %>% # drops leading rows with no data (this is for a window of 8... frist 7 rows for both speakers are empty)
  ungroup()

file_speaker_n <- accom_df %>% group_by(File) %>%
  summarize(n = n()) %>% ungroup()

# select for appropriate gimme length... 100 after separating speakers
accom_df <- accom_df %>%
  group_by(File) %>% filter(n() >= 200) %>% ungroup()


df_list <- split(accom_df, f = accom_df$File)

for (i in names(df_list)) {
  print(i)
  df <- df_list[[i]] %>%
    select(Speaker, WC, rLSM)
  if (df[[1,'Speaker']] == 'pcp') {
    print(paste('TRUE:',df[1,'Speaker']))
    print(nrow(df))
    df <- df[-1,]
    print(nrow(df))
  }
  df <- df %>%
    mutate(pair = rep(1:nrow(.), each = 2, length.out = nrow(.))) %>%
    pivot_wider(id_cols = pair, names_from = Speaker, values_from = c(WC,rLSM)) %>%
    select(-pair) 
  # print(names(df))
  print(sapply(df,typeof))
  
  df_list[[i]] <- df
}
# sapply(df_list[[1]],typeof)


# x <- accom_df %>% filter(File == 108128) %>%
#   select(Speaker, WC, rLSM) %>%
#   mutate(pair = rep(1:nrow(.), each = 2, length.out = nrow(.))) %>%
#   pivot_wider(id_cols = pair, names_from = Speaker, values_from = c(WC,rLSM))


# find all dataframews not completely smoothed
not_smooth_list <- list()
for (f in names(df_list)) {
  if ('list' %in% (sapply(df_list[[f]], typeof))) {
    print(f)
    not_smooth_list <- append(not_smooth_list, f)
      }
}

data.frame(not_smoothed = unlist(not_smooth_list)) %>% write.csv('potentially_not_smoothed_BB.csv')
# keep only perfectly smoothed files
length(df_list)
# df_list <- within(df_list, rm(list=get(not_smooth_list)))
df_list <- df_list[setdiff(names(df_list),not_smooth_list)]
length(df_list)

max_df_len <- sapply(df_list,nrow) %>% max() # find longest dataframe

# pad all other dataframes with na's at the end
for (f in names(df_list)) {
  df <- df_list[[f]]
  if (nrow(df) < max_df_len) {
    rows_to_add <- max_df_len - nrow(df)
    df[nrow(df) + 1:rows_to_add,] <- NA
    
  }
  df <- df %>%
    mutate(
      WC_patient = as.numeric(scale(WC_patient, center = TRUE, scale = TRUE)),
      WC_pcp = as.numeric(scale(WC_pcp, center = TRUE, scale = TRUE)),
      rLSM_patient = as.numeric(scale(rLSM_patient, center = TRUE, scale = TRUE)),
      rLSM_pcp = as.numeric(scale(rLSM_pcp, center = TRUE, scale = TRUE))
    )
  df_list[[f]] <- df
}
# df_list[['420069']]
gimme::gimme(df_list, out = here::here('Black_Box/gimme_out'))

