#############
#### Blackbox Scripts for generating accomodation measures
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
    rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE),
    ) %>%
  mutate(
    rLSM = ifelse(is.na(rLSM), 0, rLSM)
    ) %>%
  filter(rLSM > 0) # drops leading rows w/o rLSM calculations


accom_raw <- accom_df %>%
  group_by(File,Speaker) %>%
  mutate(t = row_number()) %>%
  group_modify( ~ as.data.frame(lm(.$rLSM ~ .$t)[['coefficients']][['.$t']]), .keep = TRUE) %>%
  ungroup() %>%
  rename(accomodation_raw = 3) %>%
  pivot_wider(id_cols = File, names_from = Speaker, values_from = accomodation_raw, names_prefix = "accom_raw_")

p <- ggplot(accom_raw, aes(x = accom_raw_patient, y = accom_raw_pcp)) + 
  geom_point() + 
  ggthemes::theme_tufte()
ggExtra::ggMarginal(p,type = "histogram")

accom_10segment <- accom_df %>%
  group_by(File,Speaker) %>%
  mutate(
  id = row_number(),
  t = case_when(id < (max(id)/10) ~ 0,
                id < (max(id/10))*2 ~ 1,
                id < (max(id/10))*3 ~ 2,
                id < (max(id/10))*4 ~ 3,
                id < (max(id/10))*5 ~ 4,
                id < (max(id/10))*6 ~ 5,
                id < (max(id/10))*7 ~ 6,
                id < (max(id/10))*8 ~ 7,
                id < (max(id/10))*9 ~ 8,
                TRUE ~ 9)
  ) %>%
  ungroup() %>%
  select(-id) %>%
  group_by(File,Speaker,t) %>%
  summarize(rLSM = mean(rLSM)) %>%
  ungroup() %>%
  group_by(File,Speaker) %>%
  group_modify( ~ as.data.frame(lm(.$rLSM ~ .$t)[['coefficients']][['.$t']]), .keep = TRUE) %>%
  ungroup() %>%
  rename(accomodation_10_seg = 3) %>%
  pivot_wider(id_cols = File, names_from = Speaker, values_from = accomodation_10_seg, names_prefix = "accom_10_seg_")

p <- ggplot(accom_10segment, aes(x = accom_10_seg_patient, y = accom_10_seg_pcp)) + 
  geom_point() + 
  ggthemes::theme_tufte()
ggExtra::ggMarginal(p,type = "histogram")

accom_20segment <- accom_df %>%
  group_by(File,Speaker) %>%
  mutate(
    id = row_number(),
    t = case_when(id < (max(id)/20) ~ 0,
                  id < (max(id/20))*2 ~ 1,
                  id < (max(id/20))*3 ~ 2,
                  id < (max(id/20))*4 ~ 3,
                  id < (max(id/20))*5 ~ 4,
                  id < (max(id/20))*6 ~ 5,
                  id < (max(id/20))*7 ~ 6,
                  id < (max(id/20))*8 ~ 7,
                  id < (max(id/20))*9 ~ 8,
                  id < (max(id/20))*10 ~ 9,
                  id < (max(id/20))*11 ~ 10,
                  id < (max(id/20))*12 ~ 11,
                  id < (max(id/20))*13 ~ 12,
                  id < (max(id/20))*14 ~ 13,
                  id < (max(id/20))*15 ~ 14,
                  id < (max(id/20))*16 ~ 15,
                  id < (max(id/20))*17 ~ 16,
                  id < (max(id/20))*18 ~ 17,
                  id < (max(id/20))*19 ~ 18,
                  TRUE ~ 20)
  ) %>%
  ungroup() %>%
  select(-id) %>%
  group_by(File,Speaker,t) %>%
  summarize(rLSM = mean(rLSM)) %>%
  ungroup() %>%
  group_by(File,Speaker) %>%
  group_modify( ~ as.data.frame(lm(.$rLSM ~ .$t)[['coefficients']][['.$t']]), .keep = TRUE) %>%
  ungroup() %>%
  rename(accomodation_20_seg = 3) %>%
  pivot_wider(id_cols = File, names_from = Speaker, values_from = accomodation_20_seg, names_prefix = "accom_20_seg_")

p <- ggplot(accom_20segment, aes(x = accom_20_seg_patient, y = accom_20_seg_pcp)) + 
  geom_point() + 
  ggthemes::theme_tufte()
ggExtra::ggMarginal(p,type = "histogram")

accom_cmb <- accom_raw %>%
  full_join(accom_10segment, by = 'File') %>%
  full_join(accom_20segment, by = 'File')
write.csv(accom_cmb,'bb_accomodation.csv')

