#############
#### Scripts for generating accommodation measures across studies
library(tidyverse)
library(here)
# start with the rw.LSM values
# accom_df <- read.csv(here('/Users/mrosen44/Johns\ Hopkins/Salar\ Khaleghzadegan\ -\ Patient\ _Provider_Communication_Projects/cross_study_analyses/combined_smoothed_rw.rLSM.csv'))
# table(accom_df$Speaker)

# accom_df$Speaker[accom_df$Speaker == 'doctor'] <- 'pcp'


#############
#### NEW Scripts using functions in peds project
#############

source('/Users/mrosen44/Documents/Data_Analysis_Local/pediatricSurgeryCommunication/3_accomodationMetrics.R')


# Conv LSM
conv_liwcd <- read.csv('LIWC-22 Results - FINAL_cmbd_conv - LIWC Analysis.csv')
convLSM_df <- getConvLSM(
  df = conv_liwcd,
  file_col = 'file_study_id',
  spkr_col = 'speaker',
  vars_to_match = c('auxverb','article','adverb','ipron','prep','negate','conj','quantity','ppron')
)
 # baseline LSM
baseline_liwcd <- read.csv('LIWC-22 Results - baselineLSM - LIWC Analysis.csv')

baselineLSM_df <- getConvLSM(
  df = baseline_liwcd,
  file_col = 'file_study_id',
  spkr_col = 'speaker',
  vars_to_match = c('auxverb','article','adverb','ipron','prep','negate','conj','quantity','ppron')
) %>%
  rename(baseline_lsm = conv_lsm)

# rLSM
windowed_liwcd <- read.csv('LIWC-22 Results - FINAL_cmbd_windowed - LIWC Analysis.csv') %>%
  mutate(
    speaker = case_match(speaker,
                         'clinician' ~ 'pcp',
                         .default = speaker)
  )

rwLSM_df <- getRwLSM(
  df = windowed_liwcd, 
  file_col = 'file_study_id',
  spkr_col = 'speaker') 

# rw.rLSM <- getRwLSM(
#   df = accom_df,
#   file_col = 'file_study_id',
#   spkr_col = 'speaker'
# )
hist(rwLSM_df$rLSM.pcp)

window_size = 8
windowed_liwcd <- windowed_liwcd %>%
  group_by(file_study_id) %>%
  slice(((window_size-1)*2):n()) %>%
  ungroup()

rw.rLSM.by.timePoint <- getAccomodation(
  df = windowed_liwcd, 
  file_col = 'file_study_id',
  spkr_col = 'speaker',
  breaks = 4)

all_lsm_la <- file_metrics %>%
  full_join(rw.rLSM.by.timePoint, by = 'file_study_id') %>% 
  full_join(rwLSM_df, by = 'file_study_id') %>%
  full_join(convLSM_df, by = 'file_study_id') %>%
  full_join(baselineLSM_df, by = 'file_study_id') %>%
  separate_wider_delim(file_study_id, '-', names = c('file','study')) %>%
  mutate(
    file_ID = paste0(study,'-',file),
    file_study_id = paste0(file,'-',study)
  )

all_lsm_la %>% write.csv('cross_study_matching_accomodation_11-07-2023.csv')

skimr::skim(all_lsm_la)

x <- all_lsm_la %>%
  separate_wider_delim(file_study_id, '-',names = c(NA,'study')) %>%
  mutate(
    over_20perc_other = if_else(prop_other_turn > .2, 1,0),
    under_100_turns = if_else(tot_turns < 100, 1, 0),
    excluded_for_either = if_else(over_20perc_other == 1 | under_100_turns ==1, 1, 0)
  ) %>%
  group_by(study) %>%
  summarize(
    tot_n = n(),
    n_over_20perc_other = sum(over_20perc_other),
    n_under_100_turns = sum(under_100_turns),
    n_excluded_for_either = sum(excluded_for_either),
    final_included = tot_n-n_excluded_for_either
  )
  
  
#############
#### OLD Scripts for generating accommodation measures across studies


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

file_speaker_n <- accom_df %>% group_by(File,Speaker) %>%
  summarize(n = n()) %>% ungroup()

accom_raw <- accom_df %>%
  # filter(File == 100019) %>%
  group_by(File,Speaker) %>%
  mutate(t = row_number()) %>%
  group_modify(function(data,key) {
    tryCatch(
      {m <- nlme::gls(
        rLSM ~ t, 
        data = data, 
        correlation = nlme::corAR1()
      ) %>%
        broom.mixed::tidy()}, error = function(e) {
          tibble(term = c(NA),
                 estimate = c(NA_real_),
                 std.error = c(NA_real_),
                 statistic = c(NA_real_),
                 p.value = c(NA_real_)
          )
        })
  }
  ) %>%
  filter(term == 't') %>%
  dplyr::select(File, Speaker, estimate) %>%
  rename(accomodation_raw = estimate) %>%
  left_join(file_speaker_n, by = c('File','Speaker')) %>%
  rowwise() %>%
  # mutate(accomodation_raw = accomodation_raw*(n-1)) %>%
  ungroup %>%
  dplyr::select(-n) |>
  pivot_wider(id_cols = File, names_from = Speaker, values_from = accomodation_raw, names_prefix = "accom_raw_")




p <- ggplot(accom_raw, aes(x = accom_raw_patient, y = accom_raw_pcp)) + 
  geom_point() +
  geom_smooth(method=lm) +
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
  dplyr::select(-id) %>%
  group_by(File,Speaker,t) %>%
  summarize(rLSM = mean(rLSM)) %>%
  ungroup() %>%
  group_by(File,Speaker) %>%
  group_modify(function(data,key) {
    tryCatch(
      {m <- nlme::gls(
        rLSM ~ t, 
        data = data, 
        correlation = nlme::corAR1()
      ) %>%
        broom.mixed::tidy()}, error = function(e) {
          tibble(term = c(NA),
                 estimate = c(NA_real_),
                 std.error = c(NA_real_),
                 statistic = c(NA_real_),
                 p.value = c(NA_real_)
          )
        })
  }
  ) %>%
  filter(term == 't') %>%
  dplyr::select(File, Speaker, estimate) %>%
  rename(accomodation_10_seg = estimate) %>%
  rowwise() %>%
  # mutate(accomodation_10_seg = accomodation_10_seg*9) %>%
  ungroup %>%
  pivot_wider(id_cols = File, names_from = Speaker, values_from = accomodation_10_seg, names_prefix = "accom_10_seg_")


p <- ggplot(accom_10segment, aes(x = accom_10_seg_patient, y = accom_10_seg_pcp)) + 
  geom_point() +
  geom_smooth(method=lm) +
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
                  TRUE ~ 19)
  ) %>%
  ungroup() %>%
  dplyr::select(-id) %>%
  group_by(File,Speaker,t) %>%
  summarize(rLSM = mean(rLSM)) %>%
  ungroup() %>%
  group_by(File,Speaker) %>%
  group_modify(function(data,key) {
    tryCatch(
      {m <- nlme::gls(
        rLSM ~ t, 
        data = data, 
        correlation = nlme::corAR1()
      ) %>%
        broom.mixed::tidy()}, error = function(e) {
          tibble(term = c(NA),
                 estimate = c(NA_real_),
                 std.error = c(NA_real_),
                 statistic = c(NA_real_),
                 p.value = c(NA_real_)
          )
        })
  }
  ) %>%
  filter(term == 't') %>%
  dplyr::select(File, Speaker, estimate) %>%
  rename(accomodation_20_seg = estimate) %>%
  rowwise() %>%
  # mutate(accomodation_20_seg = accomodation_20_seg*19) %>%
  ungroup %>%
  pivot_wider(id_cols = File, names_from = Speaker, values_from = accomodation_20_seg, names_prefix = "accom_20_seg_")

p <- ggplot(accom_20segment, aes(x = accom_20_seg_patient, y = accom_20_seg_pcp)) + 
  geom_point() + 
  geom_smooth(method=lm) +
  ggthemes::theme_tufte() 
ggExtra::ggMarginal(p,type = "histogram")

accom_cmb <- accom_raw %>%
  full_join(accom_10segment, by = 'File') %>%
  full_join(accom_20segment, by = 'File')
write.csv(accom_cmb,'bb_accomodation_acf_r_07-27-2023.csv')



pcp_turn_count <- accom_df %>% filter(Speaker == 'pcp') %>% 
  group_by(File) %>%
  summarize(n = n())



# simple ar1 model: 104140
# complex mdoel: 315016
fnum <- 311109
test <- accom_df %>% filter(File == fnum, Speaker == 'pcp') %>%  
  select(rLSM)

test2 <- test %>%
  mutate(t = row_number()) #%>%
modelsummary::modelsummary(lm(test2$rLSM ~ test2$t), estimate = "{estimate} ({std.error})", statistic = NULL)

x <- forecast::auto.arima(ts(data = test$rLSM, start = 1, end = nrow(test)), stepwise = FALSE, approximation = FALSE, allowmean = FALSE, allowdrift = FALSE) #%>%
#forecast::autoplot(.)
# matplot(cbind(test$rLSM, fitted(x), type = 'l'))
# modelsummary::modelsummary(x, estimate = "{estimate} ({std.error})", statistic = NULL)
# modelsummary::modelplot(x)

test %>% 
  mutate(
    fitted = fitted(x),
    t = row_number()
  ) %>%
  pivot_longer(
    cols = -t,
    names_to = 'variable',
    values_to = 'value'
  ) %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(color = variable, linetype = variable)) + 
  labs(title = paste('Actual and fitted values for:',fnum))

test %>% mutate(t = row_number()) %>% ggplot(aes(x=t, y = rLSM)) + geom_point() + geom_line()
test %>% mutate(t = row_number()) %>% ggplot(aes(x=t, y = fitted(x))) + geom_point() + geom_line()

for (file in unique(accom_20segment$File)) {
  test <- accom_20segment %>% filter(File == file, Speaker == 'pcp') %>% 
    # group_by(File,Speaker) %>%
    # mutate(t = row_number()) %>%
    select(rLSM) 
  t_model <- forecast::auto.arima(ts(data = test, start = 1, end = nrow(test)), stepwise = FALSE, approximation = FALSE)
  print(paste("Results for...",file))
  # print(summary(t_model))
  print(t_model$coef)
}

apply_auto_arima <- function(col) {
  # if calling from lapply, end needs to be length(col); if calling from loop: nrow(col)
  col <- ts(data = col, start = 1, end = nrow(col))
  x <- forecast::auto.arima(col, stepwise = FALSE, approximation = FALSE, allowdrift = FALSE, allowmean = FALSE)
  return(x)
}

results <- list()
for (col in colnames(test)) {
  data <- test[col] %>% drop_na()
  print(nrow(data))
  x <- apply_auto_arima(data)
  print(x)
  # results <- bind_rows(results,as.data.frame(x))
  results[[col]] <- x
}

# fnum <- '421112'
for (fnum in names(results)) {
  print(fnum)
  rw.rLSM.D <- results[[fnum]]$x
  fitted <- results[[fnum]]$fitted
  # names(rw.rLSM.D) <- 'rw.rLSM.D'
  
  
  tx <- paste(utils::capture.output((results[[fnum]])), collapse = '\n')
  arima_sum <- ggplot() + theme_void() + 
    geom_text(aes(0,0,label = tx), size = 2) + xlab(NULL) + labs(title = paste("ARIMA model\nfor:",fnum))
  
  
  arima_line <- bind_cols(rw.rLSM.D,fitted) %>%
    rename('rw.rLSM.D' = x, 'fitted' = !!fnum) %>%
    mutate(
      t = row_number()
    ) %>%
    pivot_longer(
      cols = -t,
      names_to = 'variable',
      values_to = 'value'
    ) %>%
    ggplot(aes(x = t, y = value)) +
    geom_line(aes(color = variable, linetype = variable)) + 
    labs(title = paste('Actual and fitted values for:',fnum)) +
    theme(legend.position="bottom")
  
  
  data <- bind_cols(rw.rLSM.D,fitted) %>%
    rename('rw.rLSM.D' = x, 'fitted' = !!fnum) %>%
    mutate(
      t = row_number()
    )
  fit <- lm(rw.rLSM.D ~ t, data = data)
  lm_plot <- ggplotRegression(fit)
  fit2 <- accom_20segment %>% filter(File == fnum, Speaker == 'pcp') %>%
    rename(rw.rLSM.D_20seg = rLSM) %>%
    mutate(t = t+1) %>%
    lm(rw.rLSM.D_20seg ~ t, data = .)
  lm_plot2 <- ggplotRegression(fit2)
  
  # library(patchwork)
  p <- (arima_line | arima_sum) + plot_layout(widths = c(3,1))
  p <- p / (lm_plot | lm_plot2)  
  ggsave(filename = paste0(fnum,'_raw.png'),plot = p, width=11, height=8.5, units='in')
}



ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       "\n Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5))) +
    theme(plot.title = element_text(size = 8))
}

x <- as.data.frame(do.call(rbind,results))

test <- accom_df %>% filter(Speaker == 'pcp') %>% select(File,rLSM) %>% 
  group_by(File) %>%
  mutate(t = row_number()) %>%
  ungroup() %>%
  pivot_wider(names_from = File, values_from = rLSM, values_fill = NA) %>%
  select(-t)
# auto.arima.results <- as.data.frame(do.call(rbind, lapply(test, forecast::auto.arima, stepwise = FALSE, approximation = FALSE, allowdrift = FALSE, allowmean = FALSE)))
auto.arima.results <- as.data.frame(do.call(rbind, lapply(test, apply_auto_arima)))


auto.arima.coef <- data.frame()
for (r in seq(1:nrow(auto.arima.results['coef']))) {
  one_set <- as.data.frame(auto.arima.results[r,'coef']) %>% 
    rownames_to_column() %>% 
    pivot_longer(!rowname, names_to = "File", values_to = "col2") %>% 
    pivot_wider(names_from = "rowname", values_from = "col2")
  auto.arima.coef <- bind_rows(auto.arima.coef, one_set)
}

##### dynamic correlation using dynCorr... this DOES NOT APPEAR TO PROVIDE INDIVIDUAL ESTIMATES!?!?!

library(dynCorr)
x <- dynCorr::dynCorrData

dynCor_df <- accom_df %>% dplyr::select(File, Speaker, rLSM) %>% 
  group_by(File, Speaker) %>%
  mutate(t = row_number()) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(File,t), names_from = Speaker, values_from = rLSM) %>% 
  group_by(File) %>%
  filter(n() > 100) %>% # 100 matches the default length for dyncor
  ungroup()

x <- dynCorr::dynamicCorrelation(
  # dynCor_df %>% filter(File == 101037), 
  dynCor_df,
  depVar = c('patient','pcp'), 
  indepVar = 't', 
  subjectVar = 'File',
  # lag.input = seq(from = 1, to = 5, by = 1),
  full.lag.output = TRUE,
  function.choice = c(1,0,0))

y <- dynCorr::bootstrapCI(
  # dynCor_df %>% filter(File == 101037), 
  dynCor_df,
  depVar = c('patient','pcp'), 
  indepVar = 't', 
  subjectVar = 'File',
  # lag.input = seq(from = 1, to = 5, by = 1),
  # full.lag.output = TRUE,
  function.choice = c(1,0,0))

examp1_by <- dynamicCorrelation(dataFrame = dynCorrData,
                                depVar = c('resp1', 'resp2', 'resp3'),
                                indepVar = 'time',
                                points.by = 1,
                                subjectVar = 'subject',
                                function.choice = c(1,1,0))

############### dynmanic corrleation using liu et al scripts; Also appears jacked up (sequences of different length is a big problem I think)

source('dynamicalcorrelation_fixed_022218.r')
x <- dynCor_df %>% dplyr::select(File,pcp,t) %>%
  mutate(File = paste0('f_',as.character(File))) %>%
  pivot_wider(id_cols = t, names_from = File, values_from = pcp) %>% as.matrix()

y <- dynCor_df %>% dplyr::select(File,patient,t) %>%
  mutate(File = paste0('f_',as.character(File))) %>%
  pivot_wider(id_cols = t, names_from = File, values_from = patient) %>% as.matrix()

t <- seq(from = 1, to = max(dynCor_df$t), by = 1)

z <- ind_DC(
  x = x,
  y = y,
  t = t
)

###############  rqa

library(crqa)
ts1 <- dynCor_df %>% filter(File == 315098) %>% dplyr::select(pcp) #%>% ts()

test <- crqa(ts1 = ts1$pcp, ts2 = ts1$pcp, delay = 5, embed = 3, rescale = 0, radius = 0.03, normalize = 0,
             side = 'both', method = 'rqa', datatype = 'continuous')

parC = list(unit = 10, labelx = "X", labely = "Y", 
            cols = "black", pcex = .3, pch = 20, las = 0, 
            labax = seq(0, nrow(ts1), 1), labay = seq(0, nrow(ts1), 1)) 

plotRP(test$RP, parC)
