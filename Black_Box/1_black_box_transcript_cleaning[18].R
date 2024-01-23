#opening the necessary packages
library(tidyverse)
library(stringr)
library(here)
library(config)
library(haven)
library(skimr)
library(runner)
library(corrplot)


Sys.setenv(R_CONFIG_ACTIVE = "salar") # 'default')#
config <- config::get()

####################################################################################################################
#EXTRACTING ALL TRANSCRIPTS FROM WORD DOCUMENTS INTO A SINGLE DATAFRAME
####################################################################################################################


#THIS IS THE TRANSCRIPTS FOR EXTRACTING THE TRANSCRIPTS FROM WORD DOCUMENTS INTO A SINGLE DATAFRAME
#DO NOT RERUN UNLESS NECESSARY
# setwd("/Users/skhaleg1/Documents/R_projects_2020/Blackbox_transcripts_temp")
# 
# all_filenames_blackbox <- list.files(
#   path = "/Users/skhaleg1/Documents/R_projects_2020/Blackbox_transcripts_temp",
#   pattern = ".docx", recursive = TRUE)
# 
# #initialize df with first file
# audio_doc <- read_docx(all_filenames_blackbox[1])
# audio_doc_final <- docx_extract_tbl(audio_doc, 1, header = TRUE)
# ID = strsplit(all_filenames_blackbox[1],'_')[[1]][1]
# audio_doc_final$ID = rep(ID, length(audio_doc_final$Speaker))
# 
# for(file in all_filenames_blackbox[2:length(all_filenames_blackbox)])
# {
#   audio_doc <- read_docx(file)
#   audio_doc_ind <- docx_extract_tbl(audio_doc, 1, header = TRUE)
#   ID = strsplit(file,'_')[[1]][1]
#   audio_doc_ind$ID = rep(ID, length(audio_doc_ind$Speaker))
#   colnames(audio_doc_ind)= colnames(audio_doc_final)
#   audio_doc_final = rbind(audio_doc_final,audio_doc_ind)
# }
# 
# 
# write.csv(audio_doc_final, "Black_Box_Raw_Transcripts.csv")




#opening blackbox survey data
black_box_survey_data_update <- read_dta(here::here(f_dir,"Black Box, main dataset, March 8 2023 COPY.dta"))
# black_box_survey_data_update<- read_dta(here(config$black_box_survey_data_update_path, config$black_box_survey_data_update_name))

####################################################################################################################
#EXTRACTING ALL TRANSCRIPTS FROM WORD DOCUMENTS INTO A SINGLE DATAFRAME
####################################################################################################################



#reading in my black_box transcripts
black_box_transcripts <- read.csv("black_box_transcripts.csv")
#black_box_survey_data_update<- read_csv(here(config$black_box_survey_data_update_path, config$black_box_survey_data_update_name))


black_box_transcripts <- black_box_transcripts %>%
  rename("File" = "ID",
         "Text" = "Dialogue",
         "timestamp" = "Timestamp.End.of.Speaker")


#removing all brackets with content inside of them
black_box_transcripts$Text <- gsub("\\[(.*?)\\]", "", black_box_transcripts$Text)

#removing all brackets with content inside of them
black_box_transcripts$Text <- gsub("\\((.*?)\\)", "", black_box_transcripts$Text)

#removing all brackets with content inside of them
black_box_transcripts$Text <- gsub("\\{(.*?)\\)", "", black_box_transcripts$Text)

black_box_transcripts$Text <- gsub("_", "", black_box_transcripts$Text)

black_box_transcripts$File <- gsub("-", "", black_box_transcripts$File)
  
#counting the number of words for each conversation turn
black_box_transcripts$Word_count <- str_count(black_box_transcripts$Text, "\\w+")


#Removing all rows where there is no speech
black_box_transcripts <- black_box_transcripts %>%
  filter(Word_count != 0)

#filtering out some 
black_box_transcripts <- black_box_transcripts %>%
  filter(Speaker != "No.") %>%
  filter(Text != "Cross talking") %>%
  filter(Text != "No audio from 00:04:39 through 00:25:39") %>%
  filter(Text != "Telephone conversation 00:06:21 through 00:08:39") %>%
  filter(Text != "Laughter")
  

#recoding speakers with errors in the names
black_box_transcripts$Speaker <- recode(black_box_transcripts$Speaker, 
                                     "PCP speaker" = "PCP",
                                     "] PCP" = "PCP",
                                     "P[00:15:45]CP" = "PCP",
                                     "PAP" = "PCP",
                                     "PC" = "PCP",
                                     "PCp" = "PCP",
                                     "PCP speaker" = "PCP",
                                     "PCP:" = "PCP",
                                     "PCP." = "PCP",
                                     "All right. Patient" = "PCP",
                                     "Interviewee2" = "Other Speaker",
                                     "Oher speaker" = "Other Speaker",
                                     "Other" = "Other Speaker",
                                     "Other  Speaker" = "Other Speaker",
                                     "other speaker" = "Other Speaker",
                                     "Other speaker" = "Other Speaker",
                                     "Other Speaker" = "Other Speaker",
                                     "Other speaker:" = "Other Speaker",
                                     "Other Speaker:" = "Other Speaker",
                                     "Other speaker." = "Other Speaker",
                                     "Other Speaker`" = "Other Speaker",
                                     "Other." = "Other Speaker",
                                     "Other" = "Other Speaker",
                                     "Others speaker" = "Other Speaker",
                                     "Pother speaker" = "Other Speaker",
                                     "Third Person" = "Other Speaker",
                                     "Third Person:" = "Other Speaker",
                                     "Other person" = "Other Speaker",
                                     "Nurse" = "Other Speaker",
                                     "Patent:" = "Patient",
                                     "patient" = "Patient",
                                     "Patient" = "Patient",
                                     "PATIENT" = "Patient",
                                     "Patient:" = "Patient",
                                     "PATIENT:" = "Patient",
                                     "Patient." = "Patient",
                                     "Psoriasis" = "Patient",
                                     "P" = "Patient",
                                     "UF"= "Other Speaker",
                                     "Sarah:"="Other Speaker",
                                     "Interviewer:"="PCP")


black_box_transcripts <- black_box_transcripts %>%
  filter(!(Speaker==""))


black_box_transcripts <- black_box_transcripts %>% 
  filter(!(File %in% blackbox_Transcripts_exclude))


#add utterance order
black_box_transcripts<- black_box_transcripts %>%
  group_by(File) %>%
  mutate(utterance_order = 1:n())


###
target_roles <- c('patient', 'pcp')


black_box_transcripts_clean <- black_box_transcripts%>%
  mutate(
    Text = stringr::str_squish(Text),
    Speaker = stringr::str_squish(Speaker),
    Speaker = tolower(Speaker)) %>%
  select(File, Speaker, Text, timestamp, Word_count, utterance_order) %>%
  mutate(
    to_drop = if_else(
      (!(Speaker %in% target_roles) | # drops all non-target roles
         #drops all target roles surrounded by (likely speaking to) non-target roles
         ((Speaker %in% target_roles) &
            !(lag(Speaker, 1) %in% target_roles) &
            !(lead(Speaker,1) %in% target_roles))),
      1,0)
  )

#this creates a df to see what proportion of each transcript would get 
#excluded from chunk remove
prop_dropped_df <- black_box_transcripts_clean %>% group_by(File) %>%
  summarise(prop_dropped = sum(to_drop) / n())




####################################################################################
#this code was used to drop the transcripts with >20%...there is probably a much cleaner way
#of doing this but looks like it works
prop_dropped_df_drop <- prop_dropped_df %>%
  mutate(drop_transcript= if_else(prop_dropped>=0.20, 1, 0)) %>%
  filter(drop_transcript==1) %>%
  select(File)

vector_blackbox_exclude <-unlist(prop_dropped_df_drop)
list_blackbox_exclude <-as.list(vector_blackbox_exclude)

black_box_transcripts_clean <- black_box_transcripts_clean %>% 
  filter(!(File %in% list_blackbox_exclude))

black_box_transcripts_clean <- black_box_transcripts_clean %>%
  filter(!(to_drop==1))

####################################################################################

#conversation level prep for liwc
blackbox_lsm_prep_final <- black_box_transcripts_clean %>% 
  group_by(File, Speaker) %>%
  summarise(Text = paste(Text, collapse = " ")) 


#using this file to run through LIWC for conversation-level
write.csv(blackbox_lsm_prep_final, "blackbox_lsm_prep_final.csv")


#####################################################################################

#running the following script for the smoothing
blackbox_Transcripts_TbyT_smoothed <- data.frame( # Empty dataframe to store results
  File = character(),
  Speaker = character(),
  Text = character(),
  utterance_order = numeric(),
  Word_count = integer()
)

for (f in unique(black_box_transcripts_clean$File)) { # Iterate through each file
  file_df <- data.frame( # create a df with just that files data; probably ineffecient, but...
    black_box_transcripts_clean[which(black_box_transcripts_clean$File == f),]
  )
  first <- TRUE
  smoothed_chunk <- data.frame( # Create empty dataframe to store smoothed speakers for that file
    File = character(),
    Speaker = character(),
    Text = character(),
    utterance_order = numeric(),
    Word_count = integer()
  )
  for (i in seq_len(nrow(file_df))) { # Iterate through each row in a given file
    if (first == TRUE) { # Just add the first row to the empty df for the file
      smoothed_chunk <- rbind(smoothed_chunk,file_df[i,])
      first <- FALSE
    } else if (file_df[i,'Speaker'] == file_df[i-1,'Speaker']) {
      # This tests to see of the current row's speaker is the same as the previous; if so, it appends the current row's text to the previous one
      smoothed_chunk[nrow(smoothed_chunk),'Text'] <- paste(smoothed_chunk[nrow(smoothed_chunk),'Text'],file_df[i,'Text'])
      smoothed_chunk[nrow(smoothed_chunk),'Word_count'] <- smoothed_chunk[nrow(smoothed_chunk),'Word_count'] + file_df[i,'Word_count']
    } else { # if speakers don't match, just add row to the smoothed df for that file
      smoothed_chunk <- rbind(smoothed_chunk,file_df[i,])
    }
  }
  blackbox_Transcripts_TbyT_smoothed <- rbind(blackbox_Transcripts_TbyT_smoothed,smoothed_chunk) # add the smoothed file to overall smoothed results
}



library(runner)

window_transcripts <- function(transcript_df, window, collapse){
  
  if(missing(collapse)) {collapse <- ' '}
  
  # do some quality control
  if(length(unique(transcript_df$Speaker) )!= 2) {
    print('Too many different speakers...')
  } else {print('Good to go: only 2 speakers')}
  
  windowed_df <- transcript_df %>%
    group_by(File, Speaker) %>%
    mutate(
      text_agg = runner::runner(
        Text,
        f = paste,
        collapse = collapse,
        k = window,
        na_pad = TRUE
      ),
      text_agg_wc = str_count(text_agg, "\\w+")) %>% 
    ungroup()
  
  return(windowed_df)
}

test_window <- window_transcripts(
  transcript_df = blackbox_Transcripts_TbyT_smoothed,
  window = 8,
  collapse = ' '
)

skimr::skim(test_window)


write.csv(test_window, "blackbox_rolling_window_8.csv")


#################################################################################################################
#doing conversation-level LSM analysis and joining with survey data
################################################################################################################# 

f_dir <- '/Users/mrosen44/Johns\ Hopkins/Salar\ Khaleghzadegan\ -\ Patient\ _Provider_Communication_Projects/blackbox_study'

blackbox_conv_liwc <- read.csv(here::here(f_dir,"blackbox_conv_liwc.csv"))
#blackbox_conv_liwc<- read_csv(here(config$blackbox_conv_liwc_path, config$blackbox_conv_liwc_name))

#This is for calculating LSM for this data
blackbox_conv_lsm <- blackbox_conv_liwc %>%
  rename(output_order = 'X') %>%
  #deleting all punctuations
  select(-one_of(c("Segment", "AllPunc","Period","Comma", "QMark",
                   "Exclam", "Apostro", "OtherP"))) %>%
  #creating LSM scores
  select(-Text) %>%
  select(-output_order) %>%
  pivot_longer(WC:filler) %>%
  pivot_wider(names_from = Speaker, values_from = value) %>%
  mutate(LIWC_new = (1 - abs(pcp - patient) / (pcp + patient + .0001))) %>%
  select(-pcp, -patient) %>%
  pivot_wider(names_from = name, names_prefix = "LSM_", values_from = LIWC_new) %>%
  #creating overall LSM metric for functions by getting average of auxiliary verbs,
  #articles, common adverbs, personal pronouns, indefinite pronouns, prepositions,
  #negations, conjunctions, quantifiers (missing LSM_ppronoun for now)
  rowwise() %>%
  mutate(LSM_function_mean = mean(c(LSM_auxverb, LSM_article, LSM_adverb, LSM_ipron, LSM_prep,
                                    LSM_negate, LSM_conj, LSM_quantity, LSM_ppron))) %>%
  mutate(conv.psychological.match = mean(c(LSM_Drives, LSM_Cognition,
                                           LSM_Affect, LSM_Social))) %>%
  mutate(conv.auxverb.match = LSM_auxverb,
         conv.article.match = LSM_article,
         conv.adverb.match = LSM_adverb,
         conv.ipron.match = LSM_ipron,
         conv.prep.match = LSM_prep,
         conv.negate.match = LSM_negate,
         conv.conj.match = LSM_conj,
         conv.quantity.match = LSM_quantity,
         conv.ppron.match = LSM_ppron)

#LSM CALCULATION FOR OTHER LIWC VARIABLES
# conv.Drives.match = LSM_Drives,
# conv.affiliation.match = LSM_affiliation,
# conv.achieve.match = LSM_achieve,
# conv.power.match = LSM_power,
# conv.Cognition.match = LSM_Cognition,
# conv.allnone.match = LSM_allnone,
# conv.cogproc.match = LSM_cogproc,
# conv.memory.match = LSM_memory,
# conv.Affect.match = LSM_Affect,
# conv.tone_pos.match = LSM_tone_pos,
# conv.tone_neg.match = LSM_tone_neg,
# conv.emotion.match = LSM_emotion,
# conv.emo_pos.match = LSM_emo_pos,
# conv.emo_neg.match = LSM_emo_neg,
# conv.emo_anx.match = LSM_emo_anx,
# conv.emo_anger.match = LSM_emo_anger,
# conv.emo_sad.match = LSM_emo_sad,
# conv.swear.match = LSM_swear,
# conv.Social.match = LSM_Social,
# conv.socbehav.match = LSM_socbehav,
# conv.prosocial.match = LSM_prosocial,
# conv.polite.match = LSM_polite,
# conv.conflict.match = LSM_conflict,
# conv.moral.match = LSM_moral,
# conv.comm.match = LSM_comm,
# conv.socrefs.match = LSM_socrefs,
# conv.Culture.match = LSM_Culture,
# conv.ethnicity.match = LSM_ethnicity,
# conv.Lifestyle.match = LSM_Lifestyle,
# conv.leisure.match = LSM_leisure,
# conv.home.match = LSM_home,
# conv.work.match = LSM_work,
# conv.money.match = LSM_money,
# conv.relig.match = LSM_relig,
# conv.Physical.match = LSM_Physical,
# conv.health.match = LSM_health,
# conv.illness.match = LSM_illness,
# conv.wellness.match = LSM_wellness,
# conv.mental.match = LSM_mental,
# conv.substances.match = LSM_substances,
# conv.death.match = LSM_death,
# conv.need.match = LSM_need,
# conv.want.match = LSM_want,
# conv.acquire.match = LSM_acquire,
# conv.fulfill.match = LSM_fulfill,
# conv.fatigue.match = LSM_fatigue,
# conv.reward.match = LSM_reward,
# conv.risk.match = LSM_risk,
# conv.curiosity.match = LSM_curiosity,
# conv.allure.match = LSM_allure,
# conv.Perception.match = LSM_Perception,
# conv.attention.match = LSM_attention,
# conv.feeling.match = LSM_feeling,
# conv.focuspast.match = LSM_focuspast,
# conv.focuspresent.match = LSM_focuspresent,
# conv.focusfuture.match = LSM_focusfuture,
# conv.Conversation.match = LSM_Conversation


#merge blackbox_conv_lsm and black_box_survey_data_update
#three of the transcripts didn't have survey data--> CHECK WITH TEAM
blackbox_conv_lsm <- left_join(blackbox_conv_lsm, black_box_survey_data_update, by = c("File" = "surveyid"))

#this is the code to send Molly liwced transcripts plus the surveys
# blackbox_conv_liwc_survey <- left_join(blackbox_conv_liwc, black_box_survey_data_update, by = c("File" = "surveyid"))

# write.csv(blackbox_conv_liwc_survey, "blackbox_conv_liwc_survey.csv")

#################################################################################################################
#turn-by-turn analysis code
################################################################################################################# 

#reading in the liwced version of 
blackbox_rolling_window_8_liwc <- read_csv(here::here(f_dir,"blackbox_rolling_window_8_liwc.csv"))


# Get the IDs that have a total count of at least 100 turns
# selected_ids <- counts_blackbox_rolling_window_8 %>%
#   filter(count >= 100) %>%
#   pull(File)

# Filter the original dataframe based on selected IDs
blackbox_rolling_window_8_liwc_v2 <- blackbox_rolling_window_8_liwc %>%
  # filter(File %in% selected_ids)
  group_by(File) %>%
  filter(n() > 100) %>% ungroup()



#######
#this is the original version of the blackbox_rolling_window_8_liwc without the low 
#turncount transcripts excluded
# blackbox_tbyt_rollingwindow <- blackbox_rolling_window_8_liwc %>%
#This is the version of the blackbox_rolling_window_8_liwc that removes transcripts with lower turn #'s
blackbox_tbyt_rollingwindow <- blackbox_rolling_window_8_liwc_v2 %>%
  select(-'...1') %>%
  select(-Text) %>%
  select(-timestamp) %>%
  select(-utterance_order) %>%
  select(-to_drop) %>%
  select(-Word_count) %>%
  select(-text_agg_wc) %>%
  select(-Segment) 

# test <- blackbox_tbyt_rollingwindow %>%
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


blackbox_smoothed_rw.rLSM <- blackbox_tbyt_rollingwindow %>%
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
  #mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
  # mutate(rLSM = ifelse(is.na(rLSM), 0, rLSM)) %>% # replaces NAs with 0
  # group_by(File,Speaker) %>%
  # summarize(rLSM = mean(rLSM)) %>%
  # ungroup() %>%
  group_by(File,Speaker) %>%
  summarize(
    across(contains('.rw.rLSM'), .fns = ~ mean(.x,na.rm=TRUE)),
    rw.WC_sum = sum(WC),
    rw.WPS_avg = mean(WPS)) %>%
  ungroup() %>%
  mutate(rw.rLSM = rowMeans(select(.,contains('.rw.rLSM')),na.rm = TRUE)) %>%
  pivot_wider(
    id_cols = File,
    names_from = Speaker,
    values_from = c(rw.rLSM,rw.WPS_avg,rw.WC_sum),
    # names_prefix = 'rw.rLSM.'
    names_glue = "{.value}.{Speaker}"
  ) %>%
  # drop_na() %>% # find out where these are coming from
  mutate(
    mean.rw.rLSM = rowMeans(select(.,contains('rw.rLSM')), na.rm = TRUE),
    ratio.rw.rLSM.patient = rw.rLSM.patient / rw.rLSM.pcp,
    ratio.rw.rLSM.pcp = rw.rLSM.pcp / rw.rLSM.patient,
    rw.verb_dom = rw.WC_sum.pcp / rw.WC_sum.patient
  ) %>%
  # filter(
  #   rw.WC_sum.pcp >= 50 & rw.WC_sum.patient >= 50
  # ) %>%
  select(-c(rw.WPS_avg.pcp, rw.WPS_avg.patient))


#################################################################
#MERGING ALL LSM TYPES AND SURVEYS
#################################################################

#merging the rolling window rLSM results with the previous df that combined conv-level LSM w/surveys
blackbox_lsm_all_matching <- left_join(blackbox_conv_lsm, blackbox_smoothed_rw.rLSM, by = "File")

blackbox_lsm_all_matching <-blackbox_lsm_all_matching %>%
  filter(!is.na(rw.rLSM.pcp))


################################################################
###dichotomizing outcome variables
################################################################

#dichotomizing the pt_cahps_1_20_score variable
blackbox_lsm_all_matching <- blackbox_lsm_all_matching %>%
  rowwise() %>%
  mutate(pt_cahps_1_20_score_high = ifelse(pt_cahps_1_20_score == 4, 1, 0))%>%
  mutate(pt_cahps_1_20_score_high = factor(pt_cahps_1_20_score_high))

#dichotomizing the pt_respect_score variable
blackbox_lsm_all_matching <- blackbox_lsm_all_matching %>%
  rowwise() %>%
  mutate(pt_respect_score_high = ifelse(pt_respect_score == 4, 1, 0)) %>%
  mutate(pt_respect_score_high = factor(pt_respect_score_high))

#dichotomizing the pt_cc_score variable
blackbox_lsm_all_matching <- blackbox_lsm_all_matching %>%
  rowwise() %>%
  mutate(pt_cc_score_high = ifelse(pt_cc_score == 10, 1, 0)) %>%
  mutate(pt_cc_score_high = factor(pt_cc_score_high))

#dichotomizing the pt_trust_score variable
blackbox_lsm_all_matching <- blackbox_lsm_all_matching %>%
  rowwise() %>%
  mutate(pt_trust_score_high = ifelse(pt_trust_score == 4, 1, 0))  %>%
  mutate(pt_trust_score_high = factor(pt_trust_score_high))



################################################################
###joining with accommodation 
################################################################

#reading in the version 3 of the accommodation metrics shared by Mike
# bb_accomodation_v3 <- read.csv("bb_accomodation_v3.csv")

# this is the merge for the previous version 
# blackbox_lsm_all_matching_accom <- left_join(blackbox_lsm_all_matching, accom_cmb, by = "File")
# blackbox_lsm_all_matching_accom <- left_join(blackbox_lsm_all_matching, bb_accomodation_v3, by = "File")

blackbox_lsm_all_matching_accom <- full_join(blackbox_lsm_all_matching, accom_cmb, by = "File")


# num_turns <- blackbox_rolling_window_8_liwc_v2 %>% group_by(File) %>% summarize(turns = max(utterance_order))
# 
# blackbox_lsm_all_matching_accom <- blackbox_lsm_all_matching_accom %>%
#   left_join(num_turns, by = "File") %>% rowwise() %>%
#   mutate(
#     accom_raw_patient_chng = accom_raw_patient*turns,
#     accom_raw_pcp_chng = accom_raw_pcp*turns)
# 
# hist(blackbox_lsm_all_matching_accom$turns
#      )
################################################################
###Making the dataframes for regression
################################################################


################################################################################################
#Aim 2.1: testing whether patient race and/or cultural competence is predictive of LSM
################################################################################################

H2.1_bb_combined_variables <- c('rw.rLSM.patient', 'rw.rLSM.pcp', 'prov_id', 'p_mainrace', 
                                'LSM_function_mean', 'pcp_cc_score', 
                                "accom_raw_patient","accom_raw_pcp",
                                'accom_10_seg_patient','accom_10_seg_pcp', 
                                'accom_20_seg_patient', 'accom_20_seg_pcp',
                                'accom_raw_patient','accom_raw_pcp',
                                # 'accom_raw_patient_chng','accom_raw_pcp_chng',
                                'pcp_cc_vdp_score')

H2.1_df <- blackbox_lsm_all_matching_accom %>%
  as_tibble() %>% 
  dplyr::select(H2.1_bb_combined_variables) %>%
  mutate(p_mainrace = factor(p_mainrace)) %>%
  mutate(prov_id= as.character(prov_id)) %>%
  tidyr::drop_na()

H2.1_df <- H2.1_df %>%
  mutate(rw.rLSM.patient.scaled = scale(rw.rLSM.patient, center = TRUE, scale = TRUE)) %>%
  mutate(rw.rLSM.pcp.scaled = scale(rw.rLSM.pcp, center = TRUE, scale = TRUE)) %>%
  mutate(LSM_function_mean.scaled = scale(LSM_function_mean, center = TRUE, scale = TRUE)) %>%
  
  mutate(accom_raw_patient.scaled = scale(accom_raw_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_raw_pcp.scaled = scale(accom_raw_pcp, center = TRUE, scale = TRUE)) %>%
  
  # mutate(accom_raw_patient_chng.scaled = scale(accom_raw_patient_chng, center = TRUE, scale = TRUE)) %>%
  # mutate(accom_raw_pcp_chng.scaled = scale(accom_raw_pcp_chng, center = TRUE, scale = TRUE)) %>%
  # 
  mutate(accom_10_seg_patient.scaled = scale(accom_10_seg_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_10_seg_pcp.scaled = scale(accom_10_seg_pcp, center = TRUE, scale = TRUE)) %>%
  mutate(accom_20_seg_patient.scaled = scale(accom_20_seg_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_20_seg_pcp.scaled = scale(accom_20_seg_pcp, center = TRUE, scale = TRUE))


table(H2.1_df$pcp_cc_score_high)

#median was 9.166667
H2.1_df <- H2.1_df %>%
  mutate(
    pcp_cc_vdp_score_high = ifelse(pcp_cc_vdp_score >= median(pcp_cc_vdp_score), 1, 0),
    pcp_cc_score_high = ifelse(pcp_cc_score >= median(pcp_cc_score), 1, 0),
    pcp_cc_vdp_score_high = factor(pcp_cc_vdp_score_high),
    pcp_cc_score_high = factor(pcp_cc_score_high)
    )


#this is the code for dichotomizing pcp_cc_vdp_score using "top box" approach.
# H2.1_df <- H2.1_df %>% 
#   mutate(pcp_cc_vdp_score_high = ifelse(pcp_cc_vdp_score ==10, 1, 0))

#this is the code for dichotomizing pcp_cc_vdp_score using "median split" approach.
#might want to use this version instead of the previous one since there is more of distribution
# H2.1_df <- H2.1_df %>%
#   mutate(pcp_cc_score_high = ifelse(pcp_cc_score >= median(pcp_cc_score), 1, 0))


#changing the dichotomized version of pcp_cc_vdp_score and pcp_cc_score to a factor variables
# H2.1_df <- H2.1_df %>%
#   mutate(pcp_cc_vdp_score_high = factor(pcp_cc_vdp_score_high)) %>%
#   mutate()



#only keeping data for white (p_mainrace==1) and black (p_mainrace==2) patients
#REMOVE THIS CODE IF WE DON'T PLAN ON CONTROLING FOR RACE
H2.1_df <- H2.1_df %>%
  filter(!(p_mainrace==3)) %>%
  filter(!(p_mainrace==7))

  
#drop physicians with < 5 cases
H2.1_df <- H2.1_df %>%
  group_by(prov_id) %>%
  filter(n() > 4) %>%
  ungroup() %>%
  gdata::drop.levels(.)

hist(H2.1_df$accom_raw_pcp
     )

# H2.1_df$accom_raw_pcp_dicho <- as.factor(if_else(H2.1_df$accom_raw_pcp >=.5,1,0))


################################################################################################
#Aim 2.2a: testing whether LSM is predictive of patient modified CAHPS score
################################################################################################

H2.2a_bb_combined_variables <- c('rw.rLSM.patient', 'rw.rLSM.pcp', 
                                 'pt_cahps_1_20_score_high', 'pt_cahps_1_20_score',
                                 'prov_id', 'p_mainrace', 'LSM_function_mean', 
                                 "accom_raw_patient","accom_raw_pcp",
                                 # 'accom_raw_patient_chng','accom_raw_pcp_chng',
                                 'accom_10_seg_patient','accom_10_seg_pcp', 
                                 'accom_20_seg_patient', 'accom_20_seg_pcp')

H2.2a_df <- blackbox_lsm_all_matching_accom %>%
  as_tibble() %>% 
  dplyr::select(H2.2a_bb_combined_variables) %>%
  mutate(p_mainrace = factor(p_mainrace)) %>%
  mutate(prov_id= as.character(prov_id)) %>%
  tidyr::drop_na()

skimr::skim(H2.2a_df)

#SCALING LSM VARIABLES
H2.2a_df <- H2.2a_df %>%
  mutate(rw.rLSM.patient.scaled = scale(rw.rLSM.patient, center = TRUE, scale = TRUE)) %>%
  mutate(rw.rLSM.pcp.scaled = scale(rw.rLSM.pcp, center = TRUE, scale = TRUE)) %>%
  mutate(LSM_function_mean.scaled = scale(LSM_function_mean, center = TRUE, scale = TRUE)) %>%
  
  mutate(accom_raw_patient.scaled = scale(accom_raw_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_raw_pcp.scaled = scale(accom_raw_pcp, center = TRUE, scale = TRUE)) %>%
  
  
  # mutate(accom_raw_patient_chng.scaled = scale(accom_raw_patient_chng, center = TRUE, scale = TRUE)) %>%
  # mutate(accom_raw_pcp_chng.scaled = scale(accom_raw_pcp_chng, center = TRUE, scale = TRUE)) %>%
  # 
  mutate(accom_10_seg_patient.scaled = scale(accom_10_seg_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_10_seg_pcp.scaled = scale(accom_10_seg_pcp, center = TRUE, scale = TRUE)) %>%
  mutate(accom_20_seg_patient.scaled = scale(accom_20_seg_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_20_seg_pcp.scaled = scale(accom_20_seg_pcp, center = TRUE, scale = TRUE))

#only keeping data for white (p_mainrace==1) and black (p_mainrace==2) patients
#REMOVE THIS CODE IF WE DON'T PLAN ON CONTROLING FOR RACE
H2.2a_df <- H2.2a_df %>%
  filter(!(p_mainrace==3)) %>%
  filter(!(p_mainrace==7))

#drop physicians with < 6 cases
H2.2a_df <- H2.2a_df %>%
  group_by(prov_id) %>%
  filter(n() > 4) %>%
  ungroup() %>%
  gdata::drop.levels(.)

#checking distribution of new dichotomous measure
#pt_cahps_1_20_score_high==1 (90 transcripts), pt_cahps_1_20_score_high== 0 (223 transcripts)
# table(H2.2a_df$pt_cahps_1_20_score_high)



 
################################################################################################
#Aim 2.2b: testing whether LSM is predictive of patient rating of clinician cultural comptence
################################################################################################

H2.2b_bb_combined_variables <- c('rw.rLSM.patient', 'rw.rLSM.pcp', 
                                 'pt_cc_score', 'pt_cc_score_high', 
                                 'prov_id', 'p_mainrace', 'LSM_function_mean',
                                 "accom_raw_patient","accom_raw_pcp",
                                 # 'accom_raw_patient_chng','accom_raw_pcp_chng',
                                 'accom_10_seg_patient', 'accom_10_seg_pcp', 
                                 'accom_20_seg_patient', 'accom_20_seg_pcp')

H2.2b_df <- blackbox_lsm_all_matching_accom %>%
  as_tibble() %>% 
  dplyr::select(H2.2b_bb_combined_variables) %>%
  mutate(p_mainrace = factor(p_mainrace)) %>%
  mutate(prov_id= as.character(prov_id)) %>%
  tidyr::drop_na()

#SCALING LSM VARIABLES
H2.2b_df <- H2.2b_df %>%
  mutate(rw.rLSM.patient.scaled = scale(rw.rLSM.patient, center = TRUE, scale = TRUE)) %>%
  mutate(rw.rLSM.pcp.scaled = scale(rw.rLSM.pcp, center = TRUE, scale = TRUE)) %>%
  mutate(LSM_function_mean.scaled = scale(LSM_function_mean, center = TRUE, scale = TRUE)) %>%
  
  mutate(accom_raw_patient.scaled = scale(accom_raw_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_raw_pcp.scaled = scale(accom_raw_pcp, center = TRUE, scale = TRUE)) %>%
  
  
  # mutate(accom_raw_patient_chng.scaled = scale(accom_raw_patient_chng, center = TRUE, scale = TRUE)) %>%
  # mutate(accom_raw_pcp_chng.scaled = scale(accom_raw_pcp_chng, center = TRUE, scale = TRUE)) %>%
  # 
  mutate(accom_10_seg_patient.scaled = scale(accom_10_seg_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_10_seg_pcp.scaled = scale(accom_10_seg_pcp, center = TRUE, scale = TRUE)) %>%
  mutate(accom_20_seg_patient.scaled = scale(accom_20_seg_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_20_seg_pcp.scaled = scale(accom_20_seg_pcp, center = TRUE, scale = TRUE))

#only keeping data for white (p_mainrace==1) and black (p_mainrace==2) patients
#REMOVE THIS CODE IF WE DON'T PLAN ON CONTROLING FOR RACE
H2.2b_df <- H2.2b_df %>%
  filter(!(p_mainrace==3)) %>%
  filter(!(p_mainrace==7))

#drop physicians with < 6 cases
H2.2b_df <- H2.2b_df %>%
  group_by(prov_id) %>%
  filter(n() > 4) %>%
  ungroup() %>%
  gdata::drop.levels(.)

# #checking distribution of new dichotomous measure
# #pt_cc_score_high==1 (76 transcripts), pt_cc_score_high== 0 (216transcripts)
# table(H2.2b_df$pt_cc_score_high)


################################################################################################
#Aim 2.2c: testing whether LSM is predictive of patient rating of trust in clinician
################################################################################################

H2.2c_bb_combined_variables <- c('rw.rLSM.patient', 'rw.rLSM.pcp', 
                                 'pt_trust_score', 'pt_trust_score_high', 
                                 'prov_id', 'p_mainrace', 'LSM_function_mean',
                                 "accom_raw_patient","accom_raw_pcp",
                                 # 'accom_raw_patient_chng','accom_raw_pcp_chng',
                                 'accom_10_seg_patient', 'accom_10_seg_pcp', 
                                 'accom_20_seg_patient', 'accom_20_seg_pcp')

H2.2c_df <- blackbox_lsm_all_matching_accom %>%
  as_tibble() %>% 
  dplyr::select(H2.2c_bb_combined_variables) %>%
  mutate(p_mainrace = factor(p_mainrace)) %>%
  mutate(prov_id= as.character(prov_id)) %>%
  tidyr::drop_na()

#SCALING LSM VARIABLES
H2.2c_df <- H2.2c_df %>%
  mutate(rw.rLSM.patient.scaled = scale(rw.rLSM.patient, center = TRUE, scale = TRUE)) %>%
  mutate(rw.rLSM.pcp.scaled = scale(rw.rLSM.pcp, center = TRUE, scale = TRUE)) %>%
  mutate(LSM_function_mean.scaled = scale(LSM_function_mean, center = TRUE, scale = TRUE)) %>%
  
  
  mutate(accom_raw_patient.scaled = scale(accom_raw_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_raw_pcp.scaled = scale(accom_raw_pcp, center = TRUE, scale = TRUE)) %>%
  
  
  # mutate(accom_raw_patient_chng.scaled = scale(accom_raw_patient_chng, center = TRUE, scale = TRUE)) %>%
  # mutate(accom_raw_pcp_chng.scaled = scale(accom_raw_pcp_chng, center = TRUE, scale = TRUE)) %>%
  # 
  mutate(accom_10_seg_patient.scaled = scale(accom_10_seg_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_10_seg_pcp.scaled = scale(accom_10_seg_pcp, center = TRUE, scale = TRUE)) %>%
  mutate(accom_20_seg_patient.scaled = scale(accom_20_seg_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_20_seg_pcp.scaled = scale(accom_20_seg_pcp, center = TRUE, scale = TRUE))

#only keeping data for white (p_mainrace==1) and black (p_mainrace==2) patients
#REMOVE THIS CODE IF WE DON'T PLAN ON CONTROLING FOR RACE
H2.2c_df <- H2.2c_df %>%
  filter(!(p_mainrace==3)) %>%
  filter(!(p_mainrace==7))

#drop physicians with < 6 cases
H2.2c_df <- H2.2c_df %>%
  group_by(prov_id) %>%
  filter(n() > 4) %>%
  ungroup() %>%
  gdata::drop.levels(.)

# #checking distribution of new dichotomous measure
# #pt_trust_score_high==1 (214 transcripts), pt_trust_score_high== 0 (98 transcripts)
# table(H2.2c_df$pt_trust_score_high)

################################################################################################
#Aim 2.2d: testing whether LSM is predictive of patient rating of being respected
################################################################################################

H2.2d_bb_combined_variables <- c('rw.rLSM.patient', 'rw.rLSM.pcp',
                                 'pt_respect_score', 'pt_respect_score_high',
                                 'prov_id', 'p_mainrace', 'LSM_function_mean',
                                 "accom_raw_patient","accom_raw_pcp",
                                 # 'accom_raw_patient_chng','accom_raw_pcp_chng',
                                 'accom_10_seg_patient', 'accom_10_seg_pcp',
                                 'accom_20_seg_patient', 'accom_20_seg_pcp')

H2.2d_df <- blackbox_lsm_all_matching_accom %>%
  as_tibble() %>%
  dplyr::select(H2.2d_bb_combined_variables) %>%
  mutate(p_mainrace = factor(p_mainrace)) %>%
  mutate(prov_id= as.character(prov_id)) %>%
  tidyr::drop_na()

#SCALING LSM VARIABLES
H2.2d_df <- H2.2d_df %>%
  mutate(rw.rLSM.patient.scaled = scale(rw.rLSM.patient, center = TRUE, scale = TRUE)) %>%
  mutate(rw.rLSM.pcp.scaled = scale(rw.rLSM.pcp, center = TRUE, scale = TRUE)) %>%
  mutate(LSM_function_mean.scaled = scale(LSM_function_mean, center = TRUE, scale = TRUE)) %>%
  
  
  mutate(accom_raw_patient.scaled = scale(accom_raw_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_raw_pcp.scaled = scale(accom_raw_pcp, center = TRUE, scale = TRUE)) %>%
  
  
  # mutate(accom_raw_patient_chng.scaled = scale(accom_raw_patient_chng, center = TRUE, scale = TRUE)) %>%
  # mutate(accom_raw_pcp_chng.scaled = scale(accom_raw_pcp_chng, center = TRUE, scale = TRUE)) %>%
  # 
  mutate(accom_10_seg_patient.scaled = scale(accom_10_seg_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_10_seg_pcp.scaled = scale(accom_10_seg_pcp, center = TRUE, scale = TRUE)) %>%
  mutate(accom_20_seg_patient.scaled = scale(accom_20_seg_patient, center = TRUE, scale = TRUE)) %>%
  mutate(accom_20_seg_pcp.scaled = scale(accom_20_seg_pcp, center = TRUE, scale = TRUE))

#only keeping data for white (p_mainrace==1) and black (p_mainrace==2) patients
#REMOVE THIS CODE IF WE DON'T PLAN ON CONTROLING FOR RACE
H2.2d_df <- H2.2d_df %>%
  filter(!(p_mainrace==3)) %>%
  filter(!(p_mainrace==7))

#drop physicians with <5 cases
H2.2d_df <- H2.2d_df %>%
  group_by(prov_id) %>%
  filter(n() > 4) %>%
  ungroup() %>%
  gdata::drop.levels(.)

#  
# #checking distribution of new dichotomous measure
# #pt_respect_score_high==1 (73 transcripts), pt_respect_score_high== 0 (240 transcripts)
# table(H2.2d_df$pt_respect_score_high)

