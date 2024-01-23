rm(list = ls())
library(tidyverse)
# read in Maripohsa from Salar
mar_df <- read.csv('/Users/mrosen44/Johns\ Hopkins/Salar\ Khaleghzadegan\ -\ Patient\ _Provider_Communication_Projects/maripohsa_study/final_raw_maripohsa.csv') %>%
  mutate(file_study_id = paste0(transcript_id,'-MAR')) %>%
  select(-X,-X.1,-date,-timestamp,-location,-provider_id,-patient_id) %>%
  rename('speaker' = 'role','speech' = 'transcript_text', 'sequence' = 'utterance_order','file_id' = 'transcript_id', 'mar_visit_repetition' = 'visit_repetition')
mar_visit <- mar_df %>%
  select(file_study_id,mar_visit_repetition) %>% distinct()
mar_df <- mar_df %>% select(-mar_visit_repetition,-file_id)
# read in BB from Salar
bb_df <- read.csv('/Users/mrosen44/Johns\ Hopkins/Salar\ Khaleghzadegan\ -\ Patient\ _Provider_Communication_Projects/blackbox_study/final_raw_blackbox.csv') %>%
  mutate(file_study_id = paste0(File,'-BB')) %>%
  rename('word_count' = 'Word_count', 'speaker' = 'Speaker','speech' = 'Text', 'file_id' = 'File') %>%
  select(-X,-timestamp)  %>%
  group_by(file_study_id) %>%
  mutate(sequence = row_number()) %>%
  ungroup() %>%
  select(-file_id)
# get ECHO1 to same point
echo1_df <- read.csv('/Users/mrosen44/Johns\ Hopkins/Salar\ Khaleghzadegan\ -\ Patient\ _Provider_Communication_Projects/ECHO1_study/final_raw_echo1.csv') %>%
  select(-X,-X.1) %>%
  mutate(file_study_id = paste0(File,'-ECHO1')) %>%
  rename('speaker' = 'Speaker','speech' = 'Text', 'file_id' = 'File', 'sequence' = 'Sequence') %>%
  select(-file_id)

# get ECHO3 to the same point
echo3_df <- read.csv('final_raw_ECHO3.csv') %>%
  select(-X,-speaker) %>%
  rename('speaker' = 'spkr_recode') %>%
  mutate(file_study_id = paste0(file_id,'-ECHO3')) %>%
  select(-file_id)
  

cmbd_transcripts <- bind_rows(echo1_df,echo3_df,bb_df,mar_df) %>%
  mutate(speaker = tolower(speaker),
         speaker = case_match(speaker,
                              c("family","nurse","other speaker","doctor 2") ~ 'other',
                              c('pcp','doctor') ~ 'clinician',
                              .default = speaker)
  )
  
file_metrics <- cmbd_transcripts %>%
  filter(word_count > 2) %>%
  group_by(file_study_id) %>%
  summarize(
    tot_wc = sum(word_count),
    tot_turns = n()
  )

other_metrics <- cmbd_transcripts %>%
  filter(word_count > 2 & speaker == 'other') %>%
  group_by(file_study_id) %>%
  summarize(
    tot_other_wc = sum(word_count),
    tot_other_turns = n()
  )

file_metrics <- file_metrics %>%
  full_join(other_metrics, by = 'file_study_id') %>%
  full_join(mar_visit, by = 'file_study_id') %>%
  mutate(
    prop_other_wc = tot_other_wc / tot_wc,
    prop_other_turn = tot_other_turns / tot_turns
  ) %>%
  mutate_all(~replace(., is.na(.), 0))

source('/Users/mrosen44/Documents/Data_Analysis_Local/pediatricSurgeryCommunication/2_transcriptCleaning.R')
smthd <- dropRolesAndSmoothe(
  df = cmbd_transcripts, 
  target_roles = c('clinician','patient'), 
  file_col = 'file_study_id',
  spkr_col = 'speaker', 
  seq_col = 'sequence', 
  speech_col = 'speech') %>% 
  select(-to_drop)

# write files for align python package
smthd |>
  select(file_study_id,speaker,speech_sm) |>
  rename(participant = speaker, content = speech_sm) |>
  group_by(file_study_id) |>
  group_walk(~ write_delim(.x, here::here('align_files',paste0(.y$file_study_id, ".txt")), delim = '\t'))


windowed <- window_transcripts(
  df = smthd, 
  file_col = 'file_study_id',
  spkr_col = 'speaker', 
  speech_col = 'speech_sm', 
  window = 8, 
  collapse = " ") %>%
  filter(speech_agg_wc > 0)
write.csv(windowed,'FINAL_cmbd_windowed.csv')
# run liwc
# windowed_liwcd <- read.csv('LIWC-22 Results - windowed - LIWC Analysis.csv')


smthd %>%
  group_by(file_study_id,speaker) %>%
  summarize(speech_agg = paste(speech_sm, collapse = ' ')) %>% # speech_sm is the non-windowed text
  # openxlsx::write.xlsx(.,file = 'FINAL_cmbd_conv.xlsx')
  write.csv('FINAL_cmbd_conv.csv')
# run liwc

# get file for baseline LSM
smthd %>%
  group_by(file_study_id,speaker) %>%
  mutate(
    # the cut function allows chunking into a dynamically specified number of bits
    time.point = cut(row_number(), b = 4, labels = FALSE)
  ) %>%
  ungroup() %>%
  filter(time.point == 1) %>%
  group_by(file_study_id,speaker) %>%
  summarize(speech_agg = paste(speech_sm, collapse = ' ')) %>% # speech_sm is the non-windowed text
  write.csv(.,'baselineLSM.csv')
  
# conv_liwcd <- read.csv('LIWC-22 Results - FINAL_cmbd_conv - LIWC Analysis.csv')