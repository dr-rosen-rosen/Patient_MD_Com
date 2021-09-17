##########################################################################
##########################################################################
################# R functions for Patient MD Communication Analysis
##########################################################################
##########################################################################


library(docxtractr)
library(janitor)
library(tidyverse)
library(stringr)

##########################################################################
################# Functions for pre-processing transcript files
##########################################################################

#initial data cleaning function
get_and_clean_one_transcript <- function(transcript) {
  #Import raw data from MS Word file
  audio_doc <- docxtractr::read_docx(transcript)
  
  #extracting the tables from 
  audio_doc_tbl1 <- docx_extract_tbl(audio_doc, 1, header = FALSE)
  audio_doc_tbl2 <- docx_extract_tbl(audio_doc, 2, header = FALSE)
  
  # Make the columns 'date' and 'input_recording', Make the columns 'date' 
  #and 'input_recording'and clean the column names
  audio_doc_tbl1 <- audio_doc_tbl1 %>%
    tidyr::pivot_wider(names_from = V1, values_from = V2) %>%
    janitor::clean_names()
  
  # Use `dplyr::mutate()` to add two columns,  to audio_doc_tbl2: 'date' and 'conversation_id', 
  #and with their values from audio_doc_tbl1
  audio_doc_final <- audio_doc_tbl2 %>%
    dplyr::mutate(date = audio_doc_tbl1$date, conversation_id = audio_doc_tbl1$input_sound_file)
  
  #removing the .MP3 extension from the "conversation_id" and renaming that variable to "patient_id"
  audio_doc_final <- audio_doc_final %>%
    separate("conversation_id", c("patient_id"), ".MP3")
}


get_and_clean_all_transcripts <- function(all_filenames) {
  #read in and clean each transcript and return in dataframe 
  big_tibble <- all_filenames %>% purrr::map_dfr(~get_and_clean_one_transcript(.))
  
  #creating a variable that has utterance order for each transcript
  all_transcripts_final <- big_tibble %>% group_by(patient_id) %>% mutate(utterance_order = 1:n())
  
  #cleaning to make timestamp and role variables
  all_transcripts_final <- all_transcripts_final %>%
    filter(V1 != "") %>%
    filter(V1 != "?") %>%
    filter(V1 != "[silence]") %>%
    filter(str_detect(V1, "")) %>%
    mutate(timestamp = str_match(V1, "(\\d{2}:\\d{2})")[, 2]) %>%
    mutate(role1 = str_match(V1, "((?i)doctor)")[, 2]) %>%
    mutate(role2 = str_match(V1, "((?i)patient)")[, 2]) %>%
    mutate(role3 = str_match(V1, "((?i)other)")[, 2]) %>%
    mutate(role4 = str_match(V1, "((?i)S1)")[, 2]) %>%
    mutate(role5 = str_match(V1, "((?i)S2)")[, 2]) %>%
    mutate(role6 = str_match(V1, "((?i)S3)")[, 2]) %>%
    mutate(role7 = str_match(V1, "((?i)S4)")[, 2]) %>%
    mutate(role8 = str_match(V1, "((?i)S5)")[, 2]) %>%
    mutate(role9 = str_match(V1, "((?i)S6)")[, 2]) %>%
    mutate(role10 = str_match(V1, "((?i)S7)")[, 2]) %>%
    mutate(role11 = str_match(V1, "((?i)S8)")[, 2]) %>%
    mutate(role12 = str_match(V1, "((?i)S9)")[, 2]) %>%
    mutate(role13 = str_match(V1, "((?i)Provider)")[, 2]) %>%
    mutate(role = coalesce(role1, role2, role3, role4, role5, role6, role7, role8, role9, 
                           role10, role11, role12, role13))
  
  #removing all extra role columns after I made the final role column using coalesce
  all_transcripts_final <-select(all_transcripts_final, -c("role1", "role2", "role3", "role4", 
                                                           "role5", "role6", "role7", "role8", 
                                                           "role9", "role10", "role11", "role12", "role13"))
  
  #remove all content that have brackets [....]
  all_transcripts_final$V2 <- gsub("\\[(.*?)\\]", "", all_transcripts_final$V2)
  
  return(all_transcripts_final)
}

conv_LSM_prep <- function(all_transcripts) {
  #combine all text by patient_id and role
  all_transcripts_combined_roles <- all_transcripts %>% 
    group_by(patient_id, role, date) %>%
    summarise(V2 = paste(V2, collapse = "")) %>% 
    ungroup()
  return(all_transcripts_combined_roles)
}