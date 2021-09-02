##########################################################################
##########################################################################
################# R functions for Patient MD Communication Analysis
##########################################################################
##########################################################################


library(docxtractr)
library(janitor)
library(tidyverse)
library(stringr)

all_filenames <- list.files(
  path = "/Users/skhaleg1/OneDrive - Johns Hopkins/Patient Provider Communication Projects/Analysis/Patient_MD_Com", full.names = FALSE,
  pattern = ".docx", recursive = TRUE)

data_cleaning_transcript <- function(.x) {
  #Import raw data from MS Word file
  audio_doc <- docxtractr::read_docx(.x)
  
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


big_tibble_test <-all_filenames %>% purrr::map_dfr(~data_cleaning_transcript(.))


#creating a variable that has utterance order for each transcript
all_transcripts_final <-big_tibble %>% group_by(patient_id) %>% mutate(utterance_order = 1:n())




