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
    mutate(conversation_id = tolower(conversation_id)) %>%
    separate("conversation_id", c("patient_id"), ".mp3") %>%
    separate("patient_id", c("patient_id"), ".wav")
  
  return(audio_doc_final)
}

get_and_clean_all_transcripts <- function(all_filenames, annies_role_file) {
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
  #renaming the transcript text and id variables
  all_transcripts_final <- rename(all_transcripts_final, transcript_id = patient_id) 
  all_transcripts_final <- rename(all_transcripts_final, transcript_text = V2) 
  #all_transcripts_final <- all_transcripts_final %>% mutate(role = tolower(role))
  
  if (!is.na(annies_role_file)) { # This will run this version of speaker recoding IF 
    # a recoding file path is sent to the function; if that is NA, will skip and 
    # return original speaker codings in the transcript
    
    ####### beginning of new code added by Salar on 09/27/21######
    ##############################################################
    
    #converting the annie_role_file to long form
    #got an error when I ran the code from the 0_main.R ... I think I didn't set up the config.yml script correctly
    
    annies_role_file_long <- readr::read_csv(annies_role_file, show_col_types = FALSE) %>% 
      pivot_longer(!transcript_id, names_to = "role", values_to = "recode")# %>%
      #mutate(recode = tolower(recode))
    
    #transcript_id column in annies_role_file needs to be changed to character format
    
    cols.chr <- c("transcript_id")
    annies_role_file_long[cols.chr]<- sapply(annies_role_file_long[cols.chr], as.character)
    
    #joining the main document (all_transcripts_final) with the annies_role_file_long. This adds the 
    #"recode" column, which was created in annies_role_file_long, to the all_transcripts_final df. 
    #This approach is much uglier than the approach we had discussed and I think I'll cringe if I see 
    #this in the future when I am better at R but I think this seems to be working for what we want to do.
    
    all_transcripts_final <- all_transcripts_final %>% 
      left_join(annies_role_file_long, by = c("transcript_id" = "transcript_id", "role" = "role"))
    
    #removing old role names from the role column so that these cells can be empty and that they can 
    #be replaced with the updated roles after this using "unite". 
    
    all_transcripts_final <- all_transcripts_final %>%
      mutate(across("role", str_replace, "S1", "")) %>%
      mutate(across("role", str_replace, "S2", "")) %>%
      mutate(across("role", str_replace, "S3", "")) %>%
      mutate(across("role", str_replace, "S4", "")) %>%
      mutate(across("role", str_replace, "S5", "")) %>%
      mutate(across("role", str_replace, "S6", "")) %>%
      mutate(across("role", str_replace, "S7", "")) %>%
      mutate(across("role", str_replace, "S8", "")) %>%
      mutate(across("role", str_replace, "S9", "")) %>%
      mutate(across("role", str_replace, "S10", "")) %>%
      mutate(across("role", str_replace, "S11", "")) %>%
      mutate(across("role", str_replace, "S12", "")) %>%
      mutate(across("role", str_replace, "S13", "")) %>%
      mutate(across("role", str_replace, "S?", "")) %>%
      mutate(across("role", str_replace, "Provider", ""))
    
    #uniting "role" and "recode" to make a final "role" column with the unclear roles recoded
    all_transcripts_final <- all_transcripts_final %>%
      unite("role", c("role", "recode"), sep = "", na.rm = TRUE)
    
    ##Deleting all rows where the utterances were unclear to the person who transcribed (S?) and
    ##temporarily deleting 6 transcripts that need to be checked by Annie: 15103801, 17101601, 24145401, 
    ##15134001, 16141601, 37188201. Will update annie_role_file when the roles for these 6 transcripts are clear.
    all_transcripts_final <- all_transcripts_final %>%
      filter(role != "***") %>%
      filter(role != "")
    
    ####### end of new code added by Salar on 09/27/21######
    ########################################################
  }
  #remove all content that have brackets [....]
  all_transcripts_final$transcript_text <- gsub("\\[(.*?)\\]", "", all_transcripts_final$transcript_text)
  # making column for provider_id
  all_transcripts_final <- mutate(all_transcripts_final,
                                            provider_id = str_sub(transcript_id, 1, 2)) 
  
  # making column for patient_id
  all_transcripts_final <- mutate(all_transcripts_final,
                                            patient_id = str_sub(transcript_id, 3, 6))
  
  # making column for patient visit repetition
  all_transcripts_final <- mutate(all_transcripts_final,
                                            visit_repetition = str_sub(transcript_id, 7, 8))
  
  all_transcripts_final <- all_transcripts_final %>% 
    mutate(location = if_else(str_detect(transcript_id, "^8"), "portland", "baltimore"))
  
  return(all_transcripts_final)
}

recode_func <- function(d,transcript, role_f){
  # This creates a named list of old and new roles
  mapper <- role_f %>%
    filter(transcript_id == transcript$transcript_id) %>%
    select(role, recode) %>%
    deframe()
  # checks if there needs to be any recoding done
  if(length(mapper) > 0) {
    # This recodes using above mapper
    print(paste('Recoding...',transcript$transcript_id))
    d <- d %>%
      mutate(role = recode(role, !!!mapper))
  }
  return(d)
}

recode_spkrs <- function(df, role_f) {
  # This function applies the above recoding function to each transcript
  
  #converting the annie_role_file to long form
  #got an error when I ran the code from the 0_main.R ... I think I didn't set up the config.yml script correctly
  annies_role_file_long <- readr::read_csv(role_f, show_col_types = FALSE) %>% 
    pivot_longer(!transcript_id, names_to = "role", values_to = "recode") %>%
    drop_na() #%>%
    #mutate(recode = tolower(recode))
  #transcript_id column in annies_role_file needs to be changed to character format
  cols.chr <- c("transcript_id")
  annies_role_file_long[cols.chr]<- sapply(annies_role_file_long[cols.chr], as.character)
  
  df <- df %>%
    group_by(transcript_id) %>%
    group_modify(~recode_func(d = .x, transcript = .y, role_f = annies_role_file_long)) %>%
    ungroup()
  
  return(df)
}

##########################################################################
################# Functions for running exclusions
##########################################################################


run_exclusions <- function(df){
  
  #removing transcripts with at least 10% of utterances are transcribed [foreign]
  #19106401(protocol event), 20188701, 28146601, 28146602, 37168301
  #removing transcripts with multiple patients
  #11120201, 24110501, 17129904 and 17206401, 
  #removing transcripts with multiple doctors
  #32127001, 33143601
  
  excluded_transcripts <- c("19106401(protocol event)", "20188701", "28146601", "28146602", "37168301",
                            "11120201", "24110501", "17129904", "17206401", 
                            "32127001", "33143601") 
  
  #filtering out the list of transcripts in "excluded_transcript" df
  df_excluded <- df %>% filter(!(transcript_id %in% excluded_transcripts))
  
  #commented out code below is for counting the number of utterances by count instead of sum. Will return to explore more.
  # df_excluded <- df_excluded %>%
  #   group_by(transcript_id) %>%
  #   summarize(proportion_other = count())
  #identifying transcripts with large proportions of "other" speech
  df_excluded_proportions <- df_excluded %>% 
    group_by(transcript_id) %>%
    summarise(other_role = sum(role == "other"), 
              patient_role = sum(role == "patient"),
              doctor_role = sum(role == "doctor")) 
  
  df_excluded_proportions  <- df_excluded_proportions  %>%
    rowwise() %>%
    mutate(doctor_patient = sum(c(patient_role, doctor_role))) %>%
    relocate(other_role, .after = last_col())
  
  #summing the other and doctor/patient speech to create value for all speech
  df_excluded_proportions <-df_excluded_proportions %>%
    rowwise() %>%
    mutate(total_speech = sum(c(doctor_patient, other_role)))
  
  #creating a proportion column by dividing "other_role" by "total_speech"
  df_excluded_proportions$proportion <- df_excluded_proportions$other_role / 
    df_excluded_proportions$total_speech
  
  #Filtering this df to only include transcripts where less than 20% of utterances are by "other"
  df_excluded_proportions <- df_excluded_proportions %>%
    filter(proportion < .2)
  
  #made a list of unique values of "transcript_id" from the df which represents all of the transcripts with less
  #than 20% of utterances by "other"
  df_excluded_proportions <- unique(c(df_excluded_proportions$transcript_id))
  
  #I filtered the larger df "df_excluded" using the list I made in the previous like of code 
  #(which was a list of all transcripts with less than 20% "other")
  df_excluded <- df_excluded %>% filter((transcript_id %in% df_excluded_proportions))
  
  return(df_excluded)
}



  


##########################################################################
################# Functions for LSM
##########################################################################


conv_LSM_prep <- function(recoded_df) {
  #combine all text by patient_id and role
  all_transcripts_combined_roles <- recoded_df %>% 
    group_by(transcript_id, role) %>%
    summarise(transcript_text = paste(transcript_text, collapse = "")) %>% 
    ungroup()
  return(all_transcripts_combined_roles)
}





  LIWC_high_category <- function(LIWC_df) {
    LIWC_df_high <- LIWC_df %>%
      #deleting all punctuations
      filter(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark", 
                       "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP"))) %>%
      #deleting all subcategory variables
      #starting with pronoun subcat
      filter((-one_of(c("ppron", "i", "we", "you", "shehe", "they", "ipron",
                        #deleting negative emotion subcat
                        "anger", "sad"))))
      
  }
    
    
  LIWC_low_category <- function(LIWC_df) {
    LIWC_df_low <- LIWC_df %>%
      #deleting all punctuations
      filter(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark", 
                       "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP")))
      #deleting all main category variables
      filter(-one_of(c("pronoun", "negemo", "social", "cogproc")))
      
  } 
    
    

