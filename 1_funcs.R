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

all_filenames <- list.files(
  path = "/Users/skhaleg1/OneDrive - Johns Hopkins/Patient _Provider_Communication_Projects/Analysis/Maripohsa_transcripts", full.names = TRUE,
  pattern = ".docx", recursive = TRUE)


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


#Here is a function to include variables that were decided by group to be included in analysis
LIWC_Final_Inclusion <- function(LIWC_df) {
  LIWC_df <- LIWC_df %>%
    #deleting all punctuations
    select(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark",
                     "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP")))
  #renamed function variable to funct as indicated on LIWC manual
  LIWC_df <- rename(LIWC_df, funct = "function.")

  #making new variables for the sum of all subcategories of LIWC main categories
  LIWC_df <- LIWC_df %>%
    rowwise() %>%
    mutate(percept_sum = sum(c(see, hear, feel))) %>%
    mutate(bio_sum = sum(c(body, health, sexual, ingest))) %>%
    mutate(drives_sum = sum(c(affiliation, achieve, power, reward, risk))) %>%
    mutate(relativ_sum = sum(c(motion, space, time))) %>%
    mutate(informal_sum = sum(c(swear, netspeak, assent, nonflu, filler))) %>%
    mutate(affect_sum = sum(c(posemo, negemo))) %>%
    mutate(negemo_sum = sum(c(anx, anger, sad))) %>%
    mutate(pronoun_sum = sum(c(ppron, ipron))) %>%
    mutate(ppronoun_sum = sum(c(i, we, you, shehe, they))) %>%
    mutate(funct_sum = sum(c(pronoun, article, prep, auxverb, adverb, conj, negate))) %>%
    mutate(social_sum = sum(c(family, friend, female, male))) %>%
    mutate(cogproc_sum = sum(c(insight, cause, discrep, tentat, certain, differ)))

  #creating the "other" categories
  LIWC_df <- LIWC_df %>%
    rowwise() %>%
    #other category for negative emotions
    mutate(neg_emo_other = (negemo_sum - negemo)) %>%
    #other category for social
    # mutate(social_other = (social_sum - social)) %>%
    #other category for perceptual processes
    mutate(percept_other = (percept_sum - percept))

  #removing main categories and other categories like netspeak that group wanted to exclude
  LIWC_df <- LIWC_df %>%
    rowwise() %>%
    select(-one_of(c("funct", "pronoun", "ppron","negemo", "social", "cogproc", "percept", "bio", "drives",
                     "relativ", "informal", "netspeak")))

  #removing sum of subcategories of liwc that I had created earlier
  LIWC_df <- LIWC_df %>%
  rowwise() %>%
  select(-one_of(c("percept_sum", "bio_sum", "drives_sum", "relativ_sum", "informal_sum", "affect_sum", "negemo_sum",
                   "pronoun_sum", "ppronoun_sum", "funct_sum", "social_sum", "cogproc_sum")))

  LIWC_df <- LIWC_df %>%
    rowwise() %>%
    mutate(percept_other = if_else(percept_other < 0, 0, percept_other)) %>%
    # mutate(social_other = if_else(social_other < 0, 0, social_other)) %>%
    mutate(neg_emo_other = if_else(neg_emo_other < 0, 0, neg_emo_other))
  return(LIWC_df)
}


##########################################################################
################# Functions for preparing data for factor analysis
##########################################################################
subset_and_center <- function(df,role) {
  df <- df %>%
    filter(Source..C. == role) %>%
    select(-one_of(c("Source..A.","Source..B.","Source..C.")))# %>%
  df  <- df[complete.cases(df),]
  df <- scale(df, center = TRUE, scale = TRUE)
  df <- as.data.frame(df)
  return(df)
}

prep_items_for_FA <- function(df, cutoff){
  # https://stats.stackexchange.com/questions/439653/is-there-a-standard-measure-of-fit-to-validate-exploratory-factor-analysis
  # https://www.rdocumentation.org/packages/REdaS/versions/0.9.3/topics/Kaiser-Meyer-Olkin-Statistics
  
  # calculate Mean Sampling Adequacy (MSAs) which are a measure of how much variance in the item is 'shared'
  item_scores <- REdaS::KMOS(df, use = "pairwise.complete.obs")
  keep_items <- names(item_scores$MSA[which(item_scores$MSA >= cutoff)])
  drop_items <- names(item_scores$MSA[which(item_scores$MSA < cutoff)])
  print(paste('First KMO criterion:',item_scores$KMO))
  print('First Bartlett:')
  print(
    REdaS::bart_spher(df))
  print('Retained variables: ')
  print(
    keep_items)
  print('Dropped variables: ')
  print(
    drop_items
  )
  # Keep only items above MSA cutoff
  df <- df %>%
    select(
      one_of(keep_items)
      )
  # re-run to get updated KMO criterion
  item_scores <- REdaS::KMOS(df, use = "pairwise.complete.obs")
  print('Updated KMO criterion:')
  print(item_scores$KMO)
  print('Updated Bartlett:')
  print(
    REdaS::bart_spher(df))
  return(df)
}

get_mahalanobis_distance <- function(df, auto_drop, re_center) {
  df$mahal <- mahalanobis(df, colMeans(df), cov(df))
  df$p <- pchisq(df$mahal, df = ncol(df) - 1, lower.tail = FALSE)
  print(nrow(df %>% filter(p < .001)))
  if (auto_drop) {
    df <- df %>%
      filter(p >=.001)
    if (re_center) {
      df <- df %>%
        mutate(across(!mahal & !p, ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE))))
    }
  }
  df <- df %>% select(!mahal & !p)
  return(df)
}




################################################




library(tidyverse)
library(xlsx)
library(stringr)
library(here)
library(config)
library(haven)
library(skimr)



maripohsa_survey_data <- read_dta(here(config$maripohsa_survey_data_path, config$maripohsa_survey_data_name))
#ECHO_ID_key <- read_csv(here(config$ECHO_ID_key_path, config$ECHO_ID_key_name))
