library(tidyverse)
library(xlsx)
library(stringr)
library(here)
library(config)
library(haven)
library(skimr)
library(vader)


Sys.setenv(R_CONFIG_ACTIVE = "salar") # 'default')#
config <- config::get()

ECHO_Transcripts_Complete_TbyT <- read_csv(here(config$ECHO_Transcript_path, config$ECHO_Transcript_name))

#deleting first column of data
ECHO_Transcripts_Complete_TbyT <- select(ECHO_Transcripts_Complete_TbyT, -c(...1))

#removing all brackets with content inside of them
ECHO_Transcripts_Complete_TbyT$Text <- gsub("\\[(.*?)\\]", "", ECHO_Transcripts_Complete_TbyT$Text)

#removing all punctuations
ECHO_Transcripts_Complete_TbyT$Text <- str_replace_all(ECHO_Transcripts_Complete_TbyT$Text, "[[:punct:]]", "")

#unique(ECHO_Transcripts_Complete_TbyT$File)


#removing rows with: "comment", "RA", "Missing_1", "Missing_2:" for Speaker column...Check with Annie what "Missing" means
ECHO_Transcripts_Complete_TbyT <- ECHO_Transcripts_Complete_TbyT %>%
  filter(Speaker != "Comment") %>%
  filter(Speaker != "RA") %>%
  filter(Speaker != "Missing_1") %>%
  filter(Speaker != "Missing_2") %>%
  filter(Speaker != "D?") %>%
  filter(Speaker != "M")


# ECHO_proportions <- ECHO_Transcripts_Complete_TbyT %>% 
#   group_by(File) %>%
#   summarise(D = sum(Speaker == "D"), 
#             P = sum(Speaker == "P"), 
#             D1 = sum(Speaker == "D1"),
#             UF = sum(Speaker == "UF"),
#             UM = sum(Speaker == "UM"),
#             "D?" = sum(Speaker == "D?"),
#             PA = sum(Speaker == "PA"),
#             P2 = sum(Speaker == "P2"),
#             D2 = sum(Speaker == "D2"), 
#             GD = sum(Speaker == "GD"), 
#             C = sum(Speaker == "C"), 
#             N = sum(Speaker == "N"), 
#             NP = sum(Speaker == "NP"), 
#             PM = sum(Speaker == "PM"), 
#             PF = sum(Speaker == "PF"), 
#             DM = sum(Speaker == "DM"), 
#             RN = sum(Speaker == "RN"), 
#             Q = sum(Speaker == "Q"), 
#             UM1 = sum(Speaker == "UM1"), 
#             UM2 = sum(Speaker == "UM2"))
# 
# 
# #write.csv(ECHO_proportions, "ECHO_proportions.csv")     
# 
# ECHO_proportions <- ECHO_proportions %>%
#   rowwise() %>%
#   mutate(doctor_patient = sum(c(D, P))) 
# 
# 
# ECHO_proportions <- ECHO_proportions %>%
#   rowwise() %>% 
#   mutate(all_speakers = sum(c_across(D:UM2), na.rm = T))
# 
# ECHO_proportions <- ECHO_proportions %>%
#   rowwise()%>%
#   mutate(doctor_patient_proportion = doctor_patient/all_speakers) 
# 
# #filtering only transcripts with more than 20% non-patient/doctor speech
# ECHO_transcript_exclude <- ECHO_proportions %>%
#   filter(doctor_patient_proportion < 0.8)

#Transcripts with less than 80% Doctor-Patient speech
ECHO_transcript_exclude_remove <- c("JC04P03", "JC05P03", "JC09P06", "JC10P01", "JC11P01", "JC11P04", "JC11P07", 
                                    "OC03P07", "OC04P06", "OC06P09", "SC12P051", "SC18P094")


#filtering out the list of transcripts in "excluded_transcript" df
ECHO_Transcripts_Complete_TbyT <- ECHO_Transcripts_Complete_TbyT %>% 
  filter(!(File %in% ECHO_transcript_exclude_remove))


#ECHO_Transcripts_Complete_TbyT$Speaker <- gsub("[[:space:]]", "", ECHO_Transcripts_Complete_TbyT$Speaker)
#ECHO_Transcripts_Complete_TbyT$Speaker <- str_trim(ECHO_Transcripts_Complete_TbyT$Speaker, "both")


ECHO_Speaker_exclude <- c("D1", "UF", "UM", "PA", "P2", "D2", "GD", 
                       "C", "N", "NP", "PM", "PF", "DM", "RN", 
                       "Q", "UM1", "UM2")


ECHO_Transcripts_Complete_TbyT <- ECHO_Transcripts_Complete_TbyT %>%
  filter(!(Speaker %in% ECHO_Speaker_exclude))

#need to create word count variable to remove rows without text
ECHO_Transcripts_Complete_TbyT$Word_count <- str_count(ECHO_Transcripts_Complete_TbyT$Text, "\\w+")

#ready to be used for LIWC analysis for TbyT after this line
ECHO_Transcripts_Complete_TbyT <- ECHO_Transcripts_Complete_TbyT %>%
  filter(Word_count != 0)

#making a version of the TbyT where one-word and two-word turns are deleted before smoothing/LIWC
ECHO_Transcripts_Complete_TbyT_V2 <- ECHO_Transcripts_Complete_TbyT %>%
  filter(Word_count != 1) %>%
  filter(Word_count != 2)
############################################################################################################
############# Speaker Smoothing
############################################################################################################

smoothed_tByT_df <- data.frame( # Empty dataframe to store results
  File = character(),
  Speaker = character(),
  Text = character(),
  Sequence = numeric(),
  Word_count = integer()
)

for (f in unique(ECHO_Transcripts_Complete_TbyT_V2$File)) { # Iterate through each file
  file_df <- data.frame( # create a df with just that files data; probably ineffecient, but...
    ECHO_Transcripts_Complete_TbyT_V2[which(ECHO_Transcripts_Complete_TbyT_V2$File == f),]
  )
  first <- TRUE
  smoothed_chunk <- data.frame( # Create empty dataframe to store smoothed speakers for that file
    File = character(),
    Speaker = character(),
    Text = character(),
    Sequence = numeric(),
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
  smoothed_tByT_df <- rbind(smoothed_tByT_df,smoothed_chunk) # add the smoothed file to overall smoothed results
}
write.csv(smoothed_tByT_df, "ECHO_Transcripts_Complete_TbyT_SMOOTHED_V2.csv")

#write.xlsx(ECHO_Transcripts_Complete_TbyT, file = "ECHO_Transcripts_Complete_TbyT.xlsx")
write.csv(ECHO_Transcripts_Complete_TbyT, "ECHO_Transcripts_Complete_TbyT.csv")


#conversation level prep
ECHO_LSM_Prep <- ECHO_Transcripts_Complete_TbyT %>% 
  group_by(File, Speaker) %>%
  summarise(Text = paste(Text, collapse = " ")) 


write.csv(ECHO_LSM_Prep, "ECHO_LSM_Prep.csv")



##################################################################################################
##################################################################################################
#Calculating LSM, adding back LIWC metrics, merging survey data
##################################################################################################
##################################################################################################

#Opening all files
ECHO_LSM_MLM <- read_csv(here(config$ECHO_LSM_MLM_path, config$ECHO_LSM_MLM_name))
ECHO_survey_data <- read_dta(here(config$ECHO_survey_data_path, config$ECHO_survey_data_name))
ECHO_ID_key <- read_csv(here(config$ECHO_ID_key_path, config$ECHO_ID_key_name))


#Here is the code for creating the LIWC values for patient and doctor as individual variables
ECHO_LSM_LIWC_Components <- ECHO_LSM_MLM%>%
  rename(output_order = 'Source (A)') %>%
  rename(File = 'Source (B)') %>%
  rename(Speaker = 'Source (C)') %>%
  rename(Text = 'Source (D)') %>%
  select(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark",
                   "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP"))) %>%
  ungroup()%>%
  select(-Text) %>%
  select(-output_order) %>%
  pivot_longer(WC:filler) %>%
  pivot_wider(names_from = c(name, Speaker), values_from = value) 


#This is for calculating LSM for this data
ECHO_LSM_MLM <- ECHO_LSM_MLM %>%
  rename(output_order = 'Source (A)') %>%
  rename(File = 'Source (B)') %>%
  rename(Speaker = 'Source (C)') %>%
  rename(Text = 'Source (D)') %>%
  #deleting all punctuations
  select(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark",
                   "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP"))) %>%
#renamed function variable to funct as indicated on LIWC manual
rename(funct = "function") %>%
#creating LSM scores
  select(-Text) %>%
  select(-output_order) %>%
  pivot_longer(WC:filler) %>%
  pivot_wider(names_from = Speaker, values_from = value) %>%
  mutate(LIWC_new = (1 - abs(D - P) / (D + P + .0001))) %>%
  select(-D, -P) %>%
  pivot_wider(names_from = name, names_prefix = "LSM_", values_from = LIWC_new) %>%
#creating overall LSM metric for functions by getting average of auxiliary verbs, 
#articles, common adverbs, personal pronouns, indefinite pronouns, prepositions, 
#negations, conjunctions, quantifiers (missing LSM_ppronoun for now)
  rowwise() %>%
  mutate(LSM_function_mean = mean(c(LSM_auxverb, LSM_article, LSM_adverb, LSM_ipron, LSM_prep, 
                                    LSM_negate, LSM_conj, LSM_quant, LSM_ppron))) %>%
  mutate(conv.affect.match = LSM_affect,
         conv.social.match = LSM_social,
         conv.cogproc.match = LSM_cogproc,
         conv.percept.match = LSM_percept,
         conv.negemo.match = LSM_negemo,
         conv.bio.match = LSM_bio,
         conv.drives.match = LSM_drives,
         conv.relativ.match = LSM_relativ,
         conv.informal.match = LSM_informal)
  


#Adding LIWC components back to LSM file
ECHO_LSM_MLM <- left_join(ECHO_LSM_MLM, ECHO_LSM_LIWC_Components, by = "File")
#Making the provider_id variable
ECHO_LSM_MLM <- ECHO_LSM_MLM %>%
  mutate(provider_id = str_sub(File, 1, 4))
#merge ECHO_LSM_MLM and ECHO_ID_key by "File"
ECHO_LSM_MLM <- left_join(ECHO_LSM_MLM, ECHO_ID_key, by = "File" ) 
#merge ECHO_LSM_MLM and ECHO_survey_data by "tapeid" to include survey data
ECHO_LSM_MLM <- left_join(ECHO_LSM_MLM, ECHO_survey_data, by = "tapeid")


############################################################################################################
############################################################################################################
#####Making text chunks based on Word Count and Turns
############################################################################################################
############################################################################################################


#Script for creating the chunks which will be used of studying linguistic accommodation
#chunks created BASED ON WORD COUNT
ECHO_Transcript_chunks_wc <- ECHO_Transcripts_Complete_TbyT %>% 
  group_by(File) %>%
  mutate(word_count = str_count(Text,"\\w+"), 
         cumulative = cumsum(word_count), 
         chunk = case_when(cumulative < (max(cumulative)/3) ~ 1, 
                           cumulative < (max(cumulative/3))*2 ~ 2, 
                           TRUE ~ 3) 
  ) %>%
  ungroup() 

ECHO_Transcript_chunks_wc <- ECHO_Transcript_chunks_wc %>%
  group_by(File, chunk, Speaker) %>%
  summarise(words = paste(Text,collapse = " ")) %>%
  ungroup()

write.csv(ECHO_Transcript_chunks_wc, "ECHO_Transcript_chunks_wc.csv")

#Script for creating the chunks which will be used of studying linguistic accommodation
#chunks created BASED ON TURNS
ECHO_Transcript_chunks_turns <- ECHO_Transcripts_Complete_TbyT %>% 
  mutate(turn = 1) %>%
  group_by(File) %>%
  mutate(cumulative = cumsum(turn),
         chunk = case_when(cumulative < (max(cumulative)/3) ~ 1, 
                           cumulative < (max(cumulative)/3)*2 ~ 2, 
                           TRUE ~ 3)
  ) %>%
  ungroup() 

ECHO_Transcript_chunks_turns <- ECHO_Transcript_chunks_turns %>%
  group_by(File, chunk, Speaker) %>%
  summarise(words = paste(Text,collapse = " ")) %>%
  ungroup()


write.csv(ECHO_Transcript_chunks_turns, "ECHO_Transcript_chunks_turns.csv")


############################################################################################################
############################################################################################################
#####Adding chunk variable (based on Word Count and Turns) for ECHO TbyT transcript after processing through LIWC
############################################################################################################
############################################################################################################

ECHO_smoothed_chunks <- read_csv(here(config$ECHO_LSM_TbyT_Smoothed_path, config$ECHO_LSM_TbyT_Smoothed_name))

ECHO_smoothed_chunks <-ECHO_smoothed_chunks %>%
  rename(output_order = A) %>%
  rename(File = B) %>%
  rename(Speaker = C) %>%
  rename(Text = D) %>%
  select(-E) %>%
  select(-F)


ECHO_smoothed_chunks_turns <- ECHO_smoothed_chunks %>% 
  mutate(turn = 1) %>%
  group_by(File) %>%
  mutate(cumulative = cumsum(turn),
         chunk = case_when(cumulative < (max(cumulative)/3) ~ 1, 
                           cumulative < (max(cumulative)/3)*2 ~ 2, 
                           TRUE ~ 3)
  ) %>%
  ungroup() %>%
relocate(chunk, .before = Text) %>%
  select(-c(turn, cumulative))




#Script for creating the chunks which will be used of studying linguistic accommodation
#chunks created BASED ON WORD COUNT
ECHO_smoothed_chunks_wc <- ECHO_smoothed_chunks %>% 
  group_by(File) %>%
  mutate(word_count = str_count(Text,"\\w+"), 
         cumulative = cumsum(word_count), 
         chunk = case_when(cumulative < (max(cumulative)/3) ~ 1, 
                           cumulative < (max(cumulative/3))*2 ~ 2, 
                           TRUE ~ 3) 
  ) %>%
  ungroup() %>%
relocate(chunk, .before = Text) %>%
  select(-c(word_count, cumulative))



##################################################################################################
##################################################################################################
#Running LSM on two different transcript chunk versions
#Based on WC & Turns
##################################################################################################
##################################################################################################

#Opening all files
ECHO_LSM_MLM_chunks_wc <- read_csv(here(config$ECHO_LSM_MLM_chunks_wc_path, config$ECHO_LSM_MLM_chunks_wc_name))
ECHO_LSM_MLM_chunks_turns <- read_csv(here(config$ECHO_LSM_MLM_chunks_turns_path, config$ECHO_LSM_MLM_chunks_turns_name))
ECHO_survey_data <- read_dta(here(config$ECHO_survey_data_path, config$ECHO_survey_data_name))
ECHO_ID_key <- read_csv(here(config$ECHO_ID_key_path, config$ECHO_ID_key_name))


#FIRST IS LSM ON CHUNKS BASED ON WORD COUNT (WC)
#Here is the code for creating the LIWC values for patient and doctor as individual variables
ECHO_LSM_LIWC_Components_chunks_wc <- ECHO_LSM_MLM_chunks_wc %>%
  rename(output_order = 'Source (A)') %>%
  rename(File = 'Source (B)') %>%
  rename(Chunk = 'Source (C)') %>%
  rename(Speaker = 'Source (D)') %>%
  rename(Text = 'Source (E)') %>%
  select(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark",
                   "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP"))) %>%
  ungroup()%>%
  select(-Text) %>%
  select(-output_order) %>%
  pivot_longer(WC:filler) %>%
  pivot_wider(names_from = c(name, Speaker), values_from = value)  %>%
  #the next two lines of code adds a separate LIWC variable for each chunk
  pivot_longer(WC_D:filler_P) %>%
  pivot_wider(names_from = c(name, Chunk), values_from = value)


#This is for calculating LSM for this data
ECHO_LSM_MLM_chunks_wc <- ECHO_LSM_MLM_chunks_wc %>%
  rename(output_order = 'Source (A)') %>%
  rename(File = 'Source (B)') %>%
  rename(Chunk = 'Source (C)') %>%
  rename(Speaker = 'Source (D)') %>%
  rename(Text = 'Source (E)') %>%
  #deleting all punctuation categories
  select(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark",
                   "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP"))) %>%

#renamed function variable to funct as indicated on LIWC manual
rename(funct = "function") %>%
#creating LSM values
select(-Text) %>%
select(-output_order) %>%
pivot_longer(WC:filler) %>%
pivot_wider(names_from = Speaker, values_from = value) %>%
mutate(LIWC_new = (1 - abs(D - P) / (D + P + .0001))) %>%
select(-D, -P) %>%
pivot_wider(names_from = name, names_prefix = "LSM_", values_from = LIWC_new) %>%
#creating overall LSM metric for functions by getting average of auxiliary verbs, 
#articles, common adverbs, personal pronouns, indefinite pronouns, prepositions, 
#negations, conjunctions, quantifiers (missing LSM_ppronoun for now)
rowwise() %>%
mutate(LSM_function_mean = mean(c(LSM_auxverb, LSM_article, LSM_adverb, LSM_ipron, LSM_prep, 
                                  LSM_negate, LSM_conj, LSM_quant, LSM_ppron))) %>%
  mutate(conv.affect.match = LSM_affect,
         conv.social.match = LSM_social,
         conv.cogproc.match = LSM_cogproc,
         conv.percept.match = LSM_percept,
         conv.negemo.match = LSM_negemo,
         conv.bio.match = LSM_bio,
         conv.drives.match = LSM_drives,
         conv.relativ.match = LSM_relativ,
         conv.informal.match = LSM_informal) %>%
pivot_longer(LSM_WC:conv.informal.match) %>%
pivot_wider(names_from = c(name, Chunk), values_from = value)


#Adding LIWC components back to LSM file
ECHO_LSM_MLM_chunks_wc <- left_join(ECHO_LSM_MLM_chunks_wc, ECHO_LSM_LIWC_Components_chunks_wc, by = "File")

#Making the provider_id variable
# ECHO_LSM_MLM_chunks_wc <- mutate(ECHO_LSM_MLM_chunks_wc,
#                        provider_id = str_sub(File, 1, 4))

#merge ECHO_LSM_MLM_chunks_wc and ECHO_ID_key by "File"
#ECHO_LSM_MLM_chunks_wc <- left_join(ECHO_LSM_MLM_chunks_wc, ECHO_ID_key, by = "File" )

#merge ECHO_LSM_MLM_chunks_wc and ECHO_survey_data by "tapeid" to include survey data
#ECHO_LSM_MLM_chunks_wc <- left_join(ECHO_LSM_MLM_chunks_wc, ECHO_survey_data, by = "tapeid")

#making chunk ratio to be used in analysis
ECHO_LSM_MLM_chunks_wc <- ECHO_LSM_MLM_chunks_wc %>%
  rowwise() %>%
  mutate(LSM_function_mean_chunkratio = (LSM_function_mean_3/LSM_function_mean_1))

ECHO_LSM_MLM_chunks_wc <- ECHO_LSM_MLM_chunks_wc %>%
  rename_at(vars(-(File)), ~ paste0(., '_wc')) %>%
  mutate(affect_chunkratio_wc = conv.affect.match_3_wc/ conv.affect.match_1_wc,
         social_chunkratio_wc = conv.social.match_3_wc/ conv.social.match_1_wc,
         cogproc_chunkratio_wc = conv.cogproc.match_3_wc/ conv.cogproc.match_1_wc,
         percept_chunkratio_wc = conv.percept.match_3_wc/ conv.percept.match_1_wc,
         negemo_chunkratio_wc = conv.negemo.match_3_wc/ conv.negemo.match_1_wc,
         bio_chunkratio_wc = conv.bio.match_3_wc/ conv.bio.match_1_wc,
         drives_chunkratio_wc = conv.drives.match_3_wc/ conv.drives.match_1_wc,
         relativ_chunkratio_wc = conv.relativ.match_3_wc/ conv.relativ.match_1_wc,
         informal_chunkratio_wc = conv.informal.match_3_wc/ conv.informal.match_1_wc)
######################################################################################################
#THIS SECTION IS LSM ON CHUNKS BASED ON TURNS
#Here is the code for creating the LIWC values for patient and doctor as individual variables
ECHO_LSM_LIWC_Components_chunks_turns <- ECHO_LSM_MLM_chunks_turns %>%
  rename(output_order = 'Source (A)') %>%
  rename(File = 'Source (B)') %>%
  rename(Chunk = 'Source (C)') %>%
  rename(Speaker = 'Source (D)') %>%
  rename(Text = 'Source (E)') %>%
  select(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark",
                   "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP"))) %>%
  ungroup() %>%
  select(-Text) %>%
  select(-output_order) %>%
  pivot_longer(WC:filler) %>%
  pivot_wider(names_from = c(name, Speaker), values_from = value) %>%
  pivot_longer(WC_D:filler_P) %>%
  pivot_wider(names_from = c(name, Chunk), values_from = value)


#This is for calculating LSM for this data
ECHO_LSM_MLM_chunks_turns <- ECHO_LSM_MLM_chunks_turns %>%
  rename(output_order = 'Source (A)') %>%
  rename(File = 'Source (B)') %>%
  rename(Chunk = 'Source (C)') %>%
  rename(Speaker = 'Source (D)') %>%
  rename(Text = 'Source (E)') %>%
  #deleting all punctuation variables
  select(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark",
                   "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP"))) %>%

#renamed function variable to funct as indicated on LIWC manual
rename(funct = "function") %>%
#creating LSM values
select(-Text) %>%
select(-output_order) %>%
pivot_longer(WC:filler) %>%
pivot_wider(names_from = Speaker, values_from = value) %>%
mutate(LIWC_new = (1 - abs(D - P) / (D + P + .0001))) %>%
select(-D, -P) %>%
pivot_wider(names_from = name, names_prefix = "LSM_", values_from = LIWC_new) %>%
  
#creating overall LSM metric for functions by getting average of auxiliary verbs, 
#articles, common adverbs, personal pronouns, indefinite pronouns, prepositions, 
#negations, conjunctions, quantifiers (missing LSM_ppronoun for now)
rowwise() %>%
mutate(LSM_function_mean = mean(c(LSM_auxverb, LSM_article, LSM_adverb, LSM_ipron, LSM_prep, 
                                  LSM_negate, LSM_conj, LSM_quant, LSM_ppron))) %>%
  mutate(conv.affect.match = LSM_affect,
         conv.social.match = LSM_social,
         conv.cogproc.match = LSM_cogproc,
         conv.percept.match = LSM_percept,
         conv.negemo.match = LSM_negemo,
         conv.bio.match = LSM_bio,
         conv.drives.match = LSM_drives,
         conv.relativ.match = LSM_relativ,
         conv.informal.match = LSM_informal) %>%
pivot_longer(LSM_WC:conv.informal.match) %>%
pivot_wider(names_from = c(name, Chunk), values_from = value)


#Adding LIWC components back to LSM file
ECHO_LSM_MLM_chunks_turns <- left_join(ECHO_LSM_MLM_chunks_turns, ECHO_LSM_LIWC_Components_chunks_turns, by = "File")

#Making the provider_id variable
# ECHO_LSM_MLM_chunks_turns <- mutate(ECHO_LSM_MLM_chunks_turns,
#                                  provider_id = str_sub(File, 1, 4))

#merge ECHO_LSM_MLM_chunks_turns and ECHO_ID_key by "File"
#ECHO_LSM_MLM_chunks_turns <- left_join(ECHO_LSM_MLM_chunks_turns, ECHO_ID_key, by = "File" )

#merge ECHO_LSM_MLM_chunks_turns and ECHO_survey_data by "tapeid" to include survey data
#ECHO_LSM_MLM_chunks_turns <- left_join(ECHO_LSM_MLM_chunks_turns, ECHO_survey_data, by = "tapeid")

#making chunk ratio to be used in analysis
ECHO_LSM_MLM_chunks_turns <- ECHO_LSM_MLM_chunks_turns %>%
  rowwise() %>%
  mutate(LSM_function_mean_chunkratio = (LSM_function_mean_3/LSM_function_mean_1))

ECHO_LSM_MLM_chunks_turns <- ECHO_LSM_MLM_chunks_turns %>%
  rename_at(vars(-(File)), ~ paste0(., '_turns')) %>%
  mutate(affect_chunkratio_turns = conv.affect.match_3_turns/ conv.affect.match_1_turns,
         social_chunkratio_turns = conv.social.match_3_turns/ conv.social.match_1_turns,
         cogproc_chunkratio_turns = conv.cogproc.match_3_turns/ conv.cogproc.match_1_turns,
         percept_chunkratio_turns = conv.percept.match_3_turns/ conv.percept.match_1_turns,
         negemo_chunkratio_turns = conv.negemo.match_3_turns/ conv.negemo.match_1_turns,
         bio_chunkratio_turns = conv.bio.match_3_turns/ conv.bio.match_1_turns,
         drives_chunkratio_turns = conv.drives.match_3_turns/ conv.drives.match_1_turns,
         relativ_chunkratio_turns = conv.relativ.match_3_turns/ conv.relativ.match_1_turns,
         informal_chunkratio_turns = conv.informal.match_3_turns/ conv.informal.match_1_turns)

##################################################################
##################################################################
###VADER Analysis
##################################################################
##################################################################

#NOTE: COMMENTED THE VADER SCRIPT OUT SO WE DON'T ACCIDENTALLY RUN IT UNLESS WE WANT UPDATE IT.
# #tbyt transcripts
# smoothed_tByT_VADER_df <- smoothed_tByT_df
# 
# #getting tbyt vader scores
# smoothed_tByT_VADER_scores <- vader_df(smoothed_tByT_VADER_df$Text, incl_nt = T, neu_set = T, rm_qm = T)
# 
# #binding the VADER scores with original df (which has ID and text)
# smoothed_tByT_VADER_complete_df <- bind_cols(smoothed_tByT_VADER_df,smoothed_tByT_VADER_scores)
# 
# write.csv(smoothed_tByT_VADER_complete_df, "smoothed_tByT_VADER_complete_df.csv")
# 
# 
# #conv level transcripts
# ECHO_LSM_Prep_VADER <- ECHO_LSM_Prep 
#   
# #getting conv. level VADER scores --> had ERRORS for some transcript...looking into this..
# ECHO_LSM_Prep_VADER$Text <- vader_df(ECHO_LSM_Prep_VADER$Text, incl_nt = T, neu_set = T, rm_qm = T)

##################################################################
#####Making one large df with all different matching variables
# ****RUN THE SCRIPT BELOW AFTER YOU RUN THE "4_turnbyturrnLSM.R" script
##################################################################



#merging all matching measures into one large df
ECHO_All_Matching_Measures <- list(ECHO_LSM_MLM, ECHO_smoothed_rLSM, ECHO_smoothed_LIWC_matching, 
               ECHO_LSM_MLM_chunks_turns, ECHO_LSM_MLM_chunks_wc, 
               ECHO_smoothed_chunks_turns_rLSM, ECHO_smoothed_chunks_wc_rLSM,
               ECHO_smoothed_chunks_wc_LIWC_matching, ECHO_smoothed_chunks_turns_LIWC_matching,
               ECHO_smoothed_VADER_matching) %>% 
  reduce(full_join, by = "File")


#making dataframe for all variables in the overall matching document
ECHO_All_Matching_Measures_varnames <- as.data.frame(colnames(ECHO_All_Matching_Measures))

write.csv(ECHO_All_Matching_Measures, "ECHO_All_Matching_Measures.csv")
write.csv(ECHO_All_Matching_Measures_varnames, "ECHO_All_Matching_Measures_varnames.csv")