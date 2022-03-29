library(tidyverse)
library(xlsx)
library(stringr)
library(here)
library(config)

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
  filter(Speaker != "Missing_2")


ECHO_proportions <- ECHO_Transcripts_Complete_TbyT %>% 
  group_by(File) %>%
  summarise(D = sum(Speaker == "D"), 
            P = sum(Speaker == "P"), 
            D1 = sum(Speaker == "D1"),
            UF = sum(Speaker == "UF"),
            UM = sum(Speaker == "UM"),
            "D?" = sum(Speaker == "D?"),
            PA = sum(Speaker == "PA"),
            P2 = sum(Speaker == "P2"),
            D2 = sum(Speaker == "D2"), 
            GD = sum(Speaker == "GD"), 
            C = sum(Speaker == "C"), 
            N = sum(Speaker == "N"), 
            NP = sum(Speaker == "NP"), 
            PM = sum(Speaker == "PM"), 
            PF = sum(Speaker == "PF"), 
            DM = sum(Speaker == "DM"), 
            RN = sum(Speaker == "RN"), 
            Q = sum(Speaker == "Q"), 
            UM1 = sum(Speaker == "UM1"), 
            UM2 = sum(Speaker == "UM2"))


#write.csv(ECHO_proportions, "ECHO_proportions.csv")     

ECHO_proportions <- ECHO_proportions %>%
  rowwise() %>%
  mutate(doctor_patient = sum(c(D, P))) 


ECHO_proportions <- ECHO_proportions %>%
  rowwise() %>% 
  mutate(all_speakers = sum(c_across(D:UM2), na.rm = T))

ECHO_proportions <- ECHO_proportions %>%
  rowwise()%>%
  mutate(doctor_patient_proportion = doctor_patient/all_speakers) 

#filtering only transcripts with more than 20% non-patient/doctor speech
ECHO_transcript_exclude <- ECHO_proportions %>%
  filter(doctor_patient_proportion < 0.8)

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


ECHO_Transcripts_Complete_TbyT$Word_count <- str_count(ECHO_Transcripts_Complete_TbyT$Text, "\\w+")

ECHO_Transcripts_Complete_TbyT <- ECHO_Transcripts_Complete_TbyT %>%
  filter(Word_count != 0)


#write.xlsx(ECHO_Transcripts_Complete_TbyT, file = "ECHO_Transcripts_Complete_TbyT.xlsx")