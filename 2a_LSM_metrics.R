##########################################################################
##########################################################################
################# LSM measure scripts
##########################################################################
##########################################################################

# To dos:
  # QC: Test whethere there are jsut two speaker roles per file; THis assumes we've already cleaned to just two roles somewhere... 
  # Add Vader scores to dataframe
  # Calculate matching for ALL LSM features
    # Groupby File, mutate_all (not sure about syntax), ungroup
    # This should give you a dataframe by FILE (not speaker) of LSM for each feature
  # Average over the groupings we care about:
    # Starndard LSM: ['ppron','ipron', 'article', 'prep', 'auxverb', 'adverb','negate','conj','quant']
    # mutate LSM = mean( of the LSM features)




library(tidyverse)
library(readxl)
library(stringr)
#import the LIWC output plus text for transcripts
LIWC_df <- read_excel(here(config$liwc_f_V2_path, config$liwc_f_V2_name))



#Renaming first four columns, including the row order of LIWC output, transcript id, role, and transcript_text
LIWC_df <- LIWC_df %>%
  rename(output_order = 'Source (A)') %>%
  rename(id = 'Source (B)') %>%
  rename(role = 'Source (C)') %>%
  rename(transcript_text = 'Source (D)')


LIWC_df <- LIWC_df %>%
  #deleting all punctuation
  select(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark",
                   "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP")))

#removing files that included a second doctor ( ) or family members () or two patients ()
LIWC_df <- LIWC_df %>%
  filter(id != "12103301") %>%
  filter(id != "16180201") %>%
  filter(id != "20180501") %>%
  filter(id != "36159801") %>%
  filter(id != "17102301") %>%
  filter(id != "17129904 and 17206401") %>%
  filter(id != "37158501") %>%
  filter(role != "other")

#Calculating LSM metrics
LIWC_df_LSM <- LIWC_df%>%
  select(-output_order) %>% 
  select(-transcript_text) %>%
  pivot_longer(WC:filler) %>%
  pivot_wider(names_from = role, values_from = value) %>%
  mutate(LIWC_new = (1 - abs(doctor - patient) / (doctor + patient + .0001))) %>%
  select(-doctor, -patient) %>%
  pivot_wider(names_from = name, names_prefix = "LSM_", values_from = LIWC_new)


#creating overall LSM metric for functions
#creating overall LSM metric for functions by getting average of auxiliary verbs, 
#articles, common adverbs, personal pronouns, indefinite pronouns, prepositions, 
#negations, conjunctions, quantifiers (missing LSM_ppronoun for now)
LIWC_df_LSM<- LIWC_df_LSM %>%
  rowwise() %>%
  mutate(LSM_function_mean = mean(c(LSM_auxverb, LSM_article, LSM_adverb, LSM_ipron, LSM_prep, LSM_negate, LSM_conj, LSM_quant, LSM_ppron)))

#adding variable to show location of patient visits
LIWC_df_LSM <- LIWC_df_LSM %>% 
  mutate(location = if_else(str_detect(id, "^8"), "portland", "baltimore"))

#adding variable for doctor id 
LIWC_df_LSM <- LIWC_df_LSM %>% 
  mutate(doctor_id = str_sub(id, start = 1, end = 2))

#adding variable for patient id 
LIWC_df_LSM <- LIWC_df_LSM %>% 
  mutate(patient_id = str_sub(id, start = 3, end = 6))

#adding variable for patient visit number
LIWC_df_LSM <- LIWC_df_LSM %>% 
  mutate(patient_visit_number = str_sub(id, start = 7, end = 8))



##########################################################################################################
##########################################################################################################
#VADER CALCULATION
##########################################################################################################
##########################################################################################################

library(vader)

#import the LIWC output plus text for transcripts
LIWC_df_vader <- read_excel(here(config$liwc_f_V2_path, config$liwc_f_V2_name))

LIWC_df_vader  <- LIWC_df_vader %>%
  rename(output_order = 'Source (A)') %>%
  rename(id = 'Source (B)') %>%
  rename(role = 'Source (C)') %>%
  rename(transcript_text = 'Source (D)')

#removing files that included a second doctor ( ) or family members () or two patients ()
LIWC_df_vader <- LIWC_df_vader  %>%
  filter(id != "12103301") %>%
  filter(id != "16180201") %>%
  filter(id != "20180501") %>%
  filter(id != "36159801") %>%
  filter(id != "17102301") %>%
  filter(id != "17129904 and 17206401") %>%
  filter(id != "37158501") %>%
  filter(role != "other")

vader_results <- vader_df(LIWC_df_vader$transcript_text, incl_nt = T, neu_set = T, rm_qm = T)

