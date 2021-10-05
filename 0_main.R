##########################################################################
##########################################################################
################# Main File for Patient MD Communication Analysis
##########################################################################
##########################################################################

library(here)
library(config)
library(reticulate)
library(lubridate)
library(tidyverse)
library(patchwork)
debuggingState(on=FALSE)
# start ve with: source python3/bin/activate in project folder
Sys.setenv(R_CONFIG_ACTIVE = "mike") #salar
config <- config::get()
#Sys.setenv(RETICULATE_PYTHON = config$py_version)
#reticulate::source_python('1_funcs.py')
source('1_funcs.R')

# There are now two ways to recode speakers. The first is Salar's method. To use this,
# send the path to Annie's recoding file; if you set that to NA, it will skip the reccoding
all_transcripts_df_no_recoding <- get_and_clean_all_transcripts(
  all_filenames = list.files(
    path = config$transcript_path, 
    full.names = TRUE,
    pattern = ".docx", 
    recursive = TRUE),
  annies_role_file = NA#config$annies_role_file
)

# Second way of recoding speakers using deframe and recode
recoded_df <- recode_spkrs(
  df = all_transcripts_df_no_recoding,
  role_f = config$annies_role_file
) %>% mutate(role = tolower(role)) %>% filter(role != "***")

recoded_df <- run_exclusions(recoded_df)


conv_LSM_df <- conv_LSM_prep(
  all_transcripts = all_transcripts_df
)
