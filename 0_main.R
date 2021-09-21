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

all_transcripts_df <- get_and_clean_all_transcripts(
  all_filenames = list.files(
    path = config$transcript_path, 
    full.names = TRUE,
    pattern = ".docx", 
    recursive = TRUE)
)



conv_LSM_df <- conv_LSM_prep(
  all_transcripts = all_transcripts_df
)
