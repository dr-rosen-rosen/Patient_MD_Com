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
Sys.setenv(R_CONFIG_ACTIVE = "default")
config <- config::get()
Sys.setenv(RETICULATE_PYTHON = config$py_version)
reticulate::source_python('1_funcs.py')

test <- get_transcripts(
  dir_path = config$transcript_folder
)