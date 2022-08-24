############################################################################################################
############# Growth modeling for ECHO data
############################################################################################################

library(tidyverse)
library(config)
library(here)
library(lcmm)
Sys.setenv(R_CONFIG_ACTIVE = "mike") # 'default')#
config <- config::get()

gm_df <- read.csv(here(config$ECHO_rolling_window_8_LIWC_path,config$ECHO_rolling_window_8_LIWC_name))
