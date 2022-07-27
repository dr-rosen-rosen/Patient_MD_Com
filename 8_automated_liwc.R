############################################################################################################
############# Automating 'liwcing' of a dataframe
############################################################################################################

library(fs)

get_liwc_scores <- function(transcript_df, id_var, text_col, keep_temp) {
  
  if(missing(keep_temp)) { keep_temp = TRUE}
  
  # save df
  fs::dir_create(here('temp'))
  inputFileCSV <- here('temp','temp_trans.csv')
  outputLocation <- here('temp','temp_liwcd.csv')
  write_csv(transcript_df,inputFileCSV )
  
  ### Defining command line call
  # shell_type <- 'sh'
  # id_var_num <- as.character(which(colnames(transcript_df) == id_var))
  # text_col_num <- as.character(which(colnames(transcript_df) == text_col))
  # cmd_to_execute <- paste("LIWC-22-cli",
  #                         "--mode", "wc",
  #                         "--row-id-indices", id_var_num,
  #                         "--column-indices", text_col_num,
  #                         "--input", shQuote(inputFileCSV, type=shellType),
  #                         "--output", shQuote(outputLocation, type=shellType),
  #                         sep = ' ')
  # 
  ### Execute command line call
  # shell(cmd=cmd_to_execute,
  #       shell=shellType,
  #       intern=FALSE,
  #       wait=TRUE,
  #       mustWork=TRUE)
  # 
  ### Read in liwc processed file
  # liwcd_df <- read_csv(outputLocation)
  
  ### get rid of temp directory if keep_time == FALSE
  if(!keep_temp) {fs::dir_delete(here('temp'))}
  
  # return(liwcd_df)
}

test <- get_liwc_scores(
  transcript_df = test_window,
  id_var = 'overall_sequence',
  text_col = 'text_agg',
  keep_temp = FALSE
)