############################################################################################################
############# Automating 'liwcing' of a dataframe
############################################################################################################

library(fs)

get_liwc_scores <- function(transcript_df, id_var, text_col, keep_temp) {
  
  # for this to work, LIWC 22 needs to be open and running on your machine
  
  if(missing(keep_temp)) { keep_temp = TRUE}
  
  # save df
  fs::dir_create(here('temp'))
  inputFileCSV <- here('temp','temp_trans.csv')
  outputLocation <- here('temp','temp_liwcd.csv')
  write_csv(transcript_df,inputFileCSV )
  
  ### Defining command line call
  shellType <- 'sh'
  id_var_num <- as.character(which(colnames(transcript_df) == id_var))
  text_col_num <- as.character(which(colnames(transcript_df) == text_col))
        # You may need to change the first part of this. the 'LIWC-22-cli' was working for me in terminal, but not when
        # called through 'system' in R. It's a PATH variable issue. You can either modify the PATH variable in environment,
        # or just point it to this specific location. The location may be different on your machine (but I doubt it). 
        # If you get an error running this saying it can't find LIWC-22-cli command, go to the terminal and run: which LIWC-22-cli
        # cut and paste that address if it is different than what's below. 
  cmd_to_execute <- paste("/Applications/LIWC-22-cli.app/Contents/MacOS/LIWC-22-cli",
                          "-m", "wc",
                          "-id", id_var_num,
                          "-ci", text_col_num,
                          "-ccol no",
                          "-i", shQuote(inputFileCSV, type=shellType),
                          "-o", shQuote(outputLocation, type=shellType),
                          sep = ' ')
  cat(cmd_to_execute) # prints out what command string will look like
  ### Execute command line call
  base::system(command=cmd_to_execute,
        # shell=shellType,
        intern=FALSE,
        wait=TRUE#,
        # mustWork=TRUE
        )
  ### Read in liwc processed file
  liwcd_df <- read_csv(outputLocation) %>%
    select(-Segment) %>%
    plyr::rename(c('Row ID'=id_var))
  
  print(paste('liwcd_df... ',colnames(liwcd_df)))
  print(paste('transcript_df... ',colnames(transcript_df)))
  
  cmb_df <- transcript_df %>%
    full_join(liwcd_df, by = 'overall_sequence')
  
  ### get rid of temp directory if keep_time == FALSE
  if(!keep_temp) {fs::dir_delete(here('temp'))}
  
  return(cmb_df)
}

# Example call
test <- get_liwc_scores(
  transcript_df = test_window,
  id_var = 'overall_sequence',
  text_col = 'text_agg',
  keep_temp = TRUE
)
