############################################################################################################
############# Transcript 'windowing'... creates a 'chunked' transcript combining speach over moving window
############################################################################################################

library(runner)

window_transcripts <- function(transcript_df, window, collapse){
  
  if(missing(collapse)) {collapse <- ' '}
  
  # do some quality control
  if(length(unique(transcript_df$Speaker) )!= 2) {
    print('Too many different speakers...')
  } else {print('Good to go: only 2 speakers')}
  
  windowed_df <- transcript_df %>%
    group_by(File, Speaker) %>%
    mutate(
      text_agg = runner::runner(
        Text,
        f = paste,
        collapse = collapse,
        k = window,
        na_pad = TRUE
      ),
      text_agg_wc = str_count(text_agg, "\\w+")) %>% 
    ungroup()
  
  return(windowed_df)
}

test_window <- window_transcripts(
  transcript_df = ECHO_Transcripts_Total_final,
  window = 5)

skimr::skim(test_window)
