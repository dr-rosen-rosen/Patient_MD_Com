library(tidyverse)
library(officer)
library(here)

# fDir <- '/Users/mrosen44/Johns Hopkins/Mary Catherine Beach - ECHO3/ECHO3 transcripts'
fDir <- 'ECHO3/transcripts'
fNames <- list.files(path = fDir, pattern = '*.docx')

######################
######################
# for ECHO3 transcripts:
######################
######################
transcript_df <- data.frame(
  file_id = character(),
  speaker = character(),
  speech = character(),
  text = character(),
  text_ok = logical(),
  text_pieces = integer(),
  text_remainder = character()
)

# fNames <-'00342030.docx'
# fNames <- fNames[fNames != '00103060.docx']

for (f in fNames) {
  t <- docxtractr::read_docx(here(fDir,f))
  tbl_no <- docxtractr::docx_tbl_count(t)
  print(paste0(f,'; number of tables: ',tbl_no))
  # devtools::reload(pkgload::inst("officer"))
  if (tbl_no == 0) {
    print(f)
    print(file.exists(here(fDir,f)))
    df <- officer::read_docx(here(fDir,f)) %>%
      officer::docx_summary() %>%
      mutate(
        text = str_replace(text, "\\s*\\([^\\)]+\\)", ""), #remove everything in parentheses
        text = str_replace(text, "\\[[^\\]]*\\]", "") #remove everything in brackets (?)
      ) %>%
      filter(text != '') %>%
      select(text) %>%
      separate_wider_delim(text, ":",
                           names = c("speaker", "speech"),
                           too_many = "merge",
                           too_few = "debug")
    df['file_id'] <- substr(f,1,8)
    df$sequence <- seq(1:nrow(df))
    transcript_df <- bind_rows(transcript_df,df)
    # write.csv(df,here(fDir,str_replace(f,'.docx','.csv')))
    print('no talbes')
    # rm(df)
  } else {
    print(paste('Has tables:',f))
  }
}

target_roles <- c('doctor','patient')

clean_transcript_df <- transcript_df %>%
  mutate(
    text = stringr::str_squish(text),
    speaker = stringr::str_squish(speaker),
    speaker = tolower(speaker),
    speech = stringr::str_squish(speech),
    speaker = case_match(speaker,
                         'doctor 1' ~ 'doctor',
                         'interviewer'~ 'doctor',
                         'interviewee' ~ 'patient', 
                         .default = speaker)) %>% # remove any trailing and leading whitespace; unfortunatley does not help for 
  filter(
    # speaker != '[End of Audio]',
    text != '' | !plyr::empty(text),
    !grepl("^\\[.*\\]$",text)) %>% # this is for any row where text starts with [ and ends with 
  select(file_id,speaker,speech,sequence) %>%
  mutate(
    spkr_recode = case_match(speaker,
                             'doctor' ~ 'doctor',
                             'patient' ~ 'patient',
                             .default = 'other'
    )
  )
clean_transcript_df$word_count <- stringr::str_count(unlist(clean_transcript_df[,'speech']), "\\w+")
write.csv(clean_transcript_df,'final_raw_ECHO3.csv')

#   mutate(
#     to_drop = if_else( 
#       (!(speaker %in% target_roles) | # drops all non-target roles
#          #drops all target roles surrounded by (likely speaking to) non-target roles
#         ((speaker %in% target_roles) & 
#         !(lag(speaker, 1) %in% target_roles) & 
#         !(lead(speaker,1) %in% target_roles))),
#     1,0)
#   )
# prop_dropped_df <- clean_transcript_df %>% group_by(file_id) %>%
#   summarise(prop_dropped = sum(to_drop) / n()) %>%
#   ungroup() %>%
#   filter(prop_dropped < .2)

to_keep <- unique(prop_dropped_df$file_id)
final_echo3 <- clean_transcript_df %>%
  filter(
    file_id %in% to_keep, # drops all transcripts w/ more than 20% dropped lines
    to_drop == 0 # drops all flagged turns
    ) #%>%
  # group_by(file_id, speaker) %>%
  # summarise(speech = paste(speech, collapse = " ")) %>%
  # ungroup()
write.csv(final_echo3, 'final_echo3_rawTbyT.csv')
write.csv(final_echo3, 'final_echo3_conversational.csv')
# clean_transcript_df <- clean_transcript_df %>% filter(!(speaker %in% c(
#   'Doctor','Patient','Child','Male','Female','Patient 2','Female 1','Female 2','Male 1','Male 2',
#   'Doctor 1','Doctor 2','Interviewer','Interviewee','Pharmacist','Operator','Nurse'
#   )))



table(clean_transcript_df$speaker) %>% write.csv('ECHO3_speaker.csv')
table(clean_transcript_df$speaker,clean_transcript_df$file_id) %>% write.csv('ECHO3_speakerByFile.csv')
# view(table(clean_transcript_df$file_id))
# view(clean_transcript_df[which(clean_transcript_df$file_id == "00302090"),])


