library(stm)


transcript_df <- conv_LSM_df %>% #<- read.csv(here(config$liwc_f_path,'conv_LSM_df_v2.csv')) %>%
  filter(role %in% c('doctor','patient')) %>%
  drop_na()

# process the data
Processed_transcript_data<-textProcessor(transcript_df$transcript_text, metadata=transcript_df)

# structure the data for usage in further analyses and verify formatting is correct (and no missing values, etc.)
out<-prepDocuments(Processed_transcript_data$documents, Processed_transcript_data$vocab, Processed_transcript_data$meta)
docs<-out$documents
vocab<-out$vocab
meta<-out$meta

#inspect how many words/documents are removed by adjusting threshold
plotRemoved(Processed_transcript_data$documents, lower.thresh=seq(1,200, by=100))

k_range = c(5,10,15,20,25,30,35,40)
kResult <- searchK(out$documents, out$vocab, K=k_range, prevalence=~role,
                   data=meta)
plot(kResult)

stm_30 <- stm(
  documents = out$documents, 
  vocab = out$vocab, 
  K = 30, 
  prevalence = ~role,
  max.em.its = 75,
  data = out$meta,
  init.type = "Spectral")
plot(stm_30, type = 'summary')
plot(stm_30, type = 'labels')
stm_100 <- stm(
  documents = out$documents, 
  vocab = out$vocab, 
  K = 100, 
  prevalence = ~role,
  max.em.its = 75,
  data = out$meta,
  init.type = "Spectral")
plot(stm_100, type = 'summary')
plot(stm_100, type = 'labels')
stm_5 <- stm(
  documents = out$documents, 
  vocab = out$vocab, 
  K = 5, 
  prevalence = ~role,
  max.em.its = 75,
  data = out$meta,
  init.type = "Spectral")
plot(stm_5, type = 'summary')
plot(stm_5, type = 'labels')

