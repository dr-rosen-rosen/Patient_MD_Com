#############
#### ECHO Scripts for data cleaning

library(tidyverse)
library(here)
library(config)
library(gdata)

Sys.setenv(R_CONFIG_ACTIVE = "salar") # 'default')#
config <- config::get()

#open files
ECHO_All_Matching <- read_csv(here(config$ECHO_All_Matching_Measures_path, config$ECHO_All_Matching_Measures_name))

#adding a function to apply scale() to all variables in a df
add_scaled <- function(data, vars = colnames(data), ...) {
  data.frame(data,
             setNames(data.frame(scale(data[, vars, drop = FALSE],
                                       ...)),
                      paste(vars, "scaled", sep = ".")))
}

#making a df for all nonspeech variables
ECHO_All_Matching_Nonspeech <- ECHO_All_Matching %>%
  select(c(2, "provider_id":"disclosedquest"))

#making a df for all speech variables
ECHO_All_Matching_speech <- ECHO_All_Matching %>%
  select(-(1:2)) %>%
  select(-("provider_id":"disclosedquest"))


ECHO_All_Matching_speech <-add_scaled(ECHO_All_Matching_speech)

ECHO_All_Matching <- bind_cols(ECHO_All_Matching_Nonspeech, ECHO_All_Matching_speech)


#making a race concordance variable "raceconc" between patient and provider race
ECHO_All_Matching <- ECHO_All_Matching %>%
  rowwise() %>%
  mutate(raceconc = if_else(racecat2== provrace, 1, 0)) %>%
  ungroup()
#running the code below resulted in "NaN" in all cells...tried fixing it but was unsuccessful
#mutate(LSM_function_mean_scaled = scale(LSM_function_mean, center = TRUE, scale = TRUE))


#adding LIWC outputs for function words for the doctors
ECHO_All_Matching <- ECHO_All_Matching %>%
  rowwise() %>%
  mutate(function_sum_doctors = sum(auxverb_D, article_D, adverb_D, ipron_D, 
                                    prep_D, negate_D, conj_D, quant_D, ppron_D)) %>%
  ungroup()


#getting sd of function_sum_doctors for all patient visits for individual doctors (style of doctors)
ECHO_All_Matching <- ECHO_All_Matching %>%
  group_by(provider_id) %>%
  mutate(
    provider_style_sd = sd(function_sum_doctors, na.rm = TRUE),
    provider_rLSM_sd = sd(rLSM.P, na.rm = TRUE)) %>%
  ungroup()

#Making a subscale score for the 4 cultural dissimilarity items (cdspeak, cdreason, cdstyle, cdrace)
ECHO_All_Matching <- ECHO_All_Matching %>%
  rowwise() %>%
  mutate(cd_subscale = mean(c(cdspeak, cdreason, cdstyle, cdrace))) %>%
  ungroup()



# drop physicians with < n cases
ECHO_All_Matching <- ECHO_All_Matching %>%
  group_by(provider_id) %>%
  filter(n() > 7) %>%
  ungroup() %>%
  gdata::drop.levels(.)

#count_provider_id <- ECHO_All_Matching %>% count(provider_id)

ECHO_All_Matching <- ECHO_All_Matching %>%
  mutate(provider_id = factor(provider_id))

#Making the site_name variable: only three sites in data set: JC, OC, SC
ECHO_All_Matching <- ECHO_All_Matching %>%
  mutate(site_name = str_sub(provider_id, 1, 2))

#creating site_id variable by converting JC==0, OC==1, SC==2
ECHO_All_Matching <- ECHO_All_Matching %>%
  mutate(site_id = ifelse(site_name == "JC", 0,
                   ifelse(site_name == "OC", 1,
                          2)))

#changing site_id to factor
ECHO_All_Matching <- ECHO_All_Matching %>%
  mutate(site_id = factor(site_id))


ECHO_common_variables = c('provider_id', 'site_id')
ECHO_raceconc_variables = c('raceconc')
ECHO_demographic_variables = c('racecat2', 'age', 'gender', 'working', 'marital', 'hsdegree')

ECHO_conv_LSM_variables = c('LSM_function_mean')

ECHO_conv_LSM_variables_scaled = c('LSM_function_mean.scaled')

ECHO_tbyt_rLSM_variables = c('rLSM.P', 'rLSM.D', 'WC_sum.D', 'WC_sum.P', 'mean.rLSM', 'ratio.rLSM', 'verb_dom')

ECHO_tbyt_rLSM_variables_scaled = c('rLSM.P.scaled', 'rLSM.D.scaled', 'WC_sum.D.scaled', 'WC_sum.P.scaled', 'mean.rLSM.scaled', 'ratio.rLSM.scaled', 'verb_dom.scaled')

ECHO_conv_LIWC_Matching_variables = c('conv.affect.match', 'conv.social.match', 'conv.cogproc.match',
                                      'conv.percept.match', 'conv.negemo.match', 'conv.bio.match',
                                      'conv.drives.match', 'conv.relativ.match', 'conv.informal.match')

ECHO_conv_LIWC_Matching_variables_scaled = c('conv.affect.match.scaled', 'conv.social.match.scaled', 'conv.cogproc.match.scaled',
                                             'conv.percept.match.scaled', 'conv.negemo.match.scaled', 'conv.bio.match.scaled',
                                             'conv.drives.match.scaled', 'conv.relativ.match.scaled', 'conv.informal.match.scaled')

ECHO_tbyt_LIWC_Matching_variables = c('affect.tbytmatch.D', 'affect.tbytmatch.P', 'social.tbytmatch.D', 'social.tbytmatch.P',
                                 'cogproc.tbytmatch.D', 'cogproc.tbytmatch.P', 'percept.tbytmatch.D', 'percept.tbytmatch.P',
                                 'negemo.tbytmatch.D', 'negemo.tbytmatch.P', 'bio.tbytmatch.D', 'bio.tbytmatch.P', 
                                 'drives.tbytmatch.D', 'drives.tbytmatch.P', 'relativ.tbytmatch.D', 'relativ.tbytmatch.P', 
                                 'informal.tbytmatch.D', 'informal.tbytmatch.P')

ECHO_tbyt_LIWC_Matching_variables_scaled = c('affect.tbytmatch.D.scaled', 'affect.tbytmatch.P.scaled', 'social.tbytmatch.D.scaled', 'social.tbytmatch.P.scaled',
                                             'cogproc.tbytmatch.D.scaled', 'cogproc.tbytmatch.P.scaled', 'percept.tbytmatch.D.scaled', 'percept.tbytmatch.P.scaled',
                                             'negemo.tbytmatch.D.scaled', 'negemo.tbytmatch.P.scaled', 'bio.tbytmatch.D.scaled', 'bio.tbytmatch.P.scaled', 
                                             'drives.tbytmatch.D.scaled', 'drives.tbytmatch.P.scaled', 'relativ.tbytmatch.D.scaled', 'relativ.tbytmatch.P.scaled', 
                                             'informal.tbytmatch.D.scaled', 'informal.tbytmatch.P.scaled')

ECHO_conv_LSM_chunks_turns_variables = c('LSM_function_mean_1_turns', 'LSM_function_mean_2_turns', 'LSM_function_mean_3_turns', 'LSM_function_mean_chunkratio_turns')

ECHO_conv_LSM_chunks_turns_variables_scaled = c('LSM_function_mean_1_turns.scaled', 'LSM_function_mean_2_turns.scaled', 'LSM_function_mean_3_turns.scaled', 'LSM_function_mean_chunkratio_turns.scaled')

# Not using chunks made by WC for now
# ECHO_conv_LSM_chunks_wc_variables = c('LSM_function_mean_1_wc', 'LSM_function_mean_2_wc', 'LSM_function_mean_3_wc', 'LSM_function_mean_chunkratio_wc')

ECHO_conv_LIWC_Matching_turns_variables = c('affect_chunkratio_turns',
                                            'social_chunkratio_turns',
                                            'cogproc_chunkratio_turns',
                                            'percept_chunkratio_turns',
                                            'negemo_chunkratio_turns',
                                            'bio_chunkratio_turns',
                                            'drives_chunkratio_turns',
                                           'relativ_chunkratio_turns',
                                            'informal_chunkratio_turns')

ECHO_conv_LIWC_Matching_turns_variables_scaled = c('affect_chunkratio_turns.scaled',
                                                        'social_chunkratio_turns.scaled',
                                                        'cogproc_chunkratio_turns.scaled',
                                                        'percept_chunkratio_turns.scaled',
                                                        'negemo_chunkratio_turns.scaled',
                                                        'bio_chunkratio_turns.scaled',
                                                        'drives_chunkratio_turns.scaled',
                                                        'relativ_chunkratio_turns.scaled',
                                                        'informal_chunkratio_turns.scaled')


# Not using chunks made by WC for now
# ECHO_conv_LIWC_Matching_wc_variables = c('affect_chunkratio_wc',
                                            # 'social_chunkratio_wc',
                                            # 'cogproc_chunkratio_wc',
                                            # 'percept_chunkratio_wc',
                                            # 'negemo_chunkratio_wc',
                                            # 'bio_chunkratio_wc',
                                            # 'drives_chunkratio_wc',
                                            # 'relativ_chunkratio_wc',
                                            # 'informal_chunkratio_wc')

ECHO_tbyt_rLSM_chunks_turns_variables = c('rLSM.D.1_turns', 'rLSM.D.2_turns', 'rLSM.D.3_turns', 'rLSM.P.1_turns', 'rLSM.P.2_turns',
                                          'rLSM.P.3_turns', 'mean.rLSM.1_turns', 'ratio.rLSM.1_turns', 'verb_dom.1_turns', 'mean.rLSM.2_turns',
                                          'ratio.rLSM.2_turns', 'verb_dom.2_turns', 'mean.rLSM.3_turns', 'ratio.rLSM.3_turns',
                                          'verb_dom.3_turns', 'rLSM_Chunk_Ratio.D_turns', 'rLSM_Chunk_Ratio.P_turns')

ECHO_tbyt_rLSM_chunks_turns_variables_scaled = c('rLSM.D.1_turns.scaled', 'rLSM.D.2_turns.scaled', 'rLSM.D.3_turns.scaled', 'rLSM.P.1_turns.scaled', 'rLSM.P.2_turns.scaled',
                                                 'rLSM.P.3_turns.scaled', 'mean.rLSM.1_turns.scaled', 'ratio.rLSM.1_turns.scaled', 'verb_dom.1_turns.scaled', 'mean.rLSM.2_turns.scaled',
                                                 'ratio.rLSM.2_turns.scaled', 'verb_dom.2_turns.scaled', 'mean.rLSM.3_turns.scaled', 'ratio.rLSM.3_turns.scaled',
                                                 'verb_dom.3_turns.scaled', 'rLSM_Chunk_Ratio.D_turns.scaled', 'rLSM_Chunk_Ratio.P_turns.scaled')

# # Not using chunks made by WC for now
# ECHO_tbyt_rLSM_chunks_wc_variables = c('rLSM.D.1_wc', 'rLSM.D.2_wc', 'rLSM.D.3_wc', 'rLSM.P.1_wc', 'rLSM.P.2_wc',
#                                        'rLSM.P.3_wc', 'mean.rLSM.1_wc', 'ratio.rLSM.1_wc', 'verb_dom.1_wc', 'mean.rLSM.2_wc',
#                                        'ratio.rLSM.2_wc', 'verb_dom.2_wc', 'mean.rLSM.3_wc', 'ratio.rLSM.3_wc',
#                                        'verb_dom.3_wc', 'rLSM_Chunk_Ratio.D_wc', 'rLSM_Chunk_Ratio.P_wc')
ECHO_tbyt_LIWC_matching_chunks_turns_variables = c('affect.tbytmatch.chunkratio.D.turns',
                                                    'affect.tbytmatch.chunkratio.P.turns',
                                                    'social.tbytmatch.chunkratio.D.turns',
                                                    'social.tbytmatch.chunkratio.P.turns',
                                                    'cogproc.tbytmatch.chunkratio.D.turns',
                                                    'cogproc.tbytmatch.chunkratio.P.turns',
                                                    'percept.tbytmatch.chunkratio.D.turns',
                                                    'percept.tbytmatch.chunkratio.P.turns',
                                                    'negemo.tbytmatch.chunkratio.D.turns',
                                                    'negemo.tbytmatch.chunkratio.P.turns',
                                                    'bio.tbytmatch.chunkratio.D.turns',
                                                    'bio.tbytmatch.chunkratio.P.turns',
                                                    'drives.tbytmatch.chunkratio.D.turns',
                                                    'drives.tbytmatch.chunkratio.P.turns',
                                                    'relativ.tbytmatch.chunkratio.D.turns',
                                                    'relativ.tbytmatch.chunkratio.P.turns',
                                                    'informal.tbytmatch.chunkratio.D.turns',
                                                    'informal.tbytmatch.chunkratio.P.turns')


ECHO_tbyt_LIWC_matching_chunks_turns_variables_scaled = c('affect.tbytmatch.chunkratio.D.turns.scaled',
                                                          'affect.tbytmatch.chunkratio.P.turns.scaled',
                                                          'social.tbytmatch.chunkratio.D.turns.scaled',
                                                          'social.tbytmatch.chunkratio.P.turns.scaled',
                                                          'cogproc.tbytmatch.chunkratio.D.turns.scaled',
                                                          'cogproc.tbytmatch.chunkratio.P.turns.scaled',
                                                          'percept.tbytmatch.chunkratio.D.turns.scaled',
                                                          'percept.tbytmatch.chunkratio.P.turns.scaled',
                                                          'negemo.tbytmatch.chunkratio.D.turns.scaled',
                                                          'negemo.tbytmatch.chunkratio.P.turns.scaled',
                                                          'bio.tbytmatch.chunkratio.D.turns.scaled',
                                                          'bio.tbytmatch.chunkratio.P.turns.scaled',
                                                          'drives.tbytmatch.chunkratio.D.turns.scaled',
                                                          'drives.tbytmatch.chunkratio.P.turns.scaled',
                                                          'relativ.tbytmatch.chunkratio.D.turns.scaled',
                                                          'relativ.tbytmatch.chunkratio.P.turns.scaled',
                                                          'informal.tbytmatch.chunkratio.D.turns.scaled',
                                                          'informal.tbytmatch.chunkratio.P.turns.scaled')


# ECHO_tbyt_LIWC_matching_chunks_wc_variables = c('affect_chunkratio_D_turns', 'affect_chunkratio_P_turns', 'social_chunkratio_D_turns',
#                                                 'social_chunkratio_P_turns', 'cogproc_chunkratio_D_turns', 'cogproc_chunkratio_P_turns',
#                                                 'percept_chunkratio_D_turns', 'percept_chunkratio_P_turns', 'negemo_chunkratio_D_turns',
#                                                 'negemo_chunkratio_P_turns', 'bio_chunkratio_D_turns', 'bio_chunkratio_P_turns',
#                                                 'drives_chunkratio_D_turns', 'drives_chunkratio_P_turns', 'relativ_chunkratio_D_turns',
#                                                 'relativ_chunkratio_P_turns', 'informal_chunkratio_D_turns', 'informal_chunkratio_P_turns')
ECHO_tbyt_VADER_matching_chunks_wc_variables = c('compound.tbytmatch.D', 'compound.tbytmatch.P',
                                                 'pos.tbytmatch.D', 'pos.tbytmatch.P',
                                                 'neu.tbytmatch.D', 'neu.tbytmatch.P',
                                                 'neg.tbytmatch.D', 'neg.tbytmatch.P')

#this combines all the lists of matching variables into one list (VADER excluded for now)
ECHO_Matching_variables = c(ECHO_conv_LSM_variables, ECHO_tbyt_rLSM_variables, ECHO_conv_LIWC_Matching_variables,
                            ECHO_tbyt_LIWC_Matching_variables, ECHO_conv_LSM_chunks_turns_variables, ECHO_conv_LSM_chunks_wc_variables,
                            ECHO_tbyt_rLSM_chunks_turns_variables, ECHO_tbyt_rLSM_chunks_wc_variables, 
                            ECHO_tbyt_LIWC_matching_chunks_turns_variables, ECHO_conv_LIWC_Matching_turns_variables, ECHO_tbyt_LIWC_matching_chunks_wc_variables)

#ECHO_conv_LSM_chunks_turns_variables_scaled, ECHO_conv_LSM_chunks_wc_variables_scaled, ECHO_tbyt_rLSM_chunks_wc_variables_scaled,ECHO_tbyt_LIWC_matching_chunks_wc_variables_scaled
ECHO_Matching_variables_scaled = c(ECHO_conv_LSM_variables_scaled, ECHO_tbyt_rLSM_variables_scaled, ECHO_conv_LIWC_Matching_variables_scaled,
                            ECHO_tbyt_LIWC_Matching_variables_scaled, ECHO_conv_LSM_chunks_turns_variables_scaled,
                            ECHO_tbyt_rLSM_chunks_turns_variables_scaled, ECHO_tbyt_LIWC_matching_chunks_turns_variables_scaled, 
                            ECHO_conv_LIWC_Matching_turns_variables_scaled)


ECHO_Speech_variables = c('WC_D',  'Analytic_D', 'Clout_D', 'Authentic_D',  
                          'Tone_D',   ' WPS_D',   'Sixltr_D',    'Dic_D', 
                          'function_D', 'pronoun_D',  'ppron_D',     'i_D',  'we_D', 
                          'you_D',    ' shehe_D',    'they_D',    'ipron_D',   'article_D',    
                         'prep_D',   'auxverb_D',  'adverb_D',    'conj_D',    'negate_D',    
                          'verb_D',    'adj_D',   'compare_D', 'interrog_D', 'number_D',   
                          'quant_D',   'affect_D',   'posemo_D',   'negemo_D',  'anx_D', 
                          'anger_D', 'sad_D',     'social_D',  'family_D',   'friend_D',     
                          'female_D',    'male_D',   'cogproc_D', 'insight_D',  'cause_D',     
                          'discrep_D',  'tentat_D', 'certain_D',  'differ_D', 'percept_D',   
                          'see_D',     'hear_D',     'feel_D',     'bio_D',     'body_D',    
                          'health_D',  'sexual_D',   'ingest_D',   'drives_D', 'affiliation_D',  
                          'achieve_D',   'power_D',   'reward_D',   'risk_D',  'focuspast_D',   
                          'focuspresent_D',  'focusfuture_D', 'relativ_D',  'motion_D',  'space_D',    
                          'time_D',     'work_D',  'leisure_D',    'home_D',    'money_D',     
                          'relig_D',    'death_D',   'informal_D',  'swear_D',    'netspeak_D',   
                          'assent_D',   'nonflu_D',  'filler_D', 'WC_P',  'Analytic_P' ,   
                          'Clout_P', 'Authentic_P',  'Tone_P',    'WPS_P',   'Sixltr_P',     
                          'Dic_P',   'function_P', 'pronoun_P',  'ppron_P',     'i_P', 
                          'we_P', 'you_P',    'shehe_P',    'they_P',    'ipron_P', 
                          'article_P',   'prep_P',   'auxverb_P',   'adverb_P',    'conj_P', 
                          'negate_P',  'verb_P',    'adj_P',   'compare_P', 'interrog_P',   
                          'number_P',  'quant_P',   'affect_P',   'posemo_P',   'negemo_P',    
                         'anx_P',     'anger_P',     'sad_P',     'social_P',   'family_P',     
                          'friend_P',   'female_P',    'male_P',   'cogproc_P', 'insight_P',   
                          'cause_P',  'discrep_P',  'tentat_P', 'certain_P',  'differ_P',    
                          'percept_P',   'see_P', 'hear_P',    'feel_P',     'bio_P', 
                          'body_P',    'health_P',  'sexual_P',   'ingest_P',   'drives_P',    
                          'affiliation_P', 'achieve_P',   'power_P',   'reward_P',   'risk_P', 
                          'focuspast_P', 'focuspresent_P',  'focusfuture_P', 'relativ_P',  'motion_P',    
                          'space_P',     'time_P',     'work_P',   'leisure_P',    'home_P', 
                          'money_P',    'relig_P',    'death_P',    'informal_P',  'swear_P', 
                          'netspeak_P',  'assent_P',   'nonflu_P',   'filler_P')


#' WPS_D.scaled',
#' shehe_D.scaled',
ECHO_Speech_variables_scaled = c( 'WC_D.scaled',  'Analytic_D.scaled', 'Clout_D.scaled', 'Authentic_D.scaled',  
                                  'Tone_D.scaled', 'Sixltr_D.scaled',    'Dic_D.scaled', 
                                  'function_D.scaled', 'pronoun_D.scaled',  'ppron_D.scaled',     'i_D.scaled',  'we_D.scaled', 
                                  'you_D.scaled', 'they_D.scaled',    'ipron_D.scaled',   'article_D.scaled',    
                                  'prep_D.scaled',   'auxverb_D.scaled',  'adverb_D.scaled',    'conj_D.scaled',    'negate_D.scaled',    
                                  'verb_D.scaled',    'adj_D.scaled',   'compare_D.scaled', 'interrog_D.scaled', 'number_D.scaled',   
                                  'quant_D.scaled',   'affect_D.scaled',   'posemo_D.scaled',   'negemo_D.scaled',  'anx_D.scaled', 
                                  'anger_D.scaled', 'sad_D.scaled',     'social_D.scaled',  'family_D.scaled',   'friend_D.scaled',     
                                  'female_D.scaled',    'male_D.scaled',   'cogproc_D.scaled', 'insight_D.scaled',  'cause_D.scaled',     
                                  'discrep_D.scaled',  'tentat_D.scaled', 'certain_D.scaled',  'differ_D.scaled', 'percept_D.scaled',   
                                  'see_D.scaled',     'hear_D.scaled',     'feel_D.scaled',     'bio_D.scaled',     'body_D.scaled',    
                                  'health_D.scaled',  'sexual_D.scaled',   'ingest_D.scaled',   'drives_D.scaled', 'affiliation_D.scaled',  
                                  'achieve_D.scaled',   'power_D.scaled',   'reward_D.scaled',   'risk_D.scaled',  'focuspast_D.scaled',   
                                  'focuspresent_D.scaled',  'focusfuture_D.scaled', 'relativ_D.scaled',  'motion_D.scaled',  'space_D.scaled',    
                                  'time_D.scaled',     'work_D.scaled',  'leisure_D.scaled',    'home_D.scaled',    'money_D.scaled',     
                                  'relig_D.scaled',    'death_D.scaled',   'informal_D.scaled',  'swear_D.scaled',    'netspeak_D.scaled',   
                                  'assent_D.scaled',   'nonflu_D.scaled',  'filler_D.scaled', 'WC_P.scaled',  'Analytic_P.scaled' ,   
                                  'Clout_P.scaled', 'Authentic_P.scaled',  'Tone_P.scaled',    'WPS_P.scaled',   'Sixltr_P.scaled',     
                                  'Dic_P.scaled',   'function_P.scaled', 'pronoun_P.scaled',  'ppron_P.scaled',     'i_P.scaled', 
                                  'we_P.scaled', 'you_P.scaled',    'shehe_P.scaled',    'they_P.scaled',    'ipron_P.scaled', 
                                  'article_P.scaled',   'prep_P.scaled',   'auxverb_P.scaled',   'adverb_P.scaled',    'conj_P.scaled', 
                                  'negate_P.scaled',  'verb_P.scaled',    'adj_P.scaled',   'compare_P.scaled', 'interrog_P.scaled',   
                                  'number_P.scaled',  'quant_P.scaled',   'affect_P.scaled',   'posemo_P.scaled',   'negemo_P.scaled',    
                                  'anx_P.scaled',     'anger_P.scaled',     'sad_P.scaled',     'social_P.scaled',   'family_P.scaled',     
                                  'friend_P.scaled',   'female_P.scaled',    'male_P.scaled',   'cogproc_P.scaled', 'insight_P.scaled',   
                                  'cause_P.scaled',  'discrep_P.scaled',  'tentat_P.scaled', 'certain_P.scaled',  'differ_P.scaled',    
                                  'percept_P.scaled',   'see_P.scaled', 'hear_P.scaled',    'feel_P.scaled',     'bio_P.scaled', 
                                  'body_P.scaled',    'health_P.scaled',  'sexual_P.scaled',   'ingest_P.scaled',   'drives_P.scaled',    
                                  'affiliation_P.scaled', 'achieve_P.scaled',   'power_P.scaled',   'reward_P.scaled',   'risk_P.scaled', 
                                  'focuspast_P.scaled', 'focuspresent_P.scaled',  'focusfuture_P.scaled', 'relativ_P.scaled',  'motion_P.scaled',    
                                  'space_P.scaled',     'time_P.scaled',     'work_P.scaled',   'leisure_P.scaled',    'home_P.scaled', 
                                  'money_P.scaled',    'relig_P.scaled',    'death_P.scaled',    'informal_P.scaled',  'swear_P.scaled', 
                                  'netspeak_P.scaled',  'assent_P.scaled',   'nonflu_P.scaled',   'filler_P.scaled')


ECHO_survey_cultural_dissim = c('cultdiss', 'cultdissmd', 'cdspeak','cdreason','cdstyle','cdvalue','cdspirit','cdethnic',
                                'cdtype','cdrace','cdculture','cdskin','cultdissmd1', 'cultdissmd2', 'cultdissmd3', 'cultdissmd4', 
                                'cultdissmd5', 'cultdissmd6', 'cultdissmd7', 'cultdissmd8', 'cultdissmd9', 'cultdissmd10')

ECHO_survey_cultural_dissim_subscale = c('cdspeak', 'cdreason', 'cdstyle', 'cdrace')

ECHO_survey_provcomm = c('provcomm', 'provcommhigh', 'pcwords', 'pcfast', 'pctime', 'pclisten', 'pcignore',	
                         'pcinfo',	'pchealthprob',	'pcanytest', 'pcwhytest',	'pchowtest', 'pcexamine',	
                         'pcconfuse', 'pccarehome', 'pcsymp', 'pchowmeds', 	'pcgoovermeds',	'pcwritemeds', 
                         'pcreasonmeds',	'pcsemeds',	'pcdiff',	'pcactivities',	'pcinvolvedec',	'pcfelttreat', 
                         'pcprefopin', 'pcpressure', 'pcaskprob', 'pcunderprob')

ECHO_survey_overcomm = c('overcomm', 'overcommhigh', 'ocexplain', 'ocgive', 'octell', 'occare', 'ocunderstand')

ECHO_survey_ipstyle = c('ipstyle', 'ipstylehigh', 
                        'ipfriend',	'ipwelcome', 'iprude', 'ipcare',	
                        'ipname',	'iptalkfront', 'ippriv', 'ipinferior',	
                        'ipnegattitude', 'ipdiscrimrace', 'ipdiscrimeduc',	
                        'iplessworry', 'ipcompliment', 'ipcompassion')

ECHO_survey_iptrust = c('iptrust', 'iptrusttert', 
                        'iptdoubtcare',	'iptconsiderate',	'iptadvice',	
                        'ipttrue', 'iptdistrust', 'iptjudge',	'iptnotdo',	
                        'iptaboveall', 'iptwellqual', 'iptmistake', 'iptinfopriv')

ECHO_survey_provknowcat = c('provknowcat')

ECHO_survey_overallsat = c('overallsat')

ECHO_survey_viralsuppression = c('vlsup75')

ECHO_survey_adherence = c('adhard', 'adeasy', 'adunable', 'adfollow',
                          'adpast4', 'adpast30', 'pctarv', 'adrate', 'missany')

############################################################################################################
#Hypothesis: LSM(outcome) will be [lower with racial/ethnic minority patients]

#append only worked for two lists
H1.1_combined_variables <- c(ECHO_common_variables, ECHO_demographic_variables, ECHO_survey_cultural_dissim_subscale, ECHO_Matching_variables_scaled)
#H1.1_combined_variables <- append(ECHO_common_variables, ECHO_demographic_variables,ECHO_conv_LSM_variables)

H1.1_df <- ECHO_All_Matching %>%
  as_tibble() %>% 
  select(H1.1_combined_variables)%>%
  dplyr::filter(racecat2 !=4) %>%
  #added levels to the factor because the order without it was 2, 1, 3
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  mutate(gender = factor(gender, levels = c(1,2))) %>%
  mutate(working = factor(working, levels = c(0,1))) %>%
  mutate(marital = factor(marital, levels = c(0,1))) %>%
  mutate(hsdegree = factor(hsdegree, levels = c(0,1))) %>%
  #mutate(across(!provider_id & !racecat2, ~scale(.,center = TRUE, scale = TRUE))) %>%
  tidyr::drop_na()



############################################################################################################
#Hypothesis: LSM (outcome) is higher within dyads characterized by race concordance
H1.2_combined_variables <- c(ECHO_common_variables, ECHO_demographic_variables, ECHO_raceconc_variables, ECHO_Matching_variables_scaled)

H1.2_df <- ECHO_All_Matching %>%
  as_tibble() %>% 
  select(H1.2_combined_variables) %>%
  mutate(raceconc = factor(raceconc)) %>%
  mutate(hsdegree = factor(hsdegree, levels = c(0,1))) %>%
  tidyr::drop_na()

############################################################################################################
#Hypothesis: LSM (outcome) is higher for those who rate each other as culturally similar
#need to add the dissimilarity distance variable
H1.3_combined_variables <- c(ECHO_common_variables, ECHO_demographic_variables, ECHO_survey_cultural_dissim, ECHO_Matching_variables,
                             ECHO_Matching_variables_scaled, ECHO_Speech_variables_scaled)

H1.3_df <- ECHO_All_Matching %>%
  as_tibble() %>% 
  select(H1.3_combined_variables)%>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  tidyr::drop_na() %>% # take only rows with no NA;
  mutate(
    cdAvg_dist_abs = abs(cultdiss - cultdissmd),
    cdspeak_dist_abs = abs(cdspeak - cultdissmd1),
    cdreason_dist_abs = abs(cdreason - cultdissmd2),
    cdstyle_dist_abs = abs(cdstyle - cultdissmd3),
    cdvalue_dist_abs = abs(cdvalue - cultdissmd4),
    cdspirit_dist_abs = abs(cdspirit - cultdissmd5),
    cdethnic_dist_abs = abs(cdethnic - cultdissmd6),
    cdtype_dist_abs = abs(cdtype - cultdissmd7),
    cdrace_dist_abs = abs(cdrace - cultdissmd8),
    cdculture_dist_abs = abs(cdculture - cultdissmd9),
    cdskin_dist_abs = abs(cdskin - cultdissmd10)) %>%
  mutate(hsdegree = factor(hsdegree, levels = c(0,1)))

H1.3_df <- H1.3_df %>%
  mutate(cdAvg_dist = (cultdiss - cultdissmd),
         cdspeak_dist = (cdspeak - cultdissmd1),
         cdreason_dist = (cdreason - cultdissmd2),
         cdstyle_dist = (cdstyle - cultdissmd3),
         cdvalue_dist = (cdvalue - cultdissmd4),
         cdspirit_dist = (cdspirit - cultdissmd5),
         cdethnic_dist = (cdethnic - cultdissmd6),
         cdtype_dist = (cdtype - cultdissmd7),
         cdrace_dist = (cdrace - cultdissmd8),
         cdculture_dist = (cdculture - cultdissmd9),
         cdskin_dist = (cdskin - cultdissmd10)
  )


############################################################################################################
#Using LSM_function_mean to predict communication quality with provider (provcomm)
H3a.1_combined_variables <- c(ECHO_common_variables, ECHO_demographic_variables, 
                              ECHO_survey_provcomm, 
                              ECHO_Matching_variables_scaled, ECHO_Speech_variables_scaled)

H3a.1_df <- ECHO_All_Matching %>%
  as_tibble() %>% 
  select(H3a.1_combined_variables)%>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  mutate(hsdegree = factor(hsdegree, levels = c(0,1))) %>%
  tidyr::drop_na()

############################################################################################################
#Using LSM_function_mean to predict overall communication quality (overcomm)
H3a.2_combined_variables <- c(ECHO_common_variables, ECHO_demographic_variables, ECHO_survey_overcomm, ECHO_Matching_variables_scaled)

H3a.2_df <- ECHO_All_Matching %>%
  as_tibble() %>% 
  select(H3a.2_combined_variables)%>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3")))  %>%
  mutate(hsdegree = factor(hsdegree, levels = c(0,1))) %>%
  tidyr::drop_na()

############################################################################################################
#Using LSM_function_mean to predict provider interpersonal style (ipstyle)
H3a.3_combined_variables <- c(ECHO_common_variables, ECHO_demographic_variables, 
                              ECHO_survey_ipstyle, 
                              ECHO_Matching_variables_scaled, ECHO_Speech_variables_scaled)

H3a.3_df <- ECHO_All_Matching %>%
  as_tibble() %>% 
  select(H3a.3_combined_variables)%>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  mutate(hsdegree = factor(hsdegree, levels = c(0,1))) %>%
  tidyr::drop_na()

############################################################################################################
#Using LSM_function_mean to predict interpersonal trust (iptrust)
H3a.4_combined_variables <- c(ECHO_common_variables, ECHO_demographic_variables, ECHO_survey_iptrust, ECHO_Matching_variables_scaled)

H3a.4_df <- ECHO_All_Matching %>%
  as_tibble() %>% 
  select(H3a.4_combined_variables)%>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  mutate(hsdegree = factor(hsdegree, levels = c(0,1))) %>%
  tidyr::drop_na()

H3a.4_df$iptrust_scaled <- scale(H3a.4_df$iptrust, center = TRUE, scale = TRUE)
############################################################################################################
#Using LSM_function_mean to predict if patient reports that provider knows them as a person (provknowcat)
H3a.5_combined_variables <- c(ECHO_common_variables, ECHO_demographic_variables, ECHO_survey_provknowcat, ECHO_Matching_variables_scaled)

H3a.5_df <- ECHO_All_Matching %>%
  as_tibble() %>% 
  select(H3a.5_combined_variables)%>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  mutate(hsdegree = factor(hsdegree, levels = c(0,1))) %>%
  tidyr::drop_na()


############################################################################################################
#Using LSM_function_mean to predict overall patient satisfaction (overallsat)
H3a.6_combined_variables <- c(ECHO_common_variables, ECHO_demographic_variables, ECHO_survey_overallsat, ECHO_Matching_variables_scaled)

H3a.6_df <- ECHO_All_Matching %>%
  as_tibble() %>% 
  select(H3a.6_combined_variables)%>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  mutate(hsdegree = factor(hsdegree, levels = c(0,1))) %>%
  tidyr::drop_na()




#################
H3a.7_combined_variables <- c(ECHO_common_variables, ECHO_demographic_variables, ECHO_survey_cultural_dissim,
                             ECHO_Matching_variables_scaled, ECHO_Speech_variables_scaled)



H3a.7_df <- ECHO_All_Matching %>%
  as_tibble() %>% 
  select(H3a.7_combined_variables)%>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  tidyr::drop_na()





############################################################################################################
#Using LSM_function_mean to predict viral suppression
H3b.1_combined_variables <- c(ECHO_common_variables, ECHO_demographic_variables, ECHO_survey_viralsuppression, ECHO_Matching_variables_scaled)

H3b.1_df <- ECHO_All_Matching %>%
  as_tibble() %>% 
  select(H3b.1_combined_variables)%>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  mutate(hsdegree = factor(hsdegree, levels = c(0,1))) %>%
  tidyr::drop_na()


############################################################################################################
#Using LSM_function_mean to predict adherence measures
#adhard, adeasy, adunable, adfollow,adpast4, adpast30, pctarv, adrate, missany(binomial)
H4_combined_variables <- c(ECHO_common_variables, ECHO_demographic_variables, ECHO_survey_adherence, ECHO_Matching_variables_scaled)

H4_df <- ECHO_All_Matching %>%
  as_tibble() %>% 
  select(H4_combined_variables)%>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  mutate(hsdegree = factor(hsdegree, levels = c(0,1))) %>%
  tidyr::drop_na()
