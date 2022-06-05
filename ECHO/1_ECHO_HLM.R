#############
#### ECHO Scripts for HLM

#packages
library(lme4)
library(jtools)
library(sjPlot)
library(performance)

#Used lme4 to run glmer by adding  "family = 'binomial'" to code
#install.packages("glmer")
#library(glmer)
############################################################################################################
# Predictor & control variable lists
############################################################################################################
cntrl_vars <- c('site_id')
conv_styl_preds <- ECHO_conv_LSM_variables
conv_cntnt_preds <- ECHO_conv_LIWC_Matching_variables
tbyt_styl_preds <- ECHO_tbyt_rLSM_variables
tbyt_cntnt_preds <- ECHO_tbyt_LIWC_Matching_variables
conv_styl_chnkd_preds <- ECHO_conv_LSM_chunks_turns_variables
conv_cntnt_chnkd_preds <- ECHO_conv_LIWC_Matching_turns_variables
tbyt_styl_chnkd_preds <- ECHO_tbyt_rLSM_chunks_turns_variables 
tbyt_cntnt_chnkd_preds <- ECHO_tbyt_LIWC_matching_chunks_turns_variables

## Notes on models
# M0 is Null lm
# M1 is provier id grouping var only
# M2 is M1 + conv_styl_preds + cntrl_vars
# M3 is M1 + conv_cntnt_preds + cntrl_vars
# M4 is M1 + tbyt_styl_preds + cntrl_vars
# M5 is M1 + tbyt_cntnt_preds + cntrl_vars
# M6 is M1 + conv_styl_chnkd_preds + cntrl_vars
# M7 is M1 + conv_cntnt_chnkd_preds + cntrl_vars
# M8 is M1 + tbyt_styl_chnkd_preds + cntrl_vars
# M9 is M1 + tbyt_cntnt_chnkd_preds + cntrl_vars


############################################################################################################
# model H1.1: LSM(outcome) will be [lower with racial/ethnic minority patients](with 4 dissimilarity items added)
############################################################################################################

# step 1: Null model
m.0_H1.1_HLM <- lm(
  # LSM_function_mean ~ 1, 
  # rLSM.D ~ 1,
  rLSM.D ~ 1,
  data = H1.1_df)
summary(m.0_H1.1_HLM)
summ(m.0_H1.1_HLM)

# step 2: add the culstering variable --> "provider_id"
m.1_H1.1_HLM <- lmer(rLSM.P ~ 1 + (1|provider_id), 
                     data = H1.1_df)

# test for fit of grouping structure
anova(m.1_H1.1_HLM,m.0_H1.1_HLM)
summ(m.1_H1.1_HLM)
summary(m.1_H1.1_HLM)
sjPlot::tab_model(m.1_H1.1_HLM)

# step 3: add the predictors, racecat2
m.2_H1.1_HLM_rLSM.P <- lm(rLSM.P ~ 
                       racecat2 +
                       cdspeak + 
                       cdreason +
                       cdstyle +
                       cdrace +
                       age +
                       gender+
                       working+
                       marital+
                       hsdegree,
                     #(1|provider_id) 
                       data = H1.1_df)
anova(m.1_H1.1_HLM,m.2_H1.1_HLM)
summary(m.2_H1.1_HLM)
summ(m.2_H1.1_HLM)
plot(m.2_H1.1_HLM)
sjPlot::tab_model(m.2_H1.1_HLM)
sjPlot::tab_model(m.2_H1.1_HLM_conv,m.2_H1.1_HLM_rLSM.P,m.2_H1.1_HLM_rLSM.D)


############################################################################################################
# model H1.2: LSM (outcome) is higher within dyads characterized by race concordance
############################################################################################################

m.0_H1.2_HLM <- lm(LSM_function_mean ~ 1, data = H1.2_df)
summary(m.0_H1.2_HLM)
summ(m.0_H1.2_HLM)

# step 2: add the culstering variable --> "provider_id"
m.1_H1.2_HLM <- lmer(LSM_function_mean ~ 1 + (1|provider_id), 
                        data = H1.2_df)

# test for fit of grouping structure
anova(m.1_H1.2_HLM,m.0_H1.2_HLM)
summ(m.1_H1.2_HLM)
summary(m.1_H1.2_HLM)

# step 3: add the predictors, raceconc
m.2_H1.2_HLM <- lm(LSM_function_mean ~ 
               raceconc, data = H1.2_df)
anova(m.1_H1.2_HLM,m.2_H1.2_HLM)
summary(m.2_H1.2_HLM)
summ(m.2_H1.2_HLM)
plot(m.2_H1.2_HLM)
sjPlot::tab_model(m.2_H1.2_HLM)



############################################################################################################
# model H1.3: LSM (outcome) is higher for those who rate each other as culturally similar
#need to add the dissimilarity distance variable
############################################################################################################


m.0_H1.3_HLM <- lm(LSM_function_mean ~ 1, data = H1.3_df)
summary(m.0_H1.3_HLM)
summ(m.0_H1.3_HLM)

# step 2: add the culstering variable --> "provider_id"
m.1_H1.3_HLM <- lme4::lmer(LSM_function_mean ~ 1 + (1|provider_id), 
                        data = H1.3_df)

# test for fit of grouping structure
anova(m.1_H1.3_HLM,m.0_H1.3_HLM)
summ(m.1_H1.3_HLM)
summary(m.1_H1.3_HLM)

##NOTE: This error message "boundary (singular) fit: see ?isSingular"
##means the random effects is very small so I changed it to a simple lm.
# step 3: add the predictors, cultdiss, cultdissmd
m.2_H1.3_HLM_conv <- lm(
  LSM_function_mean ~
  #rLSM.D ~
  #rLSM.P ~
                     # cultdiss +
                     # cultdissmd +
                     # cdAvg_dist_abs +
                     # cdspeak_dist_abs +
                     # cdreason_dist_abs +
                     # cdstyle_dist_abs +
                     # cdvalue_dist_abs +
                     # cdspirit_dist_abs +
                     # cdethnic_dist_abs +
                     # cdtype_dist_abs +
                     # cdrace_dist_abs +
                     # cdculture_dist_abs +
                     # cdskin_dist_abs +
                     # cdAvg_dist +
                     cdspeak_dist +
                     cdreason_dist +
                     cdstyle_dist +
                     # cdvalue_dist +
                     # cdspirit_dist +
                     # cdethnic_dist +
                     # cdtype_dist +
                     cdrace_dist+
                     # cdculture_dist+
                     # cdskin_dist,# +
                     racecat2 + 
                      hsdegree,
                     # (1|provider_id),
                   data = H1.3_df)

anova(m.1_H1.3_HLM,m.2_H1.3_HLM)
summary(m.2_H1.3_HLM)
summ(m.2_H1.3_HLM)
#plot(m.2_H1.3_HLM)
sjPlot::tab_model(m.2_H1.3_HLM_rLSM.P)
sjPlot::tab_model(m.2_H1.3_HLM_conv,m.2_H1.3_HLM_rLSM.D,m.2_H1.3_HLM_rLSM.P)


############################################################################################################
# model H3a.1: Using LSM_function_mean to predict communication quality with provider (provcomm)
# provcomm items(provcommhigh, pcwords, pcfast, pctime,	pclisten, pcignore,	pcinfo,	pchealthprob,	
# pcanytest, pcwhytest,	pchowtest, pcexamine,	pcconfuse, pccarehome, pcsymp, pchowmeds, 	
# pcgoovermeds,	pcwritemeds, pcreasonmeds, pcsemeds,	pcdiff,	pcactivities,	pcinvolvedec,	
# pcfelttreat, pcprefopin, pcpressure, pcaskprob, pcunderprob)
############################################################################################################

# step 1: Null model
m.0_H3a.1_HLM <- lm(provcomm~ 1, data = H3a.1_df)

# step 2: add the culstering variable.. MD
#m.1_H3a.1_HLM <- lme4::lmer(provcomm ~ 1 + (1|provider_id), data = H3a.1_df)
m.1_H3a.1_HLM <- lme4::glmer(provcommhigh ~ 1 + (1|provider_id), family= 'binomial', data = H3a.1_df)
# test for fit of grouping structure
anova(m.1_H3a.1_HLM,m.0_H3a.1_HLM)

## Notes on models
# M0 is Null lm
# M1 is provier id grouping var only



# NEW step 3 using update
# M2 is M1 + conv_styl_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_styl_preds)
m.2_H3a.1_HLM_conv <- update(m.1_H3a.1_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.2_H3a.1_HLM_conv)
sjPlot::tab_model(m.2_H3a.1_HLM_conv)
check_heteroscedasticity(m.2_H3a.1_HLM_conv)
check_model(m.2_H3a.1_HLM_conv)
model_performance(m.2_H3a.1_HLM_conv)
# M3 is M1 + conv_cntnt_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_cntnt_preds)
m.3_H3a.1_HLM_conv <- update(m.1_H3a.1_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.3_H3a.1_HLM_conv)
sjPlot::tab_model(m.3_H3a.1_HLM_conv)
check_heteroscedasticity(m.2_H3a.1_HLM_conv)
check_model(m.3_H3a.1_HLM_conv)
model_performance(m.3_H3a.1_HLM_conv)
# M4 is M1 + tbyt_styl_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_styl_preds)
m.4_H3a.1_HLM_conv <- update(m.1_H3a.1_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.4_H3a.1_HLM_conv)
sjPlot::tab_model(m.4_H3a.1_HLM_conv)
# M5 is M1 + tbyt_cntnt_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_cntnt_preds)
m.5_H3a.1_HLM_conv <- update(m.1_H3a.1_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.5_H3a.1_HLM_conv)
sjPlot::tab_model(m.5_H3a.1_HLM_conv)
# M6 is M1 + conv_styl_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_styl_chnkd_preds)
m.6_H3a.1_HLM_conv <- update(m.1_H3a.1_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.6_H3a.1_HLM_conv)
sjPlot::tab_model(m.6_H3a.1_HLM_conv)
# M7 is M1 + conv_cntnt_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_cntnt_chnkd_preds)
m.7_H3a.1_HLM_conv <- update(m.1_H3a.1_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.7_H3a.1_HLM_conv)
sjPlot::tab_model(m.7_H3a.1_HLM_conv)
# M8 is M1 + tbyt_styl_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_styl_chnkd_preds)
m.8_H3a.1_HLM_conv <- update(m.1_H3a.1_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.8_H3a.1_HLM_conv)
sjPlot::tab_model(m.8_H3a.1_HLM_conv)
# M9 is M1 + tbyt_cntnt_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_cntnt_chnkd_preds)
m.9_H3a.1_HLM_conv <- update(m.1_H3a.1_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.9_H3a.1_HLM_conv)
sjPlot::tab_model(m.9_H3a.1_HLM_conv)


predictors <- append(cntrl_vars,coef_list.provcomm.matched)
predictors <- append(cntrl_vars,coef_list.provcomm.speech)

  
m.10_H3a.1_HLM_conv <- update(m.1_H3a.1_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.10_H3a.1_HLM_conv)
sjPlot::tab_model(m.10_H3a.1_HLM_conv)


#backward elimination...
m.10_H3a.1_HLM_conv <- update(m.10_H3a.1_HLM_conv, ".~. -social_chunkratio_P_turns.scaled")
m.10_H3a.1_HLM_conv <- update(m.10_H3a.1_HLM_conv, ".~. -relativ_chunkratio_P_turns.scaled")
m.10_H3a.1_HLM_conv <- update(m.10_H3a.1_HLM_conv, ".~. -negemo_chunkratio_turns.scaled")


check_heteroscedasticity(m.10_H3a.1_HLM_conv)
check_model(m.10_H3a.1_HLM_conv)
model_performance(m.10_H3a.1_HLM_conv)


"rLSM.D.scaled+conv.bio.match.scaled+affect.tbytmatch.P.scaled+social.tbytmatch.P.scaled+
social_chunkratio_P_turns.scaled+percept_chunkratio_P_turns.scaled+informal_chunkratio_P_turns.scaled+cogproc_chunkratio_turns.scaled+negemo_chunkratio_turns.scaled"
############################################################################################################
# model H3a.2: Using LSM_function_mean to predict overall communication quality (overcomm)
# overcomm items(overcommhigh, ocexplain,ocgive, octell, occare, ocunderstand)
############################################################################################################

# step 1: Null model
m.0_H3a.2_HLM <- lm(overcomm~ 1, data = H3a.2_df)
summary(m.0_H3a.2_HLM)

# step 2: add the culstering variable.. MD
m.1_H3a.2_HLM <- lmer(overcomm ~ 1 + (1|provider_id), data = H3a.2_df)
# test for fit of grouping structure
anova(m.1_H3a.2_HLM,m.0_H3a.2_HLM)
sjPlot::tab_model(m.1_H3a.2_HLM)

## Notes on models
# M0 is Null lm
# M1 is provier id grouping var only



# NEW step 3 using update
# M2 is M1 + conv_styl_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_styl_preds)
m.2_H3a.2_HLM_conv <- update(m.1_H3a.2_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.2_H3a.2_HLM_conv)
sjPlot::tab_model(m.2_H3a.2_HLM_conv)
# M3 is M1 + conv_cntnt_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_cntnt_preds)
m.3_H3a.2_HLM_conv <- update(m.1_H3a.2_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.3_H3a.2_HLM_conv)
sjPlot::tab_model(m.3_H3a.2_HLM_conv)
# M4 is M1 + tbyt_styl_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_styl_preds)
m.4_H3a.2_HLM_conv <- update(m.1_H3a.2_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.4_H3a.2_HLM_conv)
sjPlot::tab_model(m.4_H3a.2_HLM_conv)
# M5 is M1 + tbyt_cntnt_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_cntnt_preds)
m.5_H3a.2_HLM_conv <- update(m.1_H3a.2_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.5_H3a.2_HLM_conv)
sjPlot::tab_model(m.5_H3a.2_HLM_conv)
# M6 is M1 + conv_styl_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_styl_chnkd_preds)
m.6_H3a.2_HLM_conv <- update(m.1_H3a.2_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.6_H3a.2_HLM_conv)
sjPlot::tab_model(m.6_H3a.2_HLM_conv)
# M7 is M1 + conv_cntnt_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_cntnt_chnkd_preds)
m.7_H3a.2_HLM_conv <- update(m.1_H3a.2_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.7_H3a.2_HLM_conv)
sjPlot::tab_model(m.7_H3a.2_HLM_conv)
# M8 is M1 + tbyt_styl_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_styl_chnkd_preds)
m.8_H3a.2_HLM_conv <- update(m.1_H3a.2_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.8_H3a.2_HLM_conv)
sjPlot::tab_model(m.8_H3a.2_HLM_conv)
# M9 is M1 + tbyt_cntnt_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_cntnt_chnkd_preds)
m.9_H3a.2_HLM_conv <- update(m.1_H3a.2_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.9_H3a.2_HLM_conv)
sjPlot::tab_model(m.9_H3a.2_HLM_conv)


############################################################################################################
# model H3a.3: Using LSM_function_mean to predict provider interpersonal style (ipstyle)
# ipstyle items(ipstylehigh,ipfriend,	ipwelcome, iprude, ipcare,ipname,	iptalkfront, ippriv, ipinferior,	
# ipnegattitude, ipdiscrimrace, ipdiscrimeduc, iplessworry, ipcompliment, ipcompassion)
############################################################################################################
# step 1: Null model
#m.0_H3a.3_HLM <- lm(ipstylehigh~ 1, data = H3a.3_df)
m.0_H3a.3_HLM <- glm(ipstylehigh~ 1, family = 'binomial', data = H3a.3_df)

# step 2: add the culstering variable.. MD
#m.1_H3a.3_HLM <- lmer(ipstylehigh ~ 1 + (1|provider_id), data = H3a.3_df)
#m.1_H3a.3_HLM <- glmer(ipstylehigh ~ 1 + (1|provider_id), family = 'binomial', data = H3a.3_df)
# test for fit of grouping structure
anova(m.1_H3a.3_HLM,m.0_H3a.3_HLM)

# NEW step 3 using update
# M2 is M1 + conv_styl_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_styl_preds)
m.2_H3a.3_HLM_conv <- update(m.1_H3a.3_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.2_H3a.3_HLM_conv)
sjPlot::tab_model(m.2_H3a.3_HLM_conv)
# M3 is M1 + conv_cntnt_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_cntnt_preds)
m.3_H3a.3_HLM_conv <- update(m.1_H3a.3_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.3_H3a.3_HLM_conv)
sjPlot::tab_model(m.3_H3a.3_HLM_conv)
# M4 is M1 + tbyt_styl_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_styl_preds)
m.4_H3a.3_HLM_conv <- update(m.1_H3a.3_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.4_H3a.3_HLM_conv)
sjPlot::tab_model(m.4_H3a.3_HLM_conv)
# M5 is M1 + tbyt_cntnt_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_cntnt_preds)
m.5_H3a.3_HLM_conv <- update(m.1_H3a.3_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.5_H3a.3_HLM_conv)
sjPlot::tab_model(m.5_H3a.3_HLM_conv)
# M6 is M1 + conv_styl_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_styl_chnkd_preds)
m.6_H3a.3_HLM_conv <- update(m.1_H3a.3_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.6_H3a.3_HLM_conv)
sjPlot::tab_model(m.6_H3a.3_HLM_conv)
# M7 is M1 + conv_cntnt_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_cntnt_chnkd_preds)
m.7_H3a.3_HLM_conv <- update(m.1_H3a.3_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.7_H3a.3_HLM_conv)
sjPlot::tab_model(m.7_H3a.3_HLM_conv)
# M8 is M1 + tbyt_styl_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_styl_chnkd_preds)
m.8_H3a.3_HLM_conv <- update(m.1_H3a.3_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.8_H3a.3_HLM_conv)
sjPlot::tab_model(m.8_H3a.3_HLM_conv)
# M9 is M1 + tbyt_cntnt_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_cntnt_chnkd_preds)
m.9_H3a.3_HLM_conv <- update(m.1_H3a.3_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.9_H3a.3_HLM_conv)
sjPlot::tab_model(m.9_H3a.3_HLM_conv)

predictors <- append(cntrl_vars,coef_list.ipstyle.matched)
#predictors <- append(cntrl_vars,coef_list.ipstyle.speech)

m.10_H3a.3_HLM_conv <- update(m.0_H3a.3_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.10_H3a.3_HLM_conv)
sjPlot::tab_model(m.10_H3a.3_HLM_conv)


#backward elimination...
m.10_H3a.3_HLM_conv <- update(m.10_H3a.3_HLM_conv, ".~. -quant_P.scaled")
m.10_H3a.3_HLM_conv <- update(m.10_H3a.3_HLM_conv, ".~. -relig_P.scaled")
m.10_H3a.3_HLM_conv <- update(m.10_H3a.3_HLM_conv, ".~. -percept_P.scaled")
m.10_H3a.3_HLM_conv <- update(m.10_H3a.3_HLM_conv, ".~. -focuspresent_P.scaled")

check_heteroscedasticity(m.10_H3a.3_HLM_conv)
check_model(m.10_H3a.3_HLM_conv)
model_performance(m.10_H3a.3_HLM_conv)
############################################################################################################
# model H3a.4: Using LSM_function_mean to predict interpersonal trust (iptrust)
# iptrust items(iptrusttert, iptdoubtcare,	iptconsiderate,	iptadvice, ipttrue, iptdistrust, 
# iptjudge,	iptnotdo,	iptaboveall, iptwellqual, iptmistake, iptinfopriv)
############################################################################################################

# step 1: Null model
m.0_H3a.4_HLM <- lm(iptrust~ 1, data = H3a.4_df)

# step 2: add the culstering variable.. MD
m.1_H3a.4_HLM <- lmer(iptrust ~ 1 + (1|provider_id), data = H3a.4_df)
# test for fit of grouping structure
anova(m.1_H3a.4_HLM,m.0_H3a.4_HLM)

# NEW step 3 using update
# M2 is M1 + conv_styl_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_styl_preds)
m.2_H3a.4_HLM_conv <- update(m.1_H3a.4_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.2_H3a.4_HLM_conv)
sjPlot::tab_model(m.2_H3a.4_HLM_conv)
# M3 is M1 + conv_cntnt_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_cntnt_preds)
m.3_H3a.4_HLM_conv <- update(m.1_H3a.4_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.3_H3a.4_HLM_conv)
sjPlot::tab_model(m.3_H3a.4_HLM_conv)
# M4 is M1 + tbyt_styl_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_styl_preds)
m.4_H3a.4_HLM_conv <- update(m.1_H3a.4_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.4_H3a.4_HLM_conv)
sjPlot::tab_model(m.4_H3a.4_HLM_conv)
# M5 is M1 + tbyt_cntnt_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_cntnt_preds)
m.5_H3a.4_HLM_conv <- update(m.1_H3a.4_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.5_H3a.4_HLM_conv)
sjPlot::tab_model(m.5_H3a.4_HLM_conv)
# M6 is M1 + conv_styl_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_styl_chnkd_preds)
m.6_H3a.4_HLM_conv <- update(m.1_H3a.4_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.6_H3a.4_HLM_conv)
sjPlot::tab_model(m.6_H3a.4_HLM_conv)
# M7 is M1 + conv_cntnt_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,conv_cntnt_chnkd_preds)
m.7_H3a.4_HLM_conv <- update(m.1_H3a.4_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.7_H3a.4_HLM_conv)
sjPlot::tab_model(m.7_H3a.4_HLM_conv)
# M8 is M1 + tbyt_styl_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_styl_chnkd_preds)
m.8_H3a.4_HLM_conv <- update(m.1_H3a.4_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.8_H3a.4_HLM_conv)
sjPlot::tab_model(m.8_H3a.4_HLM_conv)
# M9 is M1 + tbyt_cntnt_chnkd_preds + cntrl_vars
predictors <- append(cntrl_vars,tbyt_cntnt_chnkd_preds)
m.9_H3a.4_HLM_conv <- update(m.1_H3a.4_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.9_H3a.4_HLM_conv)
sjPlot::tab_model(m.9_H3a.4_HLM_conv)



############################################################################################################
# model H3a.5: Using LSM_function_mean to predict if patient reports that provider knows them as a person (provknowcat)
############################################################################################################

# step 1: Null model
m.0_H3a.5_HLM <- glm(provknowcat ~ 1, data = H3a.5_df)

# step 2: add the culstering variable.. MD
m.1_H3a.5_HLM <- glmer(provknowcat ~ 1 + (1|provider_id), family = 'binomial',data = H3a.5_df)
# test for fit of grouping structure
anova(m.1_H3a.5_HLM,m.0_H3a.5_HLM)

# step 3: add the predictors
m.2_H3a.5_HLM_rLSM.D <- glm(provknowcat ~ 
                              # LSM_function_mean,
                                 #rLSM.P,
                                rLSM.D,
                              #   WPS_avg.D +
                              #   WPS_avg.P +
                              #   WC_sum.D +
                              #   WC_sum.P +
                              # mean.rLSM +
                              # ratio.rLSM,# +
                         # WC_D_scaled+
                         # WC_P_scaled+
                         # WPS_D_scaled+
                         # WPS_P_scaled+
                         # Sixltr_D_scaled+
                         # Sixltr_P_scaled+
                         # affiliation_D_scaled+
                         # affiliation_P_scaled+
                         # i_D_scaled+
                         # i_P_scaled+
                         # Clout_D_scaled+
                         # Clout_P_scaled+
                         # differ_D_scaled+
                         # differ_P_scaled+
                         # insight_D_scaled+
                         # insight_P_scaled+
                         # cause_D_scaled+
                         # cause_P_scaled+
                         # provider_style_sd +
                      # (1|provider_id), 
                      # family = 'binomial', 
                      data = H3a.5_df)
anova(m.1_H3a.5_HLM,m.2_H3a.5_HLM)
summary(m.2_H3a.5_HLM)

sjPlot::tab_model(m.2_H3a.5_HLM)

sjPlot::tab_model(m.2_H3a.5_HLM_conv,m.2_H3a.5_HLM_rLSM.D,m.2_H3a.5_HLM_rLSM.P)
############################################################################################################
# model H3a.6: Using LSM_function_mean to predict overall patient satisfaction (overallsat)
############################################################################################################

# step 1: Null model
m.0_H3a.6_HLM <- lm(overallsat~ 1, data = H3a.6_df)

# step 2: add the culstering variable.. MD
m.1_H3a.6_HLM <- lmer(overallsat ~ 1 + (1|provider_id), data = H3a.6_df)
# test for fit of grouping structure
anova(m.1_H3a.6_HLM,m.0_H3a.6_HLM)

# step 3: add the predictors
m.2_H3a.6_HLM <- lmer(overallsat ~ 
                         # LSM_function_mean +
                        # rLSM.P+
                        rLSM.D+
                         # racecat2 +
                         # LSM_function_mean*racecat2+
                         # provider_style_sd+
                       (1|provider_id), data = H3a.6_df)
anova(m.1_H3a.6_HLM,m.2_H3a.6_HLM)
summary(m.2_H3a.6_HLM)
sjPlot::tab_model(m.2_H3a.6_HLM)


############################################################################################################
# model H3a.7: Using LSM_function_mean to predict cdspeak
############################################################################################################

# step 1: Null model
m.0_H3a.7_HLM <- lm(cdspeak~ 1, data = H3a.7_df)


# step 2: add the culstering variable.. MD
#m.1_H3a.7_HLM <- lmer(cdspeak ~ 1 + (1|provider_id), data = H3a.7_df)

# test for fit of grouping structure
anova(m.1_H3a.7_HLM,m.0_H3a.7_HLM)


predictors <- append(cntrl_vars,coef_list.cdspeak.matching)
#predictors <- append(cntrl_vars,coef_list.cdspeak.speech)

m.10_H3a.7_HLM_conv <- update(m.0_H3a.7_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
summary(m.10_H3a.7_HLM_conv)
sjPlot::tab_model(m.10_H3a.7_HLM_conv)
#backward elimination...
m.10_H3a.7_HLM_conv <- update(m.10_H3a.7_HLM_conv, ".~. -focuspresent_D.scaled")
m.10_H3a.7_HLM_conv <- update(m.10_H3a.7_HLM_conv, ".~. -conj_P.scaled")

############################################################################################################
# model H3b.1: Using LSM_function_mean to predict viral suppression
############################################################################################################

# step 1: Null model
m.0_H3b.1_HLM <- glm(vlsup75~ 1, family = 'binomial', data = H3b.1_df)

# step 2: add the culstering variable.. MD
m.1_H3b.1_HLM <- glmer(factor(vlsup75) ~ 1 + (1|provider_id), family = 'binomial', data = H3b.1_df )
# test for fit of grouping structure
anova(m.0_H3b.1_HLM,m.1_H3b.1_HLM)
summary(m.1_H3b.1_HLM)
# step 3: add the predictors
m.2_H3b.1_HLM <- glmer(factor(vlsup75) ~ 
                     #LSM_function_mean +
                       rLSM.D +
                       rLSM.P +
                       # rLSM.D*rLSM.P +
                     # i_D_scaled+
                     # i_P_scaled+
                     # negemo_D_scaled+
                     # negemo_P_scaled+
                     #racecat2 +
                       #hsdegree,# +
                     # rLSM.D*racecat2+
                     # provider_rLSM_sd+
                  (1|provider_id) +
                 family = 'binomial',
                 data = H3b.1_df)
anova(m.1_H3b.1_HLM,m.2_H3b.1_HLM)
summary(m.2_H3b.1_HLM)
sjPlot::tab_model(m.2_H3b.1_HLM)
summary(m.1_H3b.1_HLM)
sjPlot::tab_model(m.2_H3b.1_HLM_conv, )



############################################################################################################
# model H4: #Using LSM_function_mean to predict adherence measures
#adhard, adeasy, adunable, adfollow,adpast4, adpast30, pctarv, adrate, missany(binomial)

############################################################################################################

# step 1: Null model
m.0_H4_HLM <- glm(missany~ 1, family = 'binomial', data = H4_df)

# step 2: add the culstering variable.. MD
m.1_H4_HLM <- glmer(missany ~ 1 + (1|provider_id), family = 'binomial', data = H4_df)
# test for fit of grouping structure
anova(m.1_H4_HLM,m.0_H4_HLM)

# step 3: add the predictors
m.2_H4_HLM <- glm(missany ~ 
                        LSM_function_mean,# +
                        # rLSM.D,# +
                        # rLSM.P,# +
                        # racecat2+
                        # WC_D_scaled+
                        # WC_P_scaled+
                        # WPS_D_scaled+
                        # WPS_P_scaled+
                        # Sixltr_D_scaled+
                        # Sixltr_P_scaled+
                        # affiliation_D_scaled+
                        # affiliation_P_scaled+
                        # i_D_scaled+
                        # i_P_scaled+
                        # Clout_D_scaled+
                        # Clout_P_scaled+
                        # differ_D_scaled+
                        # differ_P_scaled+
                        # insight_D_scaled+
                        # insight_P_scaled+
                        # cause_D_scaled+
                        # cause_P_scaled+
                        # provider_style_sd+
                        # LSM_function_mean*racecat2,
                        # (1|provider_id), 
                  family = 'binomial', data = H4_df)
anova(m.1_H4_HLM,m.2_H4_HLM)
summary(m.2_H4_HLM)

sjPlot::tab_model(m.2_H4_HLM)



