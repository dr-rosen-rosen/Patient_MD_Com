#############
#### Blackbox Scripts for regression analysis

#packages
library(lme4)
library(jtools)
library(sjPlot)
library(performance)




#Aim 2.1: Associations of Veteran race and provider CC with LSM: Use primary racial/ethnic identity; 3 CC sub-scales and overall composite
#PCP rating of own CC: pcp_cc_score
#patient race (1=white, 2=black, 3=hispanic/latino, 4=other): p_mainrace


m.0_H2.1a_HLM <- lm(LSM_function_mean.scaled~ 1,  data = H2.1_df)

m.1_H2.1a_HLM <- lmer(LSM_function_mean.scaled~ 1 + (1|prov_id),  data = H2.1_df)

predictors <- c('p_mainrace', 'pcp_cc_score')
m.1_H2.1a_HLM<- update(m.1_H2.1a_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.1_H2.1a_HLM)

anova(m.1_H2.1a_HLM, m.0_H2.1a_HLM)
anova(m.1_H2.1a_HLM)

###########################################################
#Aim 2.2a: Associations of LSM with the quality of patient-provider relationships (pt_cahps_1_20_score)


#rating of pt_cahps_1_20_score


m.2.2a_HLM <- glm(pt_cahps_1_20_score_high~ 1 , family = binomial,  data = H2.2a_df)

#'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 'p_mainrace'
predictors <- c('rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 'p_mainrace')
m.2.2a_HLM  <- update(m.2.2a_HLM , paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.2.2a_HLM)



###########################################################
#Aim 2.2b: Associations of LSM with the quality of patient-provider relationships, as rated by patients: patient perceived PCP CC

m.2.2b_HLM <- glm(pt_cc_score_high~ 1 , family = binomial,  data = H2.2b_df)

#'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 'p_mainrace'
predictors <- c('rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 'p_mainrace')
m.2.2b_HLM  <- update(m.2.2b_HLM , paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.2.2b_HLM)

###########################################################
#Aim2.2c: Associations of LSM with the quality of patient-provider relationships, as rated by patients: trust in PCP


#pt rating of overall trust in provider

m.2.2c_HLM <- glm(pt_trust_score_high~ 1 , family = binomial,  data = H2.2c_df)

#'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled'
predictors <- c('rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 'p_mainrace')
m.2.2c_HLM  <- update(m.2.2c_HLM , paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.2.2c_HLM)

###########################################################
#Aim 2.2d: Associations of LSM with the quality of patient-provider relationships, as rated by patients: perceived respect

m.2.2d_HLM <- glm(pt_respect_score_high~ 1 , family = binomial,   data = H2.2d_df)

#'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled'
predictors <- c('LSM_function_mean.scaled')
m.2.2d_HLM  <- update(m.2.2d_HLM , paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.2.2d_HLM)



