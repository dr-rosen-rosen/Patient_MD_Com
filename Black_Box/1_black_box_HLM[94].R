####################################################
#### Blackbox Scripts for regression analysis

#packages
library(lme4)
library(jtools)
library(sjPlot)
library(performance)




################################################################################################
#Aim 2.1: Associations of Veteran race and provider CC with LSM: Use primary racial/ethnic identity; 3 CC sub-scales and overall composite
#PCP rating of own CC: pcp_cc_score
#patient race (1=white, 2=black, 3=hispanic/latino, 4=other): p_mainrace
################################################################################################

# 'LSM_function_mean.scaled', 'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 
# 'accom_20_seg_patient.scaled', 'accom_20_seg_pcp.scaled'
m.0_H2.1a_HLM <- lm(
                    LSM_function_mean.scaled
                    # rw.rLSM.pcp.scaled
                    # accom_20_seg_pcp.scaled
                    # accom_20_seg_pcp
                      # accom_10_seg_pcp.scaled
                    # accom_raw_pcp.scaled
                    # accom_raw_pcp_chng.scaled
                    ~ 1,  data = H2.1_df)

m.1_H2.1a_HLM <- lmer(
                      LSM_function_mean.scaled
                       # rw.rLSM.pcp.scaled
                      # accom_20_seg_pcp.scaled
                      # accom_20_seg_pcp
                        # accom_10_seg_pcp.scaled
                      # accom_raw_pcp.scaled
                      
                      # accom_raw_pcp_chng.scaled
                      ~ 1 + (1|prov_id),  data = H2.1_df)


#checking to see if it's a valid nesting structure
anova(m.1_H2.1a_HLM, m.0_H2.1a_HLM)



m.2_H2.1a_HLM <- lmer(
                      LSM_function_mean.scaled
                       # rw.rLSM.pcp.scaled
                      #rw.rLSM.patient.scaled
                      # accom_10_seg_pcp.scaled
                      # accom_20_seg_pcp.scaled
                      # accom_20_seg_pcp
                      # accom_raw_pcp
                      # accom_raw_pcp_dicho
                      # accom_raw_pcp.scaled
  
                      #accom_20_seg_patient.scaled
                      
                      # accom_raw_pcp_chng.scaled
                      ~ 1 + (1|prov_id),  data = H2.1_df)

predictors <- c('p_mainrace')
m.2_H2.1a_HLM<- update(m.2_H2.1a_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.2_H2.1a_HLM)




m.3_H2.1a_HLM <- lmer(
                       LSM_function_mean.scaled
                      # rw.rLSM.pcp.scaled
                      #rw.rLSM.patient.scaled
  # accom_10_seg_pcp.scaled
                      # accom_20_seg_pcp#.scaled
                      #accom_20_seg_patient.scaled
  # accom_raw_pcp
  
  # accom_raw_pcp_chng.scaled
                      ~ 1 + (1|prov_id),  data = H2.1_df)
#pcp_cc_vdp_score, pcp_cc_score
predictors <- c('pcp_cc_score_high')
m.3_H2.1a_HLM<- update(m.3_H2.1a_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.3_H2.1a_HLM)


m.4_H2.1a_HLM <- lmer(
                       LSM_function_mean.scaled
                      # rw.rLSM.pcp.scaled
                      #rw.rLSM.patient.scaled
  # accom_10_seg_pcp.scaled
                       #accom_20_seg_pcp.scaled
                      #accom_20_seg_patient.scaled
                      ~ 1 + (1|prov_id),  data = H2.1_df)

predictors <- c('p_mainrace', 'pcp_cc_score_high')
m.4_H2.1a_HLM<- update(m.4_H2.1a_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.4_H2.1a_HLM)


m.5_H2.1a_HLM <- lmer(
                      # LSM_function_mean.scaled
                      # rw.rLSM.pcp.scaled
                      #rw.rLSM.patient.scaled
  accom_10_seg_pcp.scaled
                       #accom_20_seg_pcp.scaled
                      #accom_20_seg_patient.scaled
                      ~ 1 + (1|prov_id),  data = H2.1_df)

predictors <- c('p_mainrace', 'pcp_cc_vdp_score_high', 'p_mainrace*pcp_cc_vdp_score_high')#,'pt_cc_score_high','pt_cc_score_high*p_mainrace')
m.5_H2.1a_HLM<- update(m.5_H2.1a_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.5_H2.1a_HLM)
sjPlot::plot_model(m.5_H2.1a_HLM, type = 'int')


sjPlot::tab_model(m.2_H2.1a_HLM, m.3_H2.1a_HLM, m.4_H2.1a_HLM, m.5_H2.1a_HLM)



###########################################################################################################
#Aim 2.2a: Associations of LSM with the quality of patient-provider relationships (pt_cahps_1_20_score)
###########################################################################################################


#rating of pt_cahps_1_20_score
m.0.2a_HLM <- glm(pt_cahps_1_20_score_high~ 1 , family = binomial,  data = H2.2a_df)

m.1.2a_HLM <- glmer(pt_cahps_1_20_score_high~ 1 + (1|prov_id), family = binomial,  data = H2.2a_df)

#checking to see if it's a valid nesting structure
anova(m.1.2a_HLM, m.0.2a_HLM)



m.2.2a_HLM <- glmer(pt_cahps_1_20_score_high~ 1 + (1|prov_id), family = binomial,  data = H2.2a_df)

# 'LSM_function_mean.scaled', 'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 
# 'accom_20_seg_patient.scaled', 'accom_20_seg_pcp.scaled'
predictors <- c('LSM_function_mean.scaled',  
                'rw.rLSM.pcp.scaled', 
                # 'rw.rLSM.patient.scaled', 
                'accom_10_seg_pcp.scaled'
                # 'rw.rLSM.pcp.scaled*accom_10_seg_pcp.scaled'
                # 'accom_10_seg_patient.scaled'
                )
m.2.2a_HLM  <- update(m.2.2a_HLM , paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.2.2a_HLM)


m.3.2a_HLM <- glmer(pt_cahps_1_20_score_high ~ 1 + (1|prov_id), family = binomial,data = H2.2a_df)

# 'LSM_function_mean.scaled', 'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 
# 'accom_20_seg_patient.scaled', 'accom_20_seg_pcp.scaled'
predictors <- c(
                'LSM_function_mean.scaled',
                'rw.rLSM.pcp.scaled',
                'accom_10_seg_pcp.scaled', 
                'p_mainrace' 
                )
m.3.2a_HLM   <- update(m.3.2a_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.3.2a_HLM)
# sjPlot::plot_model(m.3.2a_HLM, type = 'int')



m.4.2a_HLM <- glmer(pt_cahps_1_20_score_high~ 1 + (1|prov_id), family = binomial,  data = H2.2a_df)

# 'LSM_function_mean.scaled', 'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 
# 'accom_20_seg_patient.scaled', 'accom_20_seg_pcp.scaled'
predictors <- c('LSM_function_mean.scaled',
                'rw.rLSM.pcp.scaled',
                'accom_10_seg_pcp.scaled', 
                # 'accom_10_seg_pcp.scaled*rw.rLSM.pcp.scaled', 
                'p_mainrace',
                'p_mainrace*LSM_function_mean.scaled',
                'p_mainrace*rw.rLSM.pcp.scaled',
                'p_mainrace*accom_10_seg_pcp.scaled'
                )
                #'accom_20_seg_patient.scaled*p_mainrace'
m.4.2a_HLM   <- update(m.4.2a_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.4.2a_HLM)




sjPlot::tab_model(m.0.2a_HLM, m.1.2a_HLM, m.2.2a_HLM, m.3.2a_HLM, m.4.2a_HLM)



############################################################################################################################
#Aim 2.2b: Associations of LSM with the quality of patient-provider relationships, as rated by patients: patient perceived PCP CC
############################################################################################################################

m.0.2b_HLM <- glm(pt_cc_score_high~ 1, family = binomial,  data = H2.2b_df)


m.1.2b_HLM <- glmer(pt_cc_score_high~ 1 + (1|prov_id), family = binomial,  data = H2.2b_df)

#checking to see if it's a valid nesting structure
anova(m.1.2b_HLM, m.0.2b_HLM)

m.2.2b_HLM <- glmer(pt_cc_score_high~ 1 + (1|prov_id), family = binomial,  data = H2.2b_df, control = glmerControl(optimizer="bobyqa"))

#'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 'p_mainrace'
predictors <- c('LSM_function_mean.scaled',  
                'rw.rLSM.pcp.scaled', 
                # 'rw.rLSM.patient.scaled',
                'accom_10_seg_pcp.scaled' 
                # 'accom_10_seg_patient.scaled',
                # 'p_mainrace'
                )
m.2.2b_HLM  <- update(m.2.2b_HLM , paste(".~. +",paste(predictors,collapse = ' + ')))
# lme4::allFit(m.2.2b_HLM)
sjPlot::tab_model(m.2.2b_HLM)



m.3.2b_HLM <- glmer(pt_cc_score_high~ 1 + (1|prov_id), family = binomial,  data = H2.2b_df)

# 'LSM_function_mean.scaled', 'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 
# 'accom_20_seg_patient.scaled', 'accom_20_seg_pcp.scaled'
predictors <- c(
  'LSM_function_mean.scaled',  
  'rw.rLSM.pcp.scaled', 
  'accom_10_seg_pcp.scaled', 
  'p_mainrace')
m.3.2b_HLM   <- update(m.3.2b_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.3.2b_HLM)


m.4.2b_HLM <- glmer(pt_cc_score_high~ 1 + (1|prov_id), family = binomial,  data = H2.2b_df, control = glmerControl(optimizer="bobyqa"))

# 'LSM_function_mean.scaled', 'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 
# 'accom_20_seg_patient.scaled', 'accom_20_seg_pcp.scaled'
predictors <- c('LSM_function_mean.scaled',
                'rw.rLSM.pcp.scaled',
                'accom_10_seg_pcp.scaled', 
                # 'accom_10_seg_pcp.scaled*rw.rLSM.pcp.scaled', 
                'p_mainrace',
                'p_mainrace*LSM_function_mean.scaled',
                'p_mainrace*rw.rLSM.pcp.scaled',
                'p_mainrace*accom_10_seg_pcp.scaled'
  )
m.4.2b_HLM   <- update(m.4.2b_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.4.2b_HLM)
sjPlot::plot_model(m.4.2b_HLM, type = 'int')

sjPlot::plot_model(m.4.2b_HLM, type = 'int',terms="rw.rLSM.pcp.scaled [all]")


sjPlot::tab_model(m.0.2b_HLM, m.1.2b_HLM, m.2.2b_HLM, m.3.2b_HLM, m.4.2b_HLM)



performance::check_model(m.4.2b_HLM)



###################################################################################################################
#Aim2.2c: Associations of LSM with the quality of patient-provider relationships, as rated by patients: trust in PCP
###################################################################################################################

#pt rating of overall trust in provider


m.0.2c_HLM <- glm(pt_trust_score_high~ 1, family = binomial,  data = H2.2c_df)


m.1.2c_HLM <- glmer(pt_trust_score_high~ 1 + (1|prov_id), family = binomial,  data = H2.2c_df)

#checking to see if it's a valid nesting structure
anova(m.1.2c_HLM, m.0.2c_HLM)





m.2.2c_HLM <- glmer(pt_trust_score_high~ 1 + (1|prov_id), family = binomial,  data = H2.2c_df)

#'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled'
predictors <- c('LSM_function_mean.scaled',  'rw.rLSM.pcp.scaled', 'rw.rLSM.patient.scaled', 
                'accom_20_seg_pcp.scaled', 'accom_20_seg_patient.scaled')
m.2.2c_HLM  <- update(m.2.2c_HLM , paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.2.2c_HLM)


m.3.2c_HLM <- glmer(pt_trust_score_high~ 1 + (1|prov_id), family = binomial,  data = H2.2c_df)

# 'LSM_function_mean.scaled', 'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 
# 'accom_20_seg_patient.scaled', 'accom_20_seg_pcp.scaled'
predictors <- c('LSM_function_mean.scaled',  'rw.rLSM.pcp.scaled', 'rw.rLSM.patient.scaled', 
                'accom_20_seg_pcp.scaled', 'accom_20_seg_patient.scaled', 'p_mainrace')
m.3.2c_HLM   <- update(m.3.2c_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.3.2c_HLM)


m.4.2c_HLM <- glm(pt_trust_score_high~ 1, family = binomial,  data = H2.2c_df)

# 'LSM_function_mean.scaled', 'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 
# 'accom_20_seg_patient.scaled', 'accom_20_seg_pcp.scaled'
predictors <- c('LSM_function_mean.scaled',  
                'rw.rLSM.pcp.scaled', 
                # 'rw.rLSM.patient.scaled',
                'accom_10_seg_pcp.scaled', 
                # 'accom_10_seg_patient.scaled', 
                'p_mainrace',
                'LSM_function_mean.scaled*p_mainrace', 
                'rw.rLSM.pcp.scaled*p_mainrace', 
                # 'rw.rLSM.patient.scaled*p_mainrace', 
                'accom_10_seg_pcp.scaled*p_mainrace'#,
                # 'accom_10_seg_pcp.scaled*rw.rLSM.pcp.scaled'
                )
                # 'accom_10_seg_patient.scaled*p_mainrace')
m.4.2c_HLM   <- update(m.4.2c_HLM, paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.4.2c_HLM)


sjPlot::tab_model(m.0.2c_HLM, m.1.2c_HLM, m.2.2c_HLM, m.3.2c_HLM, m.4.2c_HLM )





######################################################################################################################
#Aim 2.2d: Associations of LSM with the quality of patient-provider relationships, as rated by patients: perceived respect
######################################################################################################################

m.0.2d_HLM <- glm(pt_respect_score_high~ 1, family = binomial,  data = H2.2d_df)


m.1.2d_HLM <- glmer(pt_respect_score_high~ 1 + (1|prov_id), family = binomial,  data = H2.2d_df)

#checking to see if it's a valid nesting structure
anova(m.1.2d_HLM, m.0.2d_HLM)




m.2.2d_HLM <- glmer(pt_respect_score_high~ 1 + (1|prov_id), family = binomial,   data = H2.2d_df)

#'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled'
predictors <- c('LSM_function_mean.scaled',  'rw.rLSM.pcp.scaled', 'rw.rLSM.patient.scaled', 
                'accom_20_seg_pcp.scaled', 'accom_20_seg_patient.scaled')
m.2.2d_HLM  <- update(m.2.2d_HLM , paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.2.2d_HLM)




m.3.2d_HLM <- glmer(pt_respect_score_high~ 1 + (1|prov_id), family = binomial,  data = H2.2d_df)

# 'LSM_function_mean.scaled', 'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 
# 'accom_20_seg_patient.scaled', 'accom_20_seg_pcp.scaled'
predictors <- c('LSM_function_mean.scaled',  'rw.rLSM.pcp.scaled', 'rw.rLSM.patient.scaled', 
                'accom_20_seg_pcp.scaled', 'accom_20_seg_patient.scaled', 'p_mainrace')
m.3.2d_HLM <- update(m.3.2d_HLM , paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.3.2d_HLM)


m.4.2d_HLM <- glmer(pt_respect_score_high~ 1 + (1|prov_id), family = binomial,  data = H2.2d_df)

# 'LSM_function_mean.scaled', 'rw.rLSM.patient.scaled', 'rw.rLSM.pcp.scaled', 
# 'accom_20_seg_patient.scaled', 'accom_20_seg_pcp.scaled'
predictors <- c(
  'LSM_function_mean.scaled',  
  'rw.rLSM.pcp.scaled', 
  # 'rw.rLSM.patient.scaled',
  'accom_10_seg_pcp.scaled', 
  # 'accom_10_seg_patient.scaled', 
  'p_mainrace',
  'LSM_function_mean.scaled*p_mainrace', 
  'rw.rLSM.pcp.scaled*p_mainrace', 
  # 'rw.rLSM.patient.scaled*p_mainrace', 
  'accom_10_seg_pcp.scaled*p_mainrace',
  'accom_10_seg_pcp.scaled*rw.rLSM.pcp.scaled'
  # 'accom_10_seg_patient.scaled*p_mainrace'
                )
m.4.2d_HLM <- update(m.4.2d_HLM , paste(".~. +",paste(predictors,collapse = ' + ')))
sjPlot::tab_model(m.4.2d_HLM)

sjPlot::tab_model(m.0.2d_HLM, m.1.2d_HLM, m.2.2d_HLM, m.3.2d_HLM, m.4.2d_HLM )



################


