#############
#### ECHO Scripts for HLM

#packages
library(lme4)
library(jtools)
library(sjPlot)

# model H1.1
############

# step 1: Null model
m.0_H1.1_HLM <- lm(LSM_function_mean ~ 1, data = H1.1_df)
summary(m.0_H1.1_HLM)
summ(m.0_H1.1_HLM)

# step 2: add the culstering variable --> "provider_id"
m.1_H1.1_HLM <- lmer(LSM_function_mean ~ 1 + (1|provider_id), 
               data = H1.1_df)

# test for fit of grouping structure
anova(m.1_H1.1_HLM,m.0_H1.1_HLM)
summ(m.1_H1.1_HLM)
summary(m.1_H1.1_HLM)

# step 3: add the predictors, racecat2
m.2_H1.1_HLM <- lm(LSM_function_mean ~ 
             racecat2, data = H1.1_df)
anova(m.1_H1.1_HLM,m.2_H1.1_HLM)
summary(m.2_H1.1_HLM)
summ(m.2_H1.1_HLM)
plot(m.2_H1.1_HLM)
sjPlot::tab_model(m.2_H1.1_HLM)


# model H1.2
############

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

# model H1.3
############


m.0_H1.3_HLM <- lm(LSM_function_mean ~ 1, data = H1.3_df)
summary(m.0_H1.3_HLM)
summ(m.0_H1.3_HLM)

# step 2: add the culstering variable --> "provider_id"
m.1_H1.3_HLM <- lmer(LSM_function_mean ~ 1 + (1|provider_id), 
                        data = H1.3_df)

# test for fit of grouping structure
anova(m.1_H1.3_HLM,m.0_H1.3_HLM)
summ(m.1_H1.3_HLM)
summary(m.1_H1.3_HLM)

# step 3: add the predictors, cultdiss, cultdissmd
m.2_H1.3_HLM <- lm(LSM_function_mean ~ 
                     cultdiss +
                     cultdissmd, data = H1.3_df)
anova(m.1_H1.3_HLM,m.2_H1.3_HLM)
summary(m.2_H1.3_HLM)
summ(m.2_H1.3_HLM)
plot(m.2_H1.3_HLM)
sjPlot::tab_model(m.2_H1.3_HLM)




# model H3a.1
############

# step 1: Null model
m.0_H3a.1_HLM <- lm(provcomm~ 1, data = H3a.1_df)

# step 2: add the culstering variable.. MD
m.1_H3a.1_HLM <- lmer(provcomm ~ 1 + (1|provider_id), data = H3a.1_df)
# test for fit of grouping structure
anova(m.1_H3a.1_HLM,m.0_H3a.1_HLM)

# step 3: add the predictors
m.2_H3a.1_HLM <- lmer(provcomm ~ 
                  LSM_function_mean, 
                  (1|provider_id), data = H3a.1_df)
anova(m.1_H3a.1_HLM,m.2_H3a.1_HLM)
summary(m.2_H3a.1_HLM)

sjPlot::tab_model(m.2_H3a.1_HLM)

# model H3a.2
############

# step 1: Null model
m.0_H3a.2_HLM <- lm(overcomm~ 1, data = H3a.2_df)

# step 2: add the culstering variable.. MD
m.1_H3a.2_HLM <- lmer(overcomm ~ 1 + (1|provider_id), data = H3a.2_df)
# test for fit of grouping structure
anova(m.1_H3a.2_HLM,m.0_H3a.2_HLM)

# step 3: add the predictors
m.2_H3a.2_HLM <- lmer(overcomm ~ 
                        LSM_function_mean, 
                      (1|provider_id), data = H3a.2_df)
anova(m.1_H3a.2_HLM,m.2_H3a.2_HLM)
summary(m.2_H3a.2_HLM)

sjPlot::tab_model(m.2_H3a.2_HLM)

# model H3a.3
############
# step 1: Null model
m.0_H3a.3_HLM <- lm(ipstyle~ 1, data = H3a.3_df)

# step 2: add the culstering variable.. MD
m.1_H3a.3_HLM <- lmer(ipstyle ~ 1 + (1|provider_id), data = H3a.3_df)
# test for fit of grouping structure
anova(m.1_H3a.3_HLM,m.0_H3a.3_HLM)

# step 3: add the predictors
m.2_H3a.3_HLM <- lmer(ipstyle ~ 
                        LSM_function_mean, 
                      (1|provider_id), data = H3a.3_df)
anova(m.1_H3a.3_HLM,m.2_H3a.3_HLM)
summary(m.2_H3a.3_HLM)

sjPlot::tab_model(m.2_H3a.3_HLM)

# model H3a.4
############

# step 1: Null model
m.0_H3a.4_HLM <- lm(iptrust~ 1, data = H3a.4_df)

# step 2: add the culstering variable.. MD
m.1_H3a.4_HLM <- lmer(iptrust ~ 1 + (1|provider_id), data = H3a.4_df)
# test for fit of grouping structure
anova(m.1_H3a.4_HLM,m.0_H3a.4_HLM)

# step 3: add the predictors
m.2_H3a.4_HLM <- lmer(iptrust ~ 
                        LSM_function_mean, 
                      (1|provider_id), data = H3a.4_df)
anova(m.1_H3a.4_HLM,m.2_H3a.4_HLM)
summary(m.2_H3a.4_HLM)

sjPlot::tab_model(m.2_H3a.4_HLM)

# model H3a.5
############

# step 1: Null model
m.0_H3a.5_HLM <- glm(provknowcat~ 1, data = H3a.5_df)

# step 2: add the culstering variable.. MD
m.1_H3a.5_HLM <- glmer(provknowcat ~ 1 + (1|provider_id), family = 'binomial',data = H3a.5_df)
# test for fit of grouping structure
anova(m.1_H3a.5_HLM,m.0_H3a.5_HLM)

# step 3: add the predictors
m.2_H3a.5_HLM <- glmer(provknowcat ~ 
                        LSM_function_mean, 
                      (1|provider_id), family = 'binomial', data = H3a.5_df)
anova(m.1_H3a.5_HLM,m.2_H3a.5_HLM)
summary(m.2_H3a.5_HLM)

sjPlot::tab_model(m.2_H3a.5_HLM)

# model H3a.6
############


# model H3b.1
############





###################################################################
###################################################################
###################################################################
###################################################################



# Hypothesis one: 
# Ha: LSM will be [lower with racial/ethnic minority patients], 
# and [higher within dyads characterized by race concordance], 
# and [those who rate each other as culturally similar].
# LSM is the DV (what we're tyring to predict... the 'outcome')


# step 1: Null model
m.0_ha <- lm(LSM_function_mean ~ 1, data = Ha_df)
summary(m.0_ha)
summ(m.0_ha)

# step 2: add the culstering variable --> "provider_id"
m.1_ha <- lmer(LSM_function_mean ~ 1 + (1|provider_id), 
               data = Ha_df)

# test for fit of grouping structure
anova(m.1_ha,m.0_ha)
summ(m.1_ha)
summary(m.1_ha)

# step 3: add the predictors
# cultdissmd, cultdissmdtert, cultdiss,cultdisstert, 
#racecat2, raceconc
m.2_ha <- lm(LSM_function_mean ~ 
               cultdissmd +
               cultdiss+
               cdAvg_dist +
               # factor(cdspeak_dichoto) +
               cdspeak_dist +
               # cdreason_dist +
               # cdstyle +
               # cultdissmd3 + 
               cdstyle_dist +
               # cdvalue_dist +
               # cdspirit_dist +
               # cdethnic_dist +
               # cdtype_dist +
               # cdrace_dist +
               # cdculture_dist +
               # cdskin_dist +
               # cultdissmd1 +
               # cultdissmd2 +
               # cultdissmd3 + 
               # cultdissmd4 +
             # cultdissmd5 +
             # cultdissmd6 +
             # cultdissmd7 +
             # cultdissmd8 +
             # cultdissmd9 +
             # cultdissmd10 +
             racecat2 +
               raceconc, data = Ha_df)
anova(m.1_ha,m.2_ha)
summary(m.2_ha)
summ(m.2_ha)
plot(m.2_ha)
sjPlot::tab_model(m.2_ha)


##################################################################
##################################################################
##################################################################
# Hypothesis three
# "outcomes" are pt ratings of clinicians and viral load suppression (two different models)
# Hc: Greater LSM and clinician linguistic accommodation will predict [higher patient ratings of clinicians] 
# and [greater viral load suppression]. 

#Hypothesis 3a- DV: patient ratings of clinicians...is it this variable? "provassess"

Hc_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, provassess, racecat2, raceconc) %>%
  tidyr::drop_na() %>% # take only rows with no NA; 
  # mutate_at(c('LSM_function_mean','provassess'), ~scale(.,center = TRUE, scale = TRUE)) %>%
  mutate(racecat2 = factor(racecat2)) %>%
  mutate(raceconc = factor(raceconc))

# step 1: Null model
m.0_Hc1 <- lm(provassess~ 1, data = Hc_df)

# step 2: add the culstering variable.. MD
m.1_Hc1 <- lmer(provassess ~ 1 + (1|provider_id), data = Hc_df)
# test for fit of grouping structure
anova(m.1_Hc1,m.0_Hc1)

# step 3: add the predictors
m.2_Hc1 <- lmer(provassess ~ 
                  LSM_function_mean +
                  # racecat2 +
                  # raceconc +
                  (1|provider_id), data = Hc_df)
anova(m.1_Hc1,m.2_Hc1)
summary(m.2_Hc1)

sjPlot::tab_model(m.2_Hc1, show.aic = TRUE)

#Hypothesis 3b- DV: viral load suppression

Hb_vl_df <- ECHO_LSM_MLM %>%
  dplyr::select(vlsup75, LSM_function_mean, provider_id) %>%
  tidyr::drop_na()

# step 1: Null model
m.0_Hc2 <- glm(vlsup75~ 1, data = ECHO_LSM_MLM)

# step 2: add the culstering variable.. MD
m.1_Hc2 <- glmer(factor(vlsup75) ~ 1 + (1|provider_id), family = 'binomial', data = ECHO_LSM_MLM, na.action=na.omit )
# test for fit of grouping structure
anova(m.0_Hc2,m.1_Hc2)
summary(m.1_Hc2)
# step 3: add the predictors
m.2_Hc2 <- glmer(factor(vlsup75) ~ 
                   LSM_function_mean +
                   (1|provider_id),
                 family = 'binomial',
                 data = ECHO_LSM_MLM, na.action=na.omit)
anova(m.1_Hc2,m.2_Hc2)
summary(m.2_Hc2)
sjPlot::tab_model(m.2_Hc2, show.aic = TRUE)
summary(m.1_Hc2)

ECHO_LSM_MLM %>%
  ggplot(aes(y = LSM_function_mean, fill = factor(vlsup75))) + geom_boxplot()