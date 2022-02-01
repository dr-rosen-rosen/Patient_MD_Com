library(lme4)
library(jtools)
library(sjPlot)
library(lmerTest)
library(tidyverse)


#open file
ECHO_LSM_MLM <- read_csv(here(config$ECHO_LSM_MLM_path, config$ECHO_LSM_MLM_name))

#making a race concordance variable "raceconc" between patient and provider race
ECHO_LSM_MLM <- ECHO_LSM_MLM %>%
  rowwise() %>%
  mutate(raceconc = if_else(racecat2== provrace, 1, 0))

# centering the lsm score with scale
# ECHO_LSM_MLM$LSM_function_mean <-scale(ECHO_LSM_MLM$LSM_function_mean, center= TRUE, scale = TRUE)

# To compare models w/ ANOVA, you need the same size data. So, you can't
# just use na.omit for each model (if there are different missingness for different variables)
# you need to use 'complete cases' for all the variables you intend to use.
# You should center AFTER you drop for missingness. 

# Get complete cases

Ha_df <- ECHO_LSM_MLM %>%
  # comiposite cult diss measures
  # select(LSM_function_mean, provider_id, cultdissmd, cultdiss, racecat2, raceconc) %>%
  # subsacle measures
  select(LSM_function_mean, provider_id, racecat2, raceconc, cdspeak,cdreason,cdstyle,cdvalue,cdspirit,cdethnic,cdtype,cdrace,cdculture,cdskin) %>%
  tidyr::drop_na() %>% # take only rows with no NA; 
  # mutate_at(c('LSM_function_mean','cultdissmd', 'cultdiss'), ~scale(.,center = TRUE, scale = TRUE)) %>%
  mutate(cdspeak_dichoto = if_else(cdspeak < median(cdspeak),1,0)) %>%
  mutate(across(!provider_id & !racecat2 & !raceconc & !cdspeak_dichoto, ~scale(.,center = TRUE, scale = TRUE))) %>%
  mutate(provider_id = factor(provider_id)) %>%
  mutate(racecat2 = factor(racecat2)) %>%
  mutate(raceconc = factor(raceconc))
nrow(Ha_df)
hist(Ha_df$cdspeak_dichoto)
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
               #cultdissmd +
               #cultdiss+
               # factor(cdspeak_dichoto) +
               cdspeak +
               # cdreason +
               # cdstyle +
               # cdvalue +
               # cdspirit +
               # cdethnic +
               # cdtype +
               # cdrace +
               # cdculture +
               # cdskin +
               racecat2 +
               raceconc, data = Ha_df)
anova(m.1_ha,m.2_ha)
summary(m.2_ha)
summ(m.2_ha)
sjPlot::tab_model(m.0_ha,m.1_ha,m.2_ha)


##################################################################
##################################################################
##################################################################
# Hypothesis three
# "outcomes" are pt ratings of clinicians and viral load suppression (two different models)
# Hc: Greater LSM and clinician linguistic accommodation will predict [higher patient ratings of clinicians] 
# and [greater viral load suppression]. 

#Hypothesis 3a- DV: patient ratings of clinicians...is it this variable? "provassess"

Hc_df <- ECHO_LSM_MLM %>%
  select(LSM_function_mean, provider_id, provassess, racecat2, raceconc) %>%
  tidyr::drop_na() %>% # take only rows with no NA; 
  mutate_at(c('LSM_function_mean','provassess'), ~scale(.,center = TRUE, scale = TRUE)) %>%
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

sjPlot::tab_model(m.0_Hc1,m.1_Hc1,m.2_Hc1)




#Hypothesis 3b- DV: viral load suppression
# step 1: Null model
m.0_Hc2 <- glm(vlsup75~ 1, data = ECHO_LSM_MLM)

# step 2: add the culstering variable.. MD
m.1_Hc2 <- glmer(vlsup75 ~ 1 + (1|provider_id), data = ECHO_LSM_MLM, na.action=na.omit )
# test for fit of grouping structure
anova(m.0_Hc2,m.1_Hc2)

# step 3: add the predictors
m.2_Hc2 <- lmer(vlsup75 ~ 
                  LSM_function_mean +
                  (1|provider_id), data = ECHO_LSM_MLM, na.action=na.omit )
anova(m.1_Hc2,m.2_Hc2)
summary(m.2_Hc2)
sjPlot::tab_model(m.0_Hc2,m.1_Hc2,m.2_Hc2)
summary(m.1_Hc2)

##################################################################
##################################################################
##################################################################

# hypothesis 2 (skip for now)
# Hb: Clinicians with higher cultural competence will be more linguistically accommodating. 






##################################################################
##################################################################
##################################################################
#### NOTES ON NEW MLMs

# Hypothesis one
# LSM is the IV (what we're tyring to predict... the 'outcome')

# step 1: Null model
m.0 <- lm(LSM ~ 1, ...)
# step 2: add the culstering variable.. MD
m.1 <- lmer(LSM ~ 1 + (1|provider_id), ... )
# test for fit of grouping structure
anova(m.0,m.1)

# step 3: add the predictors
m.2 <- lmer(LSM ~ 
              racial_ethnic +
              cultural_sim +
              cultural_comp +
              (1|provider_id), ... )
anova(m.1,m.2)
summary(m.2)


######################
# Hypothesis three
# "outcomes" are pt ratings of clinicians and viral load suppression (two different models)

# step 1: Null model
m.0 <- lm(PT_Ratings~ 1, ...)
# step 2: add the culstering variable.. MD
m.1 <- lmer(PT_Ratings ~ 1 + (1|provider_id), ... )
# test for fit of grouping structure
anova(m.0,m.1)

# step 3: add the predictors
m.2 <- lmer(PT_Ratings ~ 
              LSM +
              (1|provider_id), ... )
anova(m.1,m.2)
summary(m.2)
sjPlot::tab_model(m.0,m.1,m.2)
