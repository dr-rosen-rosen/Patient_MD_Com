library(lme4)
library(jtools)
library(sjPlot)
library(lmerTest)
library(tidyverse)


#open file
ECHO_LSM_MLM <- read_csv(here(config$ECHO_LSM_MLM_path, config$ECHO_LSM_MLM_name))


# Hypothesis one: 
# Ha: LSM will be [lower with racial/ethnic minority patients], 
# and [higher within dyads characterized by race concordance], 
# and [those who rate each other as culturally similar].
# LSM is the DV (what we're tyring to predict... the 'outcome')


# step 1: Null model
m.0_ha <- lm(LSM_function_mean ~ 1, data = ECHO_LSM_MLM, na.action=na.omit)
summary(m.0_ha)
summ(m.0_ha)

# step 2: add the culstering variable --> "provider_id"
m.1_ha <- lmer(LSM_function_mean ~ 1 + (1|provider_id), 
               data = ECHO_LSM_MLM, na.action=na.omit)

# test for fit of grouping structure
anova(m.1_ha,m.0_ha)
summ(m.1_ha)
summary(m.1_ha)

# step 3: add the predictors
# cultdissmd, cultdissmdtert, cultdiss,cultdisstert, 
#racecat2, cultcomp
m.2_ha <- lm(LSM_function_mean ~ 
               cultdissmd +
               cultdiss, data = ECHO_LSM_MLM, na.action=na.omit )
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

# step 1: Null model
m.0_Hc1 <- lm(provassess~ 1, data = ECHO_LSM_MLM, na.action=na.omit)

# step 2: add the culstering variable.. MD
m.1_Hc1 <- lmer(provassess ~ 1 + (1|provider_id), data = ECHO_LSM_MLM, na.action=na.omit )
# test for fit of grouping structure
anova(m.0_Hc1,m.1_Hc1)

# step 3: add the predictors
m.2_Hc1 <- lmer(provassess ~ 
                  LSM_function_mean +
                  (1|provider_id), data = ECHO_LSM_MLM, na.action=na.omit )
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
