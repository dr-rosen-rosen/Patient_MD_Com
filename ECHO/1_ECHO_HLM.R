#############
#### ECHO Scripts for HLM

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