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
m.2_H1.1_HLM <- lmer(LSM_function_mean ~ 
             racecat2+ 
             (1|provider_id) , data = H1.1_df)
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
m.2_H1.2_HLM <- lmer(LSM_function_mean ~ 
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
m.1_H1.3_HLM <- lme4::lmer(LSM_function_mean ~ 1 + (1|provider_id), 
                        data = H1.3_df)

# test for fit of grouping structure
anova(m.1_H1.3_HLM,m.0_H1.3_HLM)
summ(m.1_H1.3_HLM)
summary(m.1_H1.3_HLM)

# step 3: add the predictors, cultdiss, cultdissmd
m.2_H1.3_HLM <- lmer(LSM_function_mean ~ 
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
                     # cdskin_dist_abs# +
                     cdAvg_dist +
                     cdspeak_dist +
                     cdreason_dist +
                     cdstyle_dist +
                     cdvalue_dist +
                     cdspirit_dist +
                     cdethnic_dist +
                     cdtype_dist +
                     cdrace_dist +
                     cdculture_dist+
                     cdskin_dist+
                     (1|provider_id),
                   data = H1.3_df)

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
m.1_H3a.1_HLM <- lme4::lmer(provcomm ~ 1 + (1|provider_id), data = H3a.1_df)
# test for fit of grouping structure
anova(m.1_H3a.1_HLM,m.0_H3a.1_HLM)

# step 3: add the predictors
m.2_H3a.1_HLM <- lme4::lmer(provcomm ~ 
                  LSM_function_mean+
                    LSM_function_mean + 
                    WC_D_scaled+
                    WC_P_scaled+
                    # WPS_D_scaled+
                    # WPS_P_scaled+
                    Sixltr_D_scaled+
                    Sixltr_P_scaled+
                    # affiliation_D_scaled+
                    # affiliation_P_scaled+
                    # i_D_scaled+
                    # i_P_scaled+
                    # Clout_D_scaled+
                    # Clout_P_scaled+
                    # differ_D_scaled+
                    # differ_P_scaled+
                    # Clout_D_scaled+
                    # Clout_P_scaled+
                    # insight_D_scaled+
                    # insight_P_scaled+
                    # cause_D_scaled+
                    # cause_P_scaled+
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
                        LSM_function_mean + 
                        WC_D_scaled+
                        WC_P_scaled+
                        WPS_D_scaled+
                        WPS_P_scaled+
                        Sixltr_D_scaled+
                        Sixltr_P_scaled+
                      affiliation_D_scaled+
                      affiliation_P_scaled+
                      i_D_scaled+
                      i_P_scaled+
                      Clout_D_scaled+
                      Clout_P_scaled+
                      differ_D_scaled+
                      differ_P_scaled+
                      Clout_D_scaled+
                      Clout_P_scaled+
                      insight_D_scaled+
                      insight_P_scaled+
                      cause_D_scaled+
                      cause_P_scaled+
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
                        LSM_function_mean +
                        WC_D_scaled+
                        WC_P_scaled+
                        WPS_D_scaled+
                        WPS_P_scaled+
                        Sixltr_D_scaled+
                        Sixltr_P_scaled+
                        affiliation_D_scaled+
                        affiliation_P_scaled+
                        i_D_scaled+
                        i_P_scaled+
                        Clout_D_scaled+
                        Clout_P_scaled+
                        differ_D_scaled+
                        differ_P_scaled+
                        Clout_D_scaled+
                        Clout_P_scaled+
                        insight_D_scaled+
                        insight_P_scaled+
                        cause_D_scaled+
                        cause_P_scaled+
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
                      LSM_function_mean + 
                      WC_D_scaled+
                      WC_P_scaled+
                      WPS_D_scaled+
                      WPS_P_scaled+
                      Sixltr_D_scaled+
                      Sixltr_P_scaled+
                      affiliation_D_scaled+
                      affiliation_P_scaled+
                      i_D_scaled+
                      i_P_scaled+
                      Clout_D_scaled+
                      Clout_P_scaled+
                      differ_D_scaled+
                      differ_P_scaled+
                      Clout_D_scaled+
                      Clout_P_scaled+
                      insight_D_scaled+
                      insight_P_scaled+
                      cause_D_scaled+
                      cause_P_scaled+
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
                        LSM_function_mean + 
                      (1|provider_id), family = 'binomial', data = H3a.5_df)
anova(m.1_H3a.5_HLM,m.2_H3a.5_HLM)
summary(m.2_H3a.5_HLM)

sjPlot::tab_model(m.2_H3a.5_HLM)

# model H3a.6
############

# step 1: Null model
m.0_H3a.6_HLM <- lm(overallsat~ 1, data = H3a.6_df)

# step 2: add the culstering variable.. MD
m.1_H3a.6_HLM <- lmer(overallsat ~ 1 + (1|provider_id), data = H3a.6_df)
# test for fit of grouping structure
anova(m.1_H3a.6_HLM,m.0_H3a.6_HLM)

# step 3: add the predictors
m.2_H3a.6_HLM <- lmer(overallsat ~ 
                         LSM_function_mean + 
                       (1|provider_id), data = H3a.6_df)
anova(m.1_H3a.6_HLM,m.2_H3a.6_HLM)
summary(m.2_H3a.6_HLM)

sjPlot::tab_model(m.2_H3a.6_HLM)

# model H3b.1
############

# step 1: Null model
m.0_H3b.1_HLM <- glm(vlsup75~ 1, data = H3b.1_df)

# step 2: add the culstering variable.. MD
m.1_H3b.1_HLM <- glmer(factor(vlsup75) ~ 1 + (1|provider_id), family = 'binomial', data = H3b.1_df )
# test for fit of grouping structure
anova(m.0_H3b.1_HLM,m.1_H3b.1_HLM)
summary(m.1_H3b.1_HLM)
# step 3: add the predictors
m.2_H3b.1_HLM <- glmer(factor(vlsup75) ~ 
                     LSM_function_mean +
                     LSM_function_mean + 
                     i_D_scaled+
                     i_P_scaled+
                     negemo_D_scaled+
                     negemo_P_scaled+
                   (1|provider_id),
                 family = 'binomial',
                 data = H3b.1_df)
anova(m.1_H3b.1_HLM,m.2_H3b.1_HLM)
summary(m.2_H3b.1_HLM)
sjPlot::tab_model(m.2_H3b.1_HLM)
summary(m.1_H3b.1_HLM)
