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
# Get complete cases

Ha_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, racecat2, raceconc, 
         cultdissmd, cultdiss, 
         cdspeak,cdreason,cdstyle,cdvalue,cdspirit,cdethnic,cdtype,cdrace,cdculture,cdskin,
         cultdissmd1, cultdissmd2, cultdissmd3, cultdissmd4, cultdissmd5, cultdissmd6, cultdissmd7, cultdissmd8, cultdissmd9, cultdissmd10) %>%
  tidyr::drop_na() %>% # take only rows with no NA;
  mutate(
    cdAvg_dist = abs(cultdiss - cultdissmd),
    cdspeak_dist = abs(cdspeak - cultdissmd1),
    cdreason_dist = abs(cdreason - cultdissmd2),
    cdstyle_dist = abs(cdstyle - cultdissmd3),
    cdvalue_dist = abs(cdvalue - cultdissmd4),
    cdspirit_dist = abs(cdspirit - cultdissmd5),
    cdethnic_dist = abs(cdethnic - cultdissmd6),
    cdtype_dist = abs(cdtype - cultdissmd7),
    cdrace_dist = abs(cdrace - cultdissmd8),
    cdculture_dist = abs(cdculture - cultdissmd9),
    cdskin_dist = abs(cdskin - cultdissmd10)
  ) %>%
  # mutate(across(!provider_id & !racecat2 & !raceconc, ~scale(.,center = TRUE, scale = TRUE))) %>%
  mutate(provider_id = factor(provider_id)) %>%
  mutate(racecat2 = factor(racecat2)) %>%
  mutate(raceconc = factor(raceconc))

# drop physicians with < n cases
Ha_df <- Ha_df %>%
  group_by(provider_id) %>%
  filter(n() > 5) %>%
  ungroup() %>%
  gdata::drop.levels(.)

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
##################################################################
##################################################################
##################################################################

# hypothesis 2 (skip for now)
# Hb: Clinicians with higher cultural competence will be more linguistically accommodating. 


# library(mgcv)
# y.0 <- mgcv::gam(provassess ~ s(LSM_function_mean), data = Hc_df, method = 'REML')
# summary(y.0)
# plot(y.0, 
#      residuals = TRUE, 
#      rug = TRUE,
#      #pch = 1,
#      #cex = 1,
#      shade = TRUE,
#      pages = 1,
#      shift = coef(y.0)[1]
# )
# gam.check(y.0)
# concurvity(y.0, full = TRUE)

f1 <- gamlss::fitDist(Ha_df$LSM_function_mean, type = "real0to1")
f1$fits
gamlss::histDist(LSM_function_mean, family = GB1, nbins = 30, data = Ha_df)
f2 <- gamlss::fitDist(Hc_df$provassess, type = "realplus")
f2$fits
gamlss::histDist(provassess, family = BCPE, nbins = 30, data = Hc_df)
f3 <- gamlss::fitDist(Hb_vl_df$vlsup75, type = "binom")
f3$fits
gamlss::histDist(vlsup75, family = BI, nbins = 30, data = Hb_vl_df)

ip_style_fit <- gamlss::fitDist(ECHO_LSM_MLM$ipstyle, type = 'realplus')
ip_style_fit$fits
gamlss::histDist(ipstyle, family = BCCGo, nbins = 30, data = ECHO_LSM_MLM)

iptrust_fit <- gamlss::fitDist(ECHO_LSM_MLM$iptrust, type = 'realplus')
iptrust_fit$fits
gamlss::histDist(iptrust, family = BCCGo, nbins = 30, data = ECHO_LSM_MLM)

provcomm_fit <- gamlss::fitDist(ECHO_LSM_MLM$provcomm, type = 'realplus')
provcomm_fit$fits
gamlss::histDist(provcomm, family = BCCGo, nbins = 30, data = ECHO_LSM_MLM)

library(gamlss)
library(devtools)
library(sjPlot)
library(insight)
library(httr)
library(brms)

devtools::install_github("strengejacke/sjPlot")
devtools::install_github("https://github.com/easystats/insight")
install.packages("insight", repos = "https://easystats.r-universe.dev")

t1 <- gamlss::gamlss(
  formula = LSM_function_mean ~ cultdissmd + cultdiss + cdAvg_dist + cdspeak_dist + cdstyle_dist + racecat2 + raceconc + re(random = ~1|provider_id),
  family = GB1(), data = Ha_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
  )
summary(t1)
Rsq(t1)
plot(t1)
wp(t1)
plot(LSM_function_mean~cdstyle_dist, data = Ha_df)
sjPlot::tab_model(m.2_ha, show.aic = TRUE)
modelsummary::modelsummary(
  t1, 
  estimate = "{estimate} ({std.error}){stars}", 
  statistic = NULL,
  coef_omit = c("(Intercept)"))

t2 <- gamlss::gamlss(
  formula = provassess ~ LSM_function_mean + re(random = ~1|provider_id),
  family = BCPE(), data = na.omit(Hc_df), trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
modelsummary::modelsummary(
  t2, 
  estimate = "{estimate} ({std.error}){stars}", 
  statistic = NULL,
  coef_omit = c("(Intercept)"))
summary(t2)
Rsq(t2)
plot(t2)
wp(t2)
plot(LSM_function_mean~provassess, data = Hc_df)
sjPlot::tab_model(t2)
t3 <- gamlss::gamlss(
  formula = vlsup75 ~ LSM_function_mean + re(random = ~1|provider_id),
  family = BI(), data = na.omit(Hb_vl_df), trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
modelsummary::modelsummary(
  t3, 
  estimate = "{estimate} ({std.error}){stars}", 
  statistic = NULL,
  coef_omit = c("(Intercept)"))

summary(t3)
Rsq(t3)
plot(t3)
wp(t3)

prov_comm_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provcomm, provider_id) %>%
  drop_na()
t4 <- gamlss::gamlss(
  formula = provcomm ~ LSM_function_mean + re(random = ~1|provider_id),
  family = BCCGo(), data = prov_comm_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
summary(t4)
Rsq(t4)
plot(t4)
wp(t4)
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
