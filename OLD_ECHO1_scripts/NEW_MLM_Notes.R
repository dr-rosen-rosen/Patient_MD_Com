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
