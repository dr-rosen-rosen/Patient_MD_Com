
library(nlme)
library(multilevel)
library(jtools)
library(sjPlot)

IV <- 'RC1'

#Null model with grouping structure
Null.Model <- lme(RC2 ~ 1, random = ~1|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Null.Model)
summary(Null.Model)

#Model with no grouping variable
Null.Model.2 <- gls(RC2 ~ 1, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
#Compare the two null models
anova(Null.Model,Null.Model.2)
-2*logLik(Null.Model)
-2*logLik(Null.Model.2)

#reliability of group mean
GREL.Dat <- GmeanRel(Null.Model)
GREL.Dat$ICC
GREL.Dat$MeanRel
mean(GREL.Dat$MeanRel)

Model.1 <- lme(RC2 ~ factor(racecat2), random = ~1|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Model.1)
summary(Model.1)
anova(Null.Model,Model.1)
-2*logLik(Model.1)

Model.2 <- lme(RC7 ~ factor(racecat2) + factor(cultcomptert), random = ~factor(cultcomptert)|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Model.2)
summary(Model.2)
anova(Model.1,Model.2)
-2*logLik(Model.2)

###############################################################################

###############################################################################

###############################################################################

###############################################################################

#Null model with grouping structure
Null.Model <- lme(iptrust ~ 1, random = ~1|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Null.Model)
summary(Null.Model)

#Model with no grouping variable
Null.Model.2 <- gls(iptrust ~ 1, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
#Compare the two null models
anova(Null.Model,Null.Model.2)
-2*logLik(Null.Model)
-2*logLik(Null.Model.2)

Model.1 <- lme(iptrust ~ Comp.1, random = ~1|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Model.1)
summary(Model.1)
#anova(Null.Model,Model.1)
-2*logLik(Model.1)

Model.2 <- lme(iptrust ~ 
                 Comp.1 + 
                 # Comp.2 +
                 # Comp.3 +
                 # Comp.4 +
                 # Comp.5 +
                 # Comp.6 +
                 # Comp.7 +
                 # Comp.8 +
                 # Comp.9 +
                 # Comp.10 +
                 # Comp.11 +
                 factor(racecat2), random = ~ Comp.1|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Model.2)
summary(Model.2)
anova(Model.1,Model.2)
-2*logLik(Model.2)

summ(Model.2)
plot_model(Model.2)



###############################################################################

###############################################################################

###############################################################################

###############################################################################

#Null model with grouping structure
Null.Model <- lme(provassess ~ 1, random = ~1|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Null.Model)
summary(Null.Model)

#Model with no grouping variable
Null.Model.2 <- gls(provassess ~ 1, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
#Compare the two null models
anova(Null.Model,Null.Model.2)
-2*logLik(Null.Model)
-2*logLik(Null.Model.2)

Model.1 <- lme(provassess ~ Comp.3, random = ~1|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Model.1)
summary(Model.1)
#anova(Null.Model,Model.1)
-2*logLik(Model.1)

Model.2 <- lme(provassess ~ 
                 #Comp.1 +
                 #Comp.2 +
                 Comp.3 +
                 #Comp.4 +
                 Comp.5 +
                 #Comp.6 +
                 Comp.7 +
                 #Comp.8 +
                 #Comp.9 +
                 Comp.10 +
                 #Comp.11 +
                 factor(racecat2), random = ~ Comp.3|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Model.2)
summary(Model.2)
anova(Model.1,Model.2)
-2*logLik(Model.2)


###############################################################################

###############################################################################

###############################################################################

###############################################################################

#Null model with grouping structure
Null.Model <- lme(provcomm ~ 1, random = ~1|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Null.Model)
summary(Null.Model)

#Model with no grouping variable
Null.Model.2 <- gls(provcomm ~ 1, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
#Compare the two null models
anova(Null.Model,Null.Model.2)
-2*logLik(Null.Model)
-2*logLik(Null.Model.2)

Model.1 <- lme(provcomm ~ Comp.11, random = ~1|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Model.1)
summary(Model.1)
#anova(Null.Model,Model.1)
-2*logLik(Model.1)

Model.2 <- lme(provcomm ~ Comp.11 + factor(racecat2), random = ~ Comp.3|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Model.2)
summary(Model.2)
anova(Model.1,Model.2)
-2*logLik(Model.2)

plot_model(Model.2)

###############################################################################

###############################################################################

###############################################################################

###############################################################################

#Null model with grouping structure
Null.Model <- lme(ipstylehigh ~ 1, random = ~1|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Null.Model)
summary(Null.Model)

#Model with no grouping variable
Null.Model.2 <- gls(ipstylehigh ~ 1, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
#Compare the two null models
anova(Null.Model,Null.Model.2)
-2*logLik(Null.Model)
-2*logLik(Null.Model.2)

Model.1 <- lme(ipstylehigh ~ Comp.5, random = ~1|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Model.1)
summary(Model.1)
#anova(Null.Model,Model.1)
-2*logLik(Model.1)

Model.2 <- lme(ipstylehigh ~ Comp.11 + factor(racecat2), random = ~ Comp.3|provid, data = x, method = 'ML', control = list(opt='optim'),na.action=na.omit)
VarCorr(Model.2)
summary(Model.2)
anova(Model.1,Model.2)
-2*logLik(Model.2)

plot_model(Model.2, which = "fixed")

###############################################################################

###############################################################################

###############################################################################

###########################

library(lme4)

m0 <- glmer(provknowcat ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- glmer(provknowcat ~
               
               Comp.1 +
               Comp.2 +
               Comp.3 +
               Comp.4 +
               Comp.5 +
               Comp.6 +
               Comp.7 +
               Comp.8 +
               Comp.9 +
               Comp.10 +
               Comp.11 +
               factor(racecat2) +
               #factor(cultcomptert) +
               
               # Conv_LSM_ppron +
               # Conv_LSM_adverb +
               # Conv_LSM_negate +
               # TbyT_Vader_neg_autocorr +
               # TbyT_Clout_autocorr +
               # TbyT_function_autocorr +
               # TbyT_article_autocorr +
               # TbyT_prep_autocorr +
               # TbyT_quant_autocorr +
               
              (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"))#, nAGQ = 10)
print(m.1,corr = FALSE)

#SE and CIs
se <- sqrt(diag(vcov(m.1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(m.1), LL = fixef(m.1) - 1.96 * se, UL = fixef(m.1) + 1.96 * se))

# odds ratio and CI's
exp(tab)

#write.csv(exp(tab), file = "coefs.csv")
library(sjPlot)
plot_model(m.1)
tab_model(m.1)