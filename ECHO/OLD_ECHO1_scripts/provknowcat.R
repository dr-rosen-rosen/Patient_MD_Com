library(lme4)

m0 <- glmer(factor(provknowcat) ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- glmer(factor(provknowcat) ~
               #TbyT_Clout_autocorr +
               #Conv_LSM_negate +
               #TbyT_negate_autocorr +
               #RC1 +
               #RC2 +
               #RC3 +
               #RC4 +
               #RC5 +
               #RC6 +
               RC7 +
               #Comp.1 +
               #Comp.2 +
               #Comp.3 +
               #Comp.4 +
               #Comp.5 +
               #Comp.6 +
               #Comp.7 +
               #Comp.8 +
               #Comp.9 +
               #Comp.10 +
               #Comp.11 +
               #factor(racecat2) +
               #factor(cultcomptert) +
             (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"))#, nAGQ = 10)
print(m.1,corr = FALSE)

tab_model(m.1)

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


