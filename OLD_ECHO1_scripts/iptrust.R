library(lme4)
library(sjPlot)
m0 <- lmer(iptrust ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(iptrust ~
              #TbyT_function_autocorr +
              #TbyT_Analytic_autocorr +
              #TbyT_Authentic_autocorr +
              #TbyT_WC_autocorr +
              #TbyT_auxverb_autocorr +
              #RC1 +
              RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              #RC6 +
              #RC7 +
              
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
               (1|provid), data = x)
print(m.1,corr = FALSE)
tab_model(m.1)

#SE and CIs
se <- sqrt(diag(vcov(m.1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(m.1), LL = fixef(m.1) - 1.96 * se, UL = fixef(m.1) + 1.96 * se))

# odds ratio and CI's
exp(tab)

#write.csv(exp(tab), file = "coefs.csv")

plot_model(m.1)
tab_model(m.1)

