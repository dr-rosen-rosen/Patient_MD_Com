#################### cultdissmd1

library(lme4)
library(sjPlot)
m0 <- lmer(cultdiss ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cultdiss ~
              RC1 +
              RC2 +
              RC3 +
              RC4 +
              RC5 +
              RC6 +
              RC7 +
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

#################### cdspeak





#################### cultdissmd1

library(lme4)

m0 <- lmer(cultdissmd1 ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cultdissmd1 ~
              #RC1 +
              RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)

#################### cdspeak

library(lme4)

m0 <- lmer(cdspeak ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cdspeak ~
              #RC1 +
              RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              #RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)


#################### cultdissmd2

library(lme4)

m0 <- lmer(cultdissmd2 ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cultdissmd2 ~
              #RC1 +
              #RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              RC6 +
              RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)

#################### cdreason
library(lme4)

m0 <- lmer(cdreason ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cdreason ~
              #RC1 +
              #RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              #RC6 +
              RC7 +
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
library(sjPlot)
plot_model(m.1)


#################### cultdissmd3

library(lme4)

m0 <- lmer(cultdissmd3 ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cultdissmd3 ~
              #RC1 +
              RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)

#################### cdstyle
library(lme4)

m0 <- lmer(cdstyle ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cdstyle ~
              #RC1 +
              #RC2 +
              #RC3 +
             #RC4 +
              #RC5 +
              RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)


#################### cultdissmd4

library(lme4)

m0 <- lmer(cultdissmd4 ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cultdissmd4 ~
              #RC1 +
              #RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              RC6 +
              RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)

#################### cdvalue
library(lme4)

m0 <- lmer(cdvalue ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cdvalue ~
              #RC1 +
              #RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)


#################### cultdissmd5

library(lme4)

m0 <- lmer(cultdissmd5 ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cultdissmd5 ~
              #RC1 +
              #RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)

#################### cdspirit
library(lme4)

m0 <- lmer(cdspirit ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cdspirit ~
              #RC1 +
              #RC2 +
              RC3 +
              #RC4 +
              #RC5 +
              #RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)

#################### cultdissmd6

library(lme4)

m0 <- lmer(cultdissmd6 ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cultdissmd6 ~
              #RC1 +
              RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)

#################### cdethnic
library(lme4)

m0 <- lmer(cdethnic ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cdethnic ~
              RC1 +
              #RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)


#################### cultdissmd7

library(lme4)

m0 <- lmer(cultdissmd7 ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cultdissmd7 ~
              RC1 +
              #RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)

#################### cdtype
library(lme4)

m0 <- lmer(cdtype ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cdtype ~
              #RC1 +
              #RC2 +
              #RC3 +
              RC4 +
              #RC5 +
              #RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)

#################### cultdissmd8

library(lme4)

m0 <- lmer(cultdissmd8 ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cultdissmd8 ~
              #RC1 +
              RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)

#################### cdrace
library(lme4)

m0 <- lmer(cdrace ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cdrace ~
              RC1 +
              RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)


#################### cultdissmd9

library(lme4)

m0 <- lmer(cultdissmd9 ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cultdissmd9 ~
              RC1 +
              #RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              RC6 +
              #RC7 +
              #factor(racecat2) +
              #factor(cultcomptert) +
              (1|provid/racecat2), data = x)
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

#################### cdculture
library(lme4)

m0 <- lmer(cdculture ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cdculture ~
              RC1 +
              RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)


#################### cultdissmd9

library(lme4)

m0 <- lmer(cultdissmd9 ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cultdissmd9 ~
              RC1 +
              #RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)

#################### cdskin
library(lme4)

m0 <- lmer(cdskin ~ (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

m.1 <- lmer(cdskin ~
              #RC1 +
              RC2 +
              #RC3 +
              #RC4 +
              #RC5 +
              #RC6 +
              #RC7 +
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
library(sjPlot)
plot_model(m.1)
tab_model(m.1)