library('ordinal')

m.clm.1 <- clmm(cultcomptert ~
                 RC1 +
                 RC2 +
                 RC3 +
                 RC4 +
                 RC5 +
                 RC6 +
                 RC7 + (1|provid), 
                 data = x)
summary(m.clm.1)

plot(m.clm.1)

m.clm.2 <- clmm2(factor(racecat2) ~
                 RC1 +
                 RC2 +
                 RC3 +
                 RC4 +
                 RC5 +
                 RC6 +
                 RC7 + (1|provid), data = x)

summary(m.clm.2)

pr.m.clm.2 <- profile(m.clm.2)
confint(pr.m.clm.2)

par(mfrow = c(2,2))
plot(pr.m.clm.2)


x$provcommtert <- factor(x$provcommtert, ordered = TRUE, levels = c(1,2,3))
m.clm.2 <- clmm(provcommtert ~
                 RC1 +
                 RC2 +
                 RC3 +
                 RC4 +
                 RC5 +
                 RC6 +
                 RC7 + (1|provid), data = x)

summary(m.clm.2)

x$ipstyletert <- factor(x$ipstyletert, ordered = TRUE, levels = c(1,2,3))
m.clm.2 <- clmm(ipstyletert ~
                 RC1 +
                 RC2 +
                 RC3 +
                 RC4 +
                 RC5 +
                 RC6 +
                 RC7 + (1|provid), data = x)
table(x$ipstyletert)
summary(m.clm.2)


library(lme4)

m0 <- glmer(cultcomptert ~ (1|racecat2), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(m0, corr = FALSE)

x$cultcomptert <- factor(x$cultcomptert,ordered = TRUE, levels = c(1,2,3))
x$provid <- factor(x$provid, ordered = FALSE)
x$racecat2 <- factor(x$racecat2, ordered = FALSE)

m.1 <- glmer(cultcomptert ~
               RC1 +
               RC2 +
               RC3 +
               RC4 +
               RC5 +
               RC6 +
               RC7 +
             (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"))#, nAGQ = 10))
print(m.1,corr = FALSE)
tab_model(m.1)
plot_model(m.1)

print(m.1)

table(x$racecat2)


m.1 <- glmer(factor(racecat2) ~
               #RC1 +
               RC2 +
               #RC3 +
               #RC4 +
               #RC5 +
               RC6 +
               #RC7 +
               (1|provid), data = x, family = binomial, control = glmerControl(optimizer = "bobyqa"))#, nAGQ = 10))
print(m.1,corr = FALSE)
tab_model(m.1)
plot_model(m.1, type = "diag")


m.1 <- lmer(RC6 ~
               factor(cultcomptert) +
               (1|provid), data = x, )
print(m.1,corr = FALSE)
tab_model(m.1)
plot_model(m.1)