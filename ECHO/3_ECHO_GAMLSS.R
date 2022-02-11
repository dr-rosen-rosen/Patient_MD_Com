#########################################
#### ECHO Scripts for GAMLSS
#########################################

library(gamlss)

#########################################
######### H1
#########################################

## Find distribution for outcome
H1.fit <- gamlss::fitDist(H1.1_df$LSM_function_mean, type = "real0to1")
H1.fit$fits
H1.hist <- gamlss::histDist(LSM_function_mean, family = GB1, nbins = 30, data = H1.fit)

######### H1.1
## Fit model
H1.1_gamlss <- gamlss::gamlss(
  formula = LSM_function_mean ~ racecat2 + re(random = ~1|provider_id),
  family = GB1(), data = H1.1_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H1.1_gamlss)
Rsq(H1.1_gamlss)
plot(H1.1_gamlss)
wp(H1.1_gamlss)

######### H1.2
## Fit model
H1.2_gamlss <- gamlss::gamlss(
  formula = LSM_function_mean ~ raceconc + re(random = ~1|provider_id),
  family = GB1(), data = H1.1_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H1.2_gamlss)
Rsq(H1.2_gamlss)
plot(H1.2_gamlss)
wp(H1.2_gamlss)

######### H1.3
## Fit model
H1.2_gamlss <- gamlss::gamlss(
  formula = LSM_function_mean ~ cultdiss + cultdissmd + cdAvg_dist + cdspeak_dist + cdstyle_distre + (random = ~1|provider_id),
  family = GB1(), data = H1.1_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H1.2_gamlss)
Rsq(H1.2_gamlss)
plot(H1.2_gamlss)
wp(H1.2_gamlss)

#########################################
######### H3
#########################################

######### H3a.1
H3a.1.fit <- gamlss::fitDist(H3a.1_df$provcomm, type = 'realplus')
H3a.1.fit$fits
H3a.1.fit.hist <- gamlss::histDist(provcomm, family = BCCGo, nbins = 30, data = H3a.1_df)

## Fit model
H3a.1_gamlss <- gamlss::gamlss(
  formula = LSM_function_mean ~ cultdiss + cultdissmd + cdAvg_dist + cdspeak_dist + cdstyle_distre + (random = ~1|provider_id),
  family = GB1(), data = H3a.1_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H3a.1_gamlss)
Rsq(H3a.1_gamlss)
plot(H3a.1_gamlss)
wp(H3a.1_gamlss)


######### H3a.2
H3a.2.fit <- gamlss::fitDist(H3a.2_df$overcomm, type = 'realplus')
H3a.2.fit$fits
H3a.2.fit.hist <- gamlss::histDist(overcomm, family = BCCGo, nbins = 30, data = H3a.2_df)

## Fit model
H3a.2_gamlss <- gamlss::gamlss(
  formula = overcomm ~ LSM_function_mean + (random = ~1|provider_id),
  family = GB1(), data = H3a.2_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H3a.2_gamlss)
Rsq(H3a.2_gamlss)
plot(H3a.2_gamlss)
wp(H3a.2_gamlss)


######### H3a.3
H3a.3.fit <- gamlss::fitDist(H3a.3_df$ipstyle, type = 'realplus')
H3a.3.fit$fits
H3a.3.fit.hist <- gamlss::histDist(ipstyle, family = BCCGo, nbins = 30, data = H3a.3_df)

## Fit model
H3a.3_gamlss <- gamlss::gamlss(
  formula = ipstyle ~ LSM_function_mean + re(random = ~1|provider_id),
  family = GB1(), data = H3a.3_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H3a.3_gamlss)
Rsq(H3a.3_gamlss)
plot(H3a.3_gamlss)
wp(H3a.3_gamlss)

######### H3a.4
H3a.4.fit <- gamlss::fitDist(H3a.4_df$iptrust, type = 'realplus')
H3a.4.fit$fits
H3a.4.fit.hist <- gamlss::histDist(iptrust, family = BCCGo, nbins = 30, data = H3a.4_df)

## Fit model
H3a.4_gamlss <- gamlss::gamlss(
  formula = iptrust ~ LSM_function_mean + re(random = ~1|provider_id),
  family = GB1(), data = H3a.4_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H3a.4_gamlss)
Rsq(H3a.4_gamlss)
plot(H3a.4_gamlss)
wp(H3a.4_gamlss)

######### H3a.5
H3a.5.fit <- gamlss::fitDist(H3a.5_df$iptrust, type = 'realplus')
H3a.5.fit$fits
H3a.5.fit.hist <- gamlss::histDist(iptrust, family = BCCGo, nbins = 30, data = H3a.5_df)

## Fit model
H3a.4_gamlss <- gamlss::gamlss(
  formula = iptrust ~ LSM_function_mean + re(random = ~1|provider_id),
  family = GB1(), data = H3a.4_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H3a.4_gamlss)
Rsq(H3a.4_gamlss)
plot(H3a.4_gamlss)
wp(H3a.4_gamlss)

######### H3a.5
H3a.5.fit <- gamlss::fitDist(H3a.5_df$provknowcat, type = 'realplus')
H3a.5.fit$fits
H3a.5.fit.hist <- gamlss::histDist(provknowcat, family = BCCGo, nbins = 30, data = H3a.5_df)

## Fit model
H3a.5_gamlss <- gamlss::gamlss(
  formula = provknowcat ~ LSM_function_mean + re(random = ~1|provider_id),
  family = GB1(), data = H3a.5_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H3a.5_gamlss)
Rsq(H3a.5_gamlss)
plot(H3a.5_gamlss)
wp(H3a.5_gamlss)

######### H3a.6
H3a.6.fit <- gamlss::fitDist(H3a.6_df$overallsat, type = 'realplus')
H3a.6.fit$fits
H3a.6.fit.hist <- gamlss::histDist(overallsat, family = BCCGo, nbins = 30, data = H3a.6_df)

## Fit model
H3a.6_gamlss <- gamlss::gamlss(
  formula = overallsat ~ LSM_function_mean + re(random = ~1|provider_id),
  family = GB1(), data = H3a.6_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H3a.6_gamlss)
Rsq(H3a.6_gamlss)
plot(H3a.6_gamlss)
wp(H3a.6_gamlss)

######### H3b

######### H3b.1
H3b.1.fit <- gamlss::fitDist(H3b.1_df$vlsup75, type = 'realplus')
H3b.1.fit$fits
H3b.1.fit.hist <- gamlss::histDist(vlsup75, family = BCCGo, nbins = 30, data = H3b.1_df)

## Fit model
H3a.6_gamlss <- gamlss::gamlss(
  formula = vlsup75 ~ LSM_function_mean + re(random = ~1|provider_id),
  family = GB1(), data = H3b.1_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H3b.1_gamlss)
Rsq(H3b.1_gamlss)
plot(H3b.1_gamlss)
wp(H3b.1_gamlss)

