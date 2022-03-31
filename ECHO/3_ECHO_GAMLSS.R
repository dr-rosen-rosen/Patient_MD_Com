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
H1.hist <- gamlss::histDist(LSM_function_mean, family = GB1, nbins = 30, data = H1.1_df)

######### H1.1
## Fit model
H1.1_gamlss <- gamlss::gamlss(
  formula = LSM_function_mean ~ racecat2,# + re(random = ~1|provider_id),
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
  formula = LSM_function_mean ~ raceconc,# + re(random = ~1|provider_id),
  family = GB1(), data = H1.2_df, trace = FALSE,
  control = gamlss.control(n.cyc = 4000)
)
## Evaluate
summary(H1.2_gamlss)
Rsq(H1.2_gamlss)
plot(H1.2_gamlss)
wp(H1.2_gamlss)

######### H1.3
## Fit model
H1.3_gamlss <- gamlss::gamlss(
  formula = LSM_function_mean ~ 
    cultdiss +
    cultdissmd +
    cdAvg_dist_abs +
    cdspeak_dist_abs +
    cdreason_dist_abs +
    cdstyle_dist_abs +
    cdvalue_dist_abs +
    cdspirit_dist_abs +
    cdethnic_dist_abs +
    cdtype_dist_abs +
    cdrace_dist_abs +
    cdculture_dist_abs +
    cdskin_dist_abs,# +
      # cdAvg_dist +
      # cdspeak_dist +
      # cdreason_dist +
      # cdstyle_dist +
      # cdvalue_dist +
      # cdspirit_dist +
      # cdethnic_dist +
      # cdtype_dist +
      # cdrace_dist +
      # cdculture_dist +
      # cdskin_dist,# + re(random = ~1|provider_id),
  family = GB1(), data = H1.3_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H1.3_gamlss)
Rsq(H1.3_gamlss)
plot(H1.3_gamlss)
wp(H1.3_gamlss)

#########################################
######### H3
#########################################

######### H3a.1
H3a.1.fit <- gamlss::fitDist(H3a.1_df$provcomm, type = 'realplus')
H3a.1.fit$fits
H3a.1.fit.hist <- gamlss::histDist(provcomm, family = BCCG, nbins = 30, data = H3a.1_df)

## Fit model
H3a.1_gamlss_conv <- gamlss::gamlss(
  formula = provcomm ~ 
    LSM_function_mean + 
    re(random = ~1|provider_id),
  family = BCCG(), data = H3a.1_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
H3a.1_gamlss_rLSM.D <- gamlss::gamlss(
  formula = provcomm ~ 
    rLSM.D + 
    # rLSM.P + 
    racecat2 +
    # provider_rLSM_sd +
    re(random = ~1|provider_id),
  family = BCCG(), data = H3a.1_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)

## Evaluate
summary(H3a.1_gamlss_rLSM.D)
Rsq(H3a.1_gamlss_rLSM.D)
plot(H3a.1_gamlss_rLSM.D)
wp(H3a.1_gamlss_rLSM.D)

H3a.1_gamlss_rLSM.P <- gamlss::gamlss(
  formula = provcomm ~ rLSM.P + re(random = ~1|provider_id),
  family = BCCG(), data = H3a.1_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)

## Evaluate
summary(H3a.1_gamlss_rLSM.P)
Rsq(H3a.1_gamlss_rLSM.P)
plot(H3a.1_gamlss_rLSM.P)
wp(H3a.1_gamlss_rLSM.P)


######### H3a.2
H3a.2.fit <- gamlss::fitDist(H3a.2_df$overcomm, type = 'realplus')
H3a.2.fit$fits
H3a.2.fit.hist <- gamlss::histDist(overcomm, family = BCCG, nbins = 30, data = H3a.2_df)

## Fit model
H3a.2_gamlss <- gamlss::gamlss(
  formula = overcomm ~ 
    LSM_function_mean + 
    rLSM.D +
    rLSM.P +
    racecat2 +
    re(random = ~1|provider_id),
  family = BCCG(), data = H3a.2_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H3a.2_gamlss)
Rsq(H3a.2_gamlss)
plot(H3a.2_gamlss)
wp(H3a.2_gamlss)


######### H3a.3; ipstyle, ipcare, iptalkfront
H3a.3.fit <- gamlss::fitDist(H3a.3_df$ipstyle, type = 'realplus')
H3a.3.fit$fits
H3a.3.fit.hist <- gamlss::histDist(ipstyle, family = BCCG, nbins = 30, data = H3a.3_df)

## Fit model
H3a.3_gamlss <- gamlss::gamlss(
  formula = ipstyle ~ 
    LSM_function_mean + 
    rLSM.D +
    rLSM.P +
    racecat2,# +
    # re(random = ~1|provider_id),
  family = BCCG(), data = H3a.3_df, trace = FALSE,
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
H3a.4.fit.hist <- gamlss::histDist(iptrust, family = BCCG, nbins = 30, data = H3a.4_df)

## Fit model
H3a.4_gamlss <- gamlss::gamlss(
  formula = iptrust ~ 
    # LSM_function_mean + 
    # rLSM.D +
    rLSM.P +
    re(random = ~1|provider_id),
  family = BCCG(), data = H3a.4_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H3a.4_gamlss)
Rsq(H3a.4_gamlss)
plot(H3a.4_gamlss)
wp(H3a.4_gamlss)



######### H3a.5
hist(H3a.5_df$provknowcat)
H3a.5.fit <- gamlss::fitDist(H3a.5_df$provknowcat, type = 'binom')
H3a.5.fit$fits
H3a.5.fit.hist <- gamlss::histDist(provknowcat, family = BI, nbins = 30, data = H3a.5_df)

## Fit model
H3a.5_gamlss <- gamlss::gamlss(
  formula = provknowcat ~ 
    LSM_function_mean + 
    rLSM.D +
    rLSM.P,
    #re(random = ~1|provider_id),
  family = BI(), data = H3a.5_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H3a.5_gamlss)
Rsq(H3a.5_gamlss)
plot(H3a.5_gamlss)
wp(H3a.5_gamlss)

######### H3a.6
hist(H3a.6_df$overallsat)
H3a.6.fit <- gamlss::fitDist(H3a.6_df$overallsat, type = 'binom')
H3a.6.fit$fits
H3a.6.fit.hist <- gamlss::histDist(overallsat, family = BI, nbins = 30, data = H3a.6_df)

## Fit model
H3a.6_gamlss <- gamlss::gamlss(
  formula = overallsat ~ 
    LSM_function_mean + 
    rLSM.D +
    rLSM.P,
    #re(random = ~1|provider_id),
  family = BI(), data = H3a.6_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H3a.6_gamlss)
Rsq(H3a.6_gamlss)
plot(H3a.6_gamlss)
wp(H3a.6_gamlss)

######### H3b

######### H3b.1
H3b.1.fit <- gamlss::fitDist(H3b.1_df$vlsup75, type = 'binom')
H3b.1.fit$fits
H3b.1.fit.hist <- gamlss::histDist(vlsup75, family = BI, nbins = 30, data = H3b.1_df)

## Fit model
H3b.1_gamlss <- gamlss::gamlss(
  formula = vlsup75 ~ 
    LSM_function_mean + 
    rLSM.P +
    rLSM.D,
    #re(random = ~1|provider_id),
  family = BI(), data = H3b.1_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)
## Evaluate
summary(H3b.1_gamlss)
Rsq(H3b.1_gamlss)
plot(H3b.1_gamlss)
wp(H3b.1_gamlss)

