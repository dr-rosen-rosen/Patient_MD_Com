library(gamlss)

################################################################################################
#Aim 2.1: testing whether patient race and/or cultural competence is predictive of LSM
################################################################################################


hist(H2.1_df$accom_raw_pcp)
p <- ggplot(H2.1_df, aes(x = pcp_cc_score, y = accom_10_seg_pcp.scaled, col = p_mainrace)) + 
  geom_point() +
  geom_smooth(method=lm) +
  ggthemes::theme_tufte() + theme(legend.position="bottom")
ggExtra::ggMarginal(p,type = "histogram")

accom_gamlss.fit <- gamlss::fitDist(H2.1_df$accom_raw_pcp, type = 'realAll')
accom_gamlss.fit$fits
accom_gamlss.hist <- gamlss::histDist(accom_raw_pcp, family = SEP2, nbins = 30, data = H2.1_df)


accom_gamlss.1 <- gamlss::gamlss(
  formula = accom_raw_pcp ~ 
    pcp_cc_score +
    pcp_cc_vdp_score +
    as.factor(p_mainrace) +
    # pb(as.vector(accom_raw_pcp.scaled)) +
    re(random = ~1|prov_id),
  family = SHASH(), data = H2.1_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)

summary(accom_gamlss.1)
Rsq(accom_gamlss.1)
plot(accom_gamlss.1)
wp(accom_gamlss.1)

################################################################################################
#Aim 2.2a: testing whether LSM is predictive of patient modified CAHPS score
################################################################################################

hist(H2.2a_df$pt_cahps_1_20_score)
p <- ggplot(H2.2a_df, aes(x = pt_cahps_1_20_score, 
                          # y = accom_raw_pcp_chng.scaled
                          # y = LSM_function_mean.scaled
                          # y = rw.rLSM.pcp.scaled
                          y = accom_10_seg_pcp.scaled, 
                          col = p_mainrace
                          )) + 
  geom_point() +
  geom_smooth(method=lm) +
  ggthemes::theme_tufte() + theme(legend.position="bottom")
ggExtra::ggMarginal(p,type = "histogram")

cahps_gamlss.fit <- gamlss::fitDist(H2.2a_df$pt_cahps_1_20_score, type = 'realplus')
cahps_gamlss.fit$fits
cahps_gamlss.hist <- gamlss::histDist(pt_cahps_1_20_score, family = BCCG, nbins = 30, data = H2.2a_df)


cahps_gamlss.1 <- gamlss::gamlss(
  formula = pt_cahps_1_20_score ~ 
    LSM_function_mean.scaled +
    rw.rLSM.pcp.scaled + 
    accom_10_seg_pcp.scaled +
    # accom_raw_pcp.scaled +
    as.factor(p_mainrace) +
    # as.factor(p_mainrace)*accom_10_seg_pcp.scaled +
    
    rw.rLSM.pcp.scaled*accom_10_seg_pcp.scaled +
    # pb(as.vector(accom_raw_pcp.scaled)) +
    re(random = ~1|prov_id),
  family = BCCG(), data = H2.2a_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)

summary(cahps_gamlss.1)
Rsq(cahps_gamlss.1)
plot(cahps_gamlss.1)
wp(cahps_gamlss.1)


################################################################################################
#Aim 2.2b: testing whether LSM is predictive of patient rating of clinician cultural comptence
################################################################################################

hist(H2.2b_df$pt_cc_score)

p <- ggplot(H2.2b_df, aes(x = pt_cc_score, 
                          # y = accom_raw_pcp_chng.scaled
                          # y = LSM_function_mean.scaled
                          # y = rw.rLSM.pcp.scaled
                          y = accom_10_seg_pcp.scaled, 
                          col = p_mainrace
)) + 
  geom_point() +
  geom_smooth(method=lm) +
  ggthemes::theme_tufte() + theme(legend.position="bottom")
ggExtra::ggMarginal(p,type = "histogram")


cc_gamlss.fit <- gamlss::fitDist(H2.2b_df$pt_cc_score, type = 'realplus')
cc_gamlss.fit$fits
cc_gamlss.hist <- gamlss::histDist(pt_cc_score, family = BCCG, nbins = 30, data = H2.2b_df)

cc_gamlss.1 <- gamlss::gamlss(
  formula = pt_cc_score ~ 
    LSM_function_mean.scaled +
    rw.rLSM.pcp.scaled + 
    accom_10_seg_pcp.scaled +
    as.factor(p_mainrace) +
    
    rw.rLSM.pcp.scaled*accom_10_seg_pcp.scaled +
    # pb(as.vector(accom_raw_pcp.scaled)) +
    re(random = ~1|prov_id),
  family = BCCG(), data = H2.2b_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)

summary(cc_gamlss.1)
Rsq(cc_gamlss.1)
plot(cc_gamlss.1)
wp(cc_gamlss.1)

################################################################################################
#Aim 2.2c: testing whether LSM is predictive of patient rating of trust in clinician
################################################################################################

hist(H2.2c_df$pt_trust_score)

p <- ggplot(H2.2c_df, aes(x = pt_trust_score, 
                          # y = accom_raw_pcp_chng.scaled
                          # y = LSM_function_mean.scaled
                          # y = rw.rLSM.pcp.scaled
                          y = accom_10_seg_pcp.scaled, 
                          col = p_mainrace
)) + 
  geom_point() +
  geom_smooth(method=lm) +
  ggthemes::theme_tufte() + theme(legend.position="bottom")
ggExtra::ggMarginal(p,type = "histogram")

trust_gamlss.fit <- gamlss::fitDist(H2.2c_df$pt_trust_score, type = 'realplus')
trust_gamlss.fit$fits
trust_gamlss.hist <- gamlss::histDist(pt_trust_score, family = BCCGo, nbins = 30, data = H2.2c_df)

trust_gamlss.1 <- gamlss::gamlss(
  formula = pt_trust_score ~ 
    LSM_function_mean.scaled +
    rw.rLSM.pcp.scaled + 
    accom_10_seg_pcp.scaled +
    as.factor(p_mainrace) +
    # pb(as.vector(accom_raw_pcp.scaled)) +
    
    rw.rLSM.pcp.scaled*accom_10_seg_pcp.scaled +
    re(random = ~1|prov_id),
  family = BCCGo(), data = H2.2c_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)

summary(trust_gamlss.1)
Rsq(trust_gamlss.1)
plot(trust_gamlss.1)
wp(trust_gamlss.1)


################################################################################################
#Aim 2.2d: testing whether LSM is predictive of patient rating of being respected
################################################################################################

hist(H2.2d_df$pt_respect_score)

p <- ggplot(H2.2d_df, aes(x = pt_respect_score, 
                          # y = accom_raw_pcp_chng.scaled
                          # y = LSM_function_mean.scaled
                          # y = rw.rLSM.pcp.scaled
                          y = accom_10_seg_pcp.scaled, 
                          col = p_mainrace
)) + 
  geom_point() +
  geom_smooth(method=lm) +
  ggthemes::theme_tufte() + theme(legend.position="bottom")
ggExtra::ggMarginal(p,type = "histogram")

respect_gamlss.fit <- gamlss::fitDist(H2.2d_df$pt_respect_score, type = 'realplus')
respect_gamlss.fit$fits
respect_gamlss.hist <- gamlss::histDist(pt_respect_score, family = BCCG, nbins = 30, data = H2.2d_df)

respect_gamlss.1 <- gamlss::gamlss(
  formula = pt_respect_score ~ 
    LSM_function_mean.scaled +
    rw.rLSM.pcp.scaled + 
    accom_10_seg_pcp.scaled +
    as.factor(p_mainrace) +
    
    rw.rLSM.pcp.scaled*accom_10_seg_pcp.scaled +
    # pb(as.vector(accom_raw_pcp.scaled)) +
    re(random = ~1|prov_id),
  family = BCCG(), data = H2.2d_df, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)

summary(respect_gamlss.1)
Rsq(respect_gamlss.1)
plot(respect_gamlss.1)
wp(respect_gamlss.1)
