#########################################
#### ECHO Scripts for GAMLSS
#########################################

library(gamlss)
library(tidyverse)
library(here)
library(ggplot2)
library(ggdist)
library(patchwork)



#########################################
######### cdspeak analysis 9/22/2022
#########################################

# f <- 'ECHO_All_Matching_Final.csv'
f <- 'H3a.7_df2.csv'
cd.spk.data <- read_csv(here(f)) %>%
  dplyr::select(
    age, gender, hsdegree, cdspeak, racecat2, site_id, provider_id, 
    LSM_function_mean, #LSM_function_mean.scaled, 
    class.D, class.P, 
    rLSM.D,rw.rLSM.D,# rLSM.D.scaled, 
    rLSM.P, rw.rLSM.P#rLSM.P.scaled
    ) %>%
  drop_na() %>%
  mutate(
    # cdspeak.shift = cdspeak - 1,
    racecat2 = as.factor(racecat2),
    provider_id = as.factor(provider_id),
    gender = as.factor(gender),
    hsdegree = as.factor(hsdegree),
    site_id = as.factor(site_id),
    class.D = recode(class.D, 'Constant_rLSM.D' = 'Stable', 'Accommodating_rLSM.D' = 'Increasing'),
    class.D = ordered(class.D, levels = c('Stable','Increasing')),
    class.P = recode(class.P, 'Constant_rLSM.P' = 'Stable', 'Decreasing_rLSM.P' = 'Decreasing'),
    class.P = ordered(class.P, levels = c('Stable','Decreasing')),
    cdspeak.rev = 7 - cdspeak) %>%
  filter(
    racecat2 != 4) %>%
  droplevels() %>%
  mutate(
    racecat2 = recode_factor(racecat2,`1` = "White", `2` = "Black",`3` = "Latino"),
    racecat2 = ordered(racecat2, levels = c('Black','Latino','White'))
  )


skimr::skim(cd.spk.data)


# https://mjskay.github.io/ggdist/articles/dotsinterval.html

f.LSM <- cd.spk.data %>%
  ggplot(aes(y = racecat2, x = LSM_function_mean, fill = racecat2)) +
  scale_y_discrete(limits = rev) +
  # ggplot(aes(y = as.factor(cdspeak), x = LSM_function_mean, fill = as.factor(cdspeak))) +
  xlim(0,1) + 
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
  stat_dotsinterval(side = "bottom", scale = 0.5, slab_size = NA) +
  scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom") +
  labs(title = "Distribtion of LSM by patient race",fill = "Race", y = 'Race', x = 'LSM')

f.LSM.overall <- cd.spk.data %>%
  ggplot(aes(x = LSM_function_mean)) +
  xlim(0,1) + 
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
  stat_dotsinterval(side = "bottom", scale = 0.5, slab_size = NA) +
  ggthemes::theme_tufte() + theme(legend.position="bottom") +
  labs(title = "Distribtion of LSM ",x = 'LSM')

# f.rLSM.P <- cd.spk.data %>%
#   ggplot(aes(y = racecat2, x = rLSM.P, fill = racecat2)) +
#   # ggplot(aes(y = as.factor(cdspeak), x = rLSM.P, fill = as.factor(cdspeak))) +
#   xlim(0,1) + 
#   stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
#   stat_dotsinterval(side = "bottom", scale = 0.5, slab_size = NA) +
#   scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom") +
#   labs(title = "Distribtion of rLSM.P by patient race",fill = "Race", y = 'Race', x = 'rLSM.P')

f.rLSM.D <- cd.spk.data %>%
  ggplot(aes(y = racecat2, x = rLSM.D, fill = racecat2)) +
  scale_y_discrete(limits = rev) +
  # ggplot(aes(y = as.factor(cdspeak), x = rLSM.D, fill = as.factor(cdspeak))) +
  xlim(0,1) + 
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
  stat_dotsinterval(side = "bottom", scale = 0.5, slab_size = NA) +
  scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom") +
  labs(title = "Distribtion of rLSM by patient race",fill = "Race", y = 'Race', x = 'rLSM')

f.rLSM.D.overall <- cd.spk.data %>%
  ggplot(aes(x = rLSM.D)) +
  # ggplot(aes(y = as.factor(cdspeak), x = rLSM.D, fill = as.factor(cdspeak))) +
  xlim(0,1) + 
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
  stat_dotsinterval(side = "bottom", scale = 0.5, slab_size = NA) +
  scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom") +
  labs(title = "Distribtion of rLSM",x = 'rLSM')

# f.rw.rLSM.P <- cd.spk.data %>%
#   ggplot(aes(y = racecat2, x = rw.rLSM.P, fill = racecat2)) +
#   # ggplot(aes(y = as.factor(cdspeak), x = rw.rLSM.P, fill = as.factor(cdspeak))) +
#   xlim(0,1) + 
#   stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
#   stat_dotsinterval(side = "bottom", scale = 0.5, slab_size = NA) +
#   scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom") +
#   labs(title = "Distribtion of rw.rLSM.P by patient race",fill = "Race", y = 'Race', x = 'rw.rLSM.P')

f.rw.rLSM.D <- cd.spk.data %>%
  ggplot(aes(y = racecat2, x = rw.rLSM.D, fill = racecat2)) +
  scale_y_discrete(limits = rev) +
  # ggplot(aes(y = as.factor(race), x = rw.rLSM.D, fill = as.factor(cdspeak))) +
  xlim(0,1) + 
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
  stat_dotsinterval(side = "bottom", scale = 0.5, slab_size = NA) +
  scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom") +
  labs(title = "Distribtion of rw.rLSM by patient race",fill = "Race", y = 'Race', x = 'rw.rLSM')

f.rw.rLSM.D.overall <- cd.spk.data %>%
  ggplot(aes(x = rw.rLSM.D)) +
  xlim(0,1) + 
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
  stat_dotsinterval(side = "bottom", scale = 0.5, slab_size = NA) +
  scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom") +
  labs(title = "Distribtion of rw.rLSM",x = 'rw.rLSM')

f.overall.test <- cd.spk.data %>%
  select(LSM_function_mean,rLSM.D,rw.rLSM.D) %>%
  pivot_longer(cols = everything(), names_to = 'matching_var',values_to = 'value') %>%
  mutate(
    matching_var = recode(matching_var, 'rw.rLSM.D' = 'rw.rLSM', 'rLSM.D' = 'rLSM', 'LSM_function_mean' = 'LSM'),
  ) %>%
  ggplot(aes(y = matching_var, x = value, fill = matching_var)) +
  scale_y_discrete(limits = rev) +
  # ggplot(aes(y = as.factor(race), x = rw.rLSM.D, fill = as.factor(cdspeak))) +
  xlim(0,1) + 
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
  stat_dotsinterval(side = "bottom", scale = 0.5, slab_size = NA) +
  scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom") +
  labs(title = "Distribtion of linguistic style matching measures",fill = "Matching measure", y = 'Matching measure', x = 'Matching score')
  

f.class.D <- cd.spk.data %>%
  # ggplot(aes(fill = class.D, x = racecat2)) +
  ggplot(aes(fill = racecat2, x = class.D)) +
  geom_bar(position = 'dodge') + scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom") +
  labs(title = "Distribtion of rw.rLSM.D growth class by patient race",fill = "Race", x = 'Accomodation Class', y = '# of patient encounters')

# f.class.P <- cd.spk.data %>%
#   ggplot(aes(fill = class.P, x = racecat2)) +
#   geom_bar(position = 'dodge') + scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom") +
#   labs(title = "Distribtion of rw.rLSM.P growth class by patient race",fill = "Race", x = 'Race', y = '# of patient encounters')



f <- f.overall.test / f.class + plot_annotation(tag_levels = 'A')

# f <- f.LSM + f.rLSM.D + f.rLSM.P +  f.rw.rLSM.D + f.rw.rLSM.P + guide_area() + plot_annotation(tag_levels = 'A') +
#   plot_layout(guides = 'collect')# & theme(legend.position = 'bottom')
# 
# bottom <- (f.rLSM.D.2Class + f.rLSM.P.2Class ) / (f.class.D + f.class.P) +  plot_annotation(tag_levels = 'A') #+ plot_layout(guides = 'collect')# & theme(legend.position = 'bottom')

f.rLSM.D.2Class
cd.spk.data %>%
  mutate(cdspeak = as.factor(cdspeak)) %>%
  ggplot(aes(y = cdspeak, x = rw.rLSM.D, fill = cdspeak)) +
  # ggplot(aes(y = as.factor(cdspeak), x = LSM_function_mean, fill = as.factor(cdspeak))) +
  # xlim(0,1) + 
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
  stat_dotsinterval(side = "bottom", scale = 0.5, slab_size = NA) +
  scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom")
  # labs(title = "Distribtion of LSM by patient race",fill = "Race", y = 'Race', x = 'LSM')

cd.spk.data %>%
  mutate(cdspeak = as.factor(cdspeak)) %>%
  ggplot(aes(fill = class.D, x = cdspeak)) +
  geom_bar(position = 'dodge') + scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom")# +
  # labs(title = "Distribtion of rw.rLSM.D growth class by patient race",fill = "Race", x = 'Race', y = '# of patient encounters')
cd.spk.data %>%
  mutate(cdspeak = as.factor(cdspeak)) %>%
  ggplot(aes(fill = class.D, x = cdspeak)) +
  # geom_bar(position = 'fill') + 
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set2") + 
  ggthemes::theme_tufte() + theme(legend.position="bottom") + 
  scale_y_continuous(labels = scales::percent)
# labs(title = "Distribtion of rw.rLSM.D growth class by patient race",fill = "Race", x = 'Race', y = '# of patient encounters')



# scale
cd.spk.data <- cd.spk.data %>%
  mutate(
    LSM_function_mean = scale(LSM_function_mean),
    rLSM.D = scale(rLSM.D),
    rw.rLSM.D = scale(rw.rLSM.D),
    rLSM.P = scale(rLSM.P),
    rw.rLSM.P = scale(rw.rLSM.P)
    # across(c(LSM_function_mean, rLSM.D,rw.rLSM.D, rLSM.P, rw.rLSM.P),
    #        scale(.,center = TRUE))
  )

# Potential distributions: IG, LOGNO, LOGNO2, GA, WEI3, WEI2, WEI, 

cd.speak.fit <- gamlss::fitDist(cd.spk.data$cdspeak.rev, type = "realplus")
cd.speak.fit$fits
cd.speak.hist <- gamlss::histDist(cdspeak.rev, family = exGAUS, nbins = 30, data = cd.spk.data)


## Fit models
cd.speak.gamlss.1 <- gamlss::gamlss(
  formula = cdspeak.rev ~ racecat2 + 
    site_id +
    age + gender +
    hsdegree +
    # LSM_function_mean +
    # rLSM.D + rLSM.P +
    # rw.rLSM.D + rw.rLSM.P +
    # class.D + class.P +
    re(random = ~1|provider_id),
  family = exGAUS(), data = cd.spk.data, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)

cd.speak.gamlss.2 <- gamlss::gamlss(
  formula = cdspeak.rev ~ racecat2 + 
    site_id +
    age + gender +
    hsdegree +
    LSM_function_mean +
    #rLSM.D + #rLSM.P +
    #rw.rLSM.D + #rw.rLSM.P +
    #class.D + #class.P +
    re(random = ~1|provider_id),
  family = exGAUS(), data = cd.spk.data, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)

cd.speak.gamlss.3 <- gamlss::gamlss(
  formula = cdspeak.rev ~ racecat2 + 
    site_id +
    age + gender +
    hsdegree +
    # LSM_function_mean +
    rLSM.D + #rLSM.P +
    # rw.rLSM.D + #rw.rLSM.P +
    # class.D + #class.P +
    re(random = ~1|provider_id),
  family = exGAUS(), data = cd.spk.data, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)


cd.speak.gamlss.4 <- gamlss::gamlss(
  formula = cdspeak.rev ~ racecat2 + 
    site_id +
    age + gender +
    hsdegree +
    # LSM_function_mean +
    # rLSM.D + #rLSM.P +
    rw.rLSM.D + #rw.rLSM.P +
    # class.D + #class.P +
    re(random = ~1|provider_id),
  family = exGAUS(), data = cd.spk.data, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)

cd.speak.gamlss.5 <- gamlss::gamlss(
  formula = cdspeak.rev ~  racecat2 +
    site_id +
    age + gender +
    hsdegree +
    # LSM_function_mean +
    # rLSM.D + #rLSM.P +
    # rw.rLSM.D + #rw.rLSM.P +
    forcats::fct_rev(class.D) +# forcats::fct_rev(class.P) +
    re(random = ~1|provider_id),
  family = exGAUS(), data = cd.spk.data, trace = FALSE,
  control = gamlss.control(n.cyc = 2000)
)

# cd.speak.gamlss.6 <- gamlss::gamlss(
#   formula = cdspeak.rev ~  racecat2 +
#     site_id +
#     age + gender +
#     hsdegree +
#     LSM_function_mean +
#     rLSM.D + #rLSM.P +
#     rw.rLSM.D + #rw.rLSM.P +
#     class.D + class.P +
#     class.D*class.P +
#     re(random = ~1|provider_id),
#   family = exGAUS(), data = cd.spk.data, trace = FALSE,
#   control = gamlss.control(n.cyc = 2000)
# )
# summary(cd.speak.gamlss.6)
## Evaluate
summary(cd.speak.gamlss.4)
Rsq(cd.speak.gamlss.5)
plot(cd.speak.gamlss.5)
wp(cd.speak.gamlss.5)

table(cd.spk.data$racecat2,cd.spk.data$class.D)


library(modelsummary)
models = list(
  # "M1" = cd.speak.gamlss.1,
  "Model 1" = cd.speak.gamlss.2,
  "Model 2" = cd.speak.gamlss.3,
  "Model 3" = cd.speak.gamlss.4,
  "Model 4" = cd.speak.gamlss.5
)
modelsummary(models, 
             fmt = NULL,
             #estimate = "{round(exp(estimate),2)}{stars} [{round(exp(conf.low),2)}, {round(exp(conf.high),2)}]", 
             estimate = "{round(estimate,2)}{stars} [{round(conf.low,2)}, {round(conf.high,2)}]", 
             coef_omit = 'Intercept|racecat2|site_id|age|gender|hsdegree', 
             statistic = NULL#,
             # output = "gamlssTable.html",
             # output = "gamlssTable.docx",
             # coef_rename = c("LSM_function_mean.scaled" = "LSM","rLSM.D.scaled" = 'rLSM: MD',"rLSM.P.scaled" = "rLSM: P", "class.D" = "GMM Class: MD")
             )

summary(cd.speak.gamlss.1)
summary(cd.speak.gamlss.2)
Rsq(cd.speak.gamlss.2)
summary(cd.speak.gamlss.3)
Rsq(cd.speak.gamlss.3)
summary(cd.speak.gamlss.4)
Rsq(cd.speak.gamlss.4)
summary(cd.speak.gamlss.5)
Rsq(cd.speak.gamlss.5)
summary(cd.speak.gamlss.6)

drop1(cd.speak.gamlss.6)

library(gamlss.ggplots)
gamlss.ggplots::centile_bucket(cd.speak.gamlss.5)

gamlss.ggplots::model_GAIC(chooseDist(cd.speak.gamlss.5))




t <- cd.spk.data %>%
  mutate(crossClass = interaction(class.D,class.P, sep = "_D.P_")) %>%
  ggplot(aes(y = crossClass, x = cdspeak.rev, fill = crossClass)) +
  # xlim(0,1) + 
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
  stat_dotsinterval(side = "bottom", scale = 0.5, slab_size = NA) +
  scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom") #+
  # labs(title = "Distribtion of rw.rLSM.D by patient race",fill = "Race", y = 'Race', x = 'rw.rLSM.D')


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




# bb_test <- haven::read_dta(here('/Users/mrosen44/Johns Hopkins/Anne Links - Black Box study/Black Box quantitative data/Black Box data merged_all RIAS_9-16-21.dta'))

