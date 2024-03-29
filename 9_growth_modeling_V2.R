############################################################################################################
############# Growth modeling for ECHO data
############################################################################################################

library(tidyverse)
library(config)
library(here)
library(lcmm)
Sys.setenv(R_CONFIG_ACTIVE = "mike") # 'default')#
config <- config::get()

# gm_df <- read.csv(here(config$ECHO_rolling_window_8_LIWC_path,config$ECHO_rolling_window_8_LIWC_name))

############# rLSM.D models

gm_df <- ECHO_smoothed_rLSM %>% # This df comes from runing the rLSM script trhough line 124 (stops before aggregating to file level)
  mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
  drop_na(text_agg) %>%
  filter(Speaker == 'D') %>%
  select(File, rLSM) %>%
  drop_na() %>%
  # mutate(
  #   rLSM = scale(rLSM,center = TRUE)
  # ) %>%
  group_by(File) %>%
  mutate(
    id = row_number(),
    # t = case_when(id < (max(id)/5) ~ 0,
    #           id < (max(id/5))*2 ~ 1,
    #           id < (max(id/5))*3 ~ 2,
    #           id < (max(id/5))*4 ~ 3,
    #           TRUE ~ 4)
    t = case_when(id < (max(id)/10) ~ 0,
                  id < (max(id/10))*2 ~ 1,
                  id < (max(id/10))*3 ~ 2,
                  id < (max(id/10))*4 ~ 3,
                  id < (max(id/10))*5 ~ 4,
                  id < (max(id/10))*6 ~ 5,
                  id < (max(id/10))*7 ~ 6,
                  id < (max(id/10))*8 ~ 7,
                  id < (max(id/10))*9 ~ 8,
                  TRUE ~ 9)
    ) %>%
    ungroup() %>%
  select(-id) %>%
  group_by(File,t) %>%
  summarize(rLSM = mean(rLSM)) %>%
  ungroup() %>% 
  mutate(group_id = group_indices(., File))



m1.rLSM.D.v2 <- lcmm::hlme(
  rLSM ~ t,
  # random = ~ t,
  subject = 'group_id',
  data = gm_df
)

m2.rLSM.D.v2 <- lcmm::hlme(
  rLSM ~ t,
  # random = ~ t,
  mixture = ~ t,
  ng = 2,
  subject = 'group_id',
  data = gm_df,
  B = random(m1.rLSM.D.v2)
  )

# m2g.rLSM.D.v2 <- lcmm::gridsearch(
#   rep = 100, maxiter = 30, minit = m1.rLSM.D.v2, 
#   hlme(rLSM ~ t,
#     # random = ~ 1 + t,
#     mixture = ~ t,
#     ng = 2,
#     subject = 'group_id',
#     data = gm_df))

m3.rLSM.D.v2 <- lcmm::hlme(
  rLSM ~ t,
  # random = ~ 1 + t,
  mixture = ~ t,
  ng = 3,
  subject = 'group_id',
  data = gm_df,
  B = random(m1.rLSM.D.v2)
)

# m3g.rLSM.D.v2 <- lcmm::gridsearch(
#   rep = 100, maxiter = 30, minit = m1.rLSM.D.v2, 
#   hlme(rLSM ~ t,
#        random = ~ 1 + t,
#        mixture = ~ t,
#        ng = 3,
#        subject = 'group_id',
#        data = gm_df))

m4.rLSM.D.v2 <- lcmm::hlme(
  rLSM ~ t,
  # random = ~ 1 + t,
  mixture = ~ t,
  ng = 4,
  subject = 'group_id',
  data = gm_df,
  B = random(m1.rLSM.D.v2)
)

# m4g.rLSM.D.v2 <- lcmm::gridsearch(
#   rep = 100, maxiter = 30, minit = m1.rLSM.D.v2, 
#   hlme(rLSM ~ t,
#        random = ~ 1 + t,
#        mixture = ~ t,
#        ng = 4,
#        subject = 'group_id',
#        data = gm_df))

m5.rLSM.D.v2 <- lcmm::hlme(
  rLSM ~ t,
  # random = ~ 1 + t,
  mixture = ~ t,
  ng = 5,
  subject = 'group_id',
  data = gm_df,
  B = random(m1.rLSM.D.v2)
)
# 
# m5g.rLSM.D.v2 <- lcmm::gridsearch(
#   rep = 100, maxiter = 30, minit = m1.rLSM.D.v2, 
#   hlme(rLSM ~ t,
#        random = ~ 1 + t,
#        mixture = ~ 1 + t,
#        ng = 5,
#        subject = 'group_id',
#        data = gm_df))

summarytable(m1.rLSM.D.v2,m2.rLSM.D.v2,m3.rLSM.D.v2, m4.rLSM.D.v2,m5.rLSM.D.v2,
             # m4, m4g,#m5,m5g,
             which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy","ICL", "%class"))
# summaryplot(m1.rLSM.D,m2.rLSM.D,m2g.rLSM.D,m3.rLSM.D,m3g.rLSM.D,
#             # m4, m4g,#m5,m5g,
#             which = c("BIC", "entropy","ICL"))
  
summary(m2.rLSM.D.v2)

data_pred0 <- data.frame(t = seq(0,9,length.out = 50), CEP = 0)
data_pred1 <- data.frame(t = seq(0,9,length.out = 50), CEP = 1)
pred0 <- predictY(m2.rLSM.D, data_pred0, var.time = "t")
pred1 <- predictY(m2.rLSM.D, data_pred1, var.time = "t")

plot(pred0, col=c("red","navy"), lty=1,lwd=5,ylab="normMMSE",legend=NULL,  main="Predicted trajectories for normMMSE ",ylim=c(.6,.8))
plot(pred1, col=c("red","navy"), lty=2,lwd=3,legend=NULL,add=TRUE)
legend(x="topright",legend=c("class1 :","CEP-","CEP+","class2:","CEP-","CEP+"), col=c(rep("red",3),rep("navy",3)), lwd=2, lty=c(0,1,2,0,1,2), ncol=2, bty="n", cex = 0.7)

rLSM.D.2Class <- m2.rLSM.D.v2$pprob %>%
  full_join(gm_df, by = 'group_id') %>%
  group_by(class,t) %>%
  mutate(t = t+1) %>%
  summarize(
    sd = sd(rLSM,na.rm = TRUE),
    rLSM = mean(rLSM)) %>%
  ungroup() %>%
  mutate(class = as.factor(class)) %>%
  ggplot(aes(x = as.factor(t), y = rLSM, color = class, group = class)) + geom_point() + geom_line() + ggthemes::theme_tufte() +
  # geom_errorbar(aes(ymin=rLSM-sd, ymax=rLSM+sd), width=.2,
  #               position=position_dodge(.9)
  #               ) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = 'Mean rw.rLSM.D over time within encounters by growth class',
       x = 'Decile of turns within encounter',
       y = 'Mean rw.LSM.D')

############# rLSM.P models

gm_df_p <- ECHO_smoothed_rLSM %>% # This df comes from runing the rLSM script trhough line 124 (stops before aggregating to file level)
  mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
  drop_na(text_agg) %>%
  filter(Speaker == 'P') %>%
  select(File, rLSM) %>%
  drop_na() %>%
  group_by(File) %>%
  mutate(
    id = row_number(),
    # t = case_when(id < (max(id)/5) ~ 0,
    #           id < (max(id/5))*2 ~ 1,
    #           id < (max(id/5))*3 ~ 2,
    #           id < (max(id/5))*4 ~ 3,
    #           TRUE ~ 4)
    t = case_when(id < (max(id)/10) ~ 0,
                  id < (max(id/10))*2 ~ 1,
                  id < (max(id/10))*3 ~ 2,
                  id < (max(id/10))*4 ~ 3,
                  id < (max(id/10))*5 ~ 4,
                  id < (max(id/10))*6 ~ 5,
                  id < (max(id/10))*7 ~ 6,
                  id < (max(id/10))*8 ~ 7,
                  id < (max(id/10))*9 ~ 8,
                  TRUE ~ 9)
  ) %>%
  ungroup() %>%
  select(-id) %>%
  group_by(File,t) %>%
  summarize(rLSM = mean(rLSM)) %>%
  ungroup() %>% 
  mutate(group_id = group_indices(., File))



m1.rLSM.P.v2 <- lcmm::hlme(
  rLSM ~ t,
  random = ~ 1 + t,
  subject = 'group_id',
  data = gm_df_p
)

m2.rLSM.P.v2 <- lcmm::hlme(
  rLSM ~ t,
  random = ~ 1 + t,
  mixture = ~ t,
  ng = 2,
  subject = 'group_id',
  data = gm_df_p,
  B = random(m1.rLSM.P.v2)
)

# m2g.rLSM.P.v2 <- lcmm::gridsearch(
#   rep = 100, maxiter = 30, minit = m1.rLSM.P.v2, 
#   hlme(rLSM ~ t,
#        random = ~ 1 + t,
#        mixture = ~ t,
#        ng = 2,
#        subject = 'group_id',
#        data = gm_df_p))

m3.rLSM.P.v2 <- lcmm::hlme(
  rLSM ~ t,
  random = ~ 1 + t,
  mixture = ~ t,
  ng = 3,
  subject = 'group_id',
  data = gm_df_p,
  B = random(m1.rLSM.P.v2)
)

# m3g.rLSM.P.v2 <- lcmm::gridsearch(
#   rep = 100, maxiter = 30, minit = m1.rLSM.P.v2, 
#   hlme(rLSM ~ t,
#        random = ~ 1 + t,
#        mixture = ~ t,
#        ng = 3,
#        subject = 'group_id',
#        data = gm_df_p))

m4.rLSM.P.v2 <- lcmm::hlme(
  rLSM ~ t,
  random = ~ 1 + t,
  mixture = ~ t,
  ng = 4,
  subject = 'group_id',
  data = gm_df_p,
  B = random(m1.rLSM.P.v2)
)

# m4g.rLSM.P.v2 <- lcmm::gridsearch(
#   rep = 100, maxiter = 30, minit = m1.rLSM.P.v2, 
#   hlme(rLSM ~ t,
#        random = ~ 1 + t,
#        mixture = ~ t,
#        ng = 4,
#        subject = 'group_id',
#        data = gm_df_p))

m5.rLSM.P.v2 <- lcmm::hlme(
  rLSM ~ t,
  random = ~ 1 + t,
  mixture = ~ t,
  ng = 5,
  subject = 'group_id',
  data = gm_df_p,
  B = random(m1.rLSM.P.v2)
)

# m5g.rLSM.P.v2 <- lcmm::gridsearch(
#   rep = 100, maxiter = 30, minit = m1.rLSM.P.v2, 
#   hlme(rLSM ~ t,
#        # random = ~ 1 + t,
#        mixture = ~ 1 + t,
#        ng = 5,
#        subject = 'group_id',
#        data = gm_df))

summarytable(m1.rLSM.P.v2,m2.rLSM.P.v2,m3.rLSM.P.v2,m4.rLSM.P.v2,m5.rLSM.P.v2,
             which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy","ICL", "%class"))
# summaryplot(m1.rLSM.P,m2.rLSM.P,m2g.rLSM.P,m3.rLSM.P,m3g.rLSM.P,
#             # m4, m4g,#m5,m5g,
#             which = c("BIC", "entropy","ICL"))

summary(m2.rLSM.P)

data_pred0 <- data.frame(t = seq(0,9,length.out = 50), CEP = 0)
data_pred1 <- data.frame(t = seq(0,9,length.out = 50), CEP = 1)
pred0 <- predictY(m2.rLSM.P, data_pred0, var.time = "t")
pred1 <- predictY(m2.rLSM.P, data_pred1, var.time = "t")

plot(pred0, col=c("red","navy"), lty=1,lwd=5,ylab="normMMSE",legend=NULL,  main="Predicted trajectories for normMMSE ",ylim=c(0,1))
plot(pred1, col=c("red","navy"), lty=2,lwd=3,legend=NULL,add=TRUE)
legend(x="topright",legend=c("class1 :","CEP-","CEP+","class2:","CEP-","CEP+"), col=c(rep("red",3),rep("navy",3)), lwd=2, lty=c(0,1,2,0,1,2), ncol=2, bty="n", cex = 0.7)

rLSM.P.2Class <- m2.rLSM.P.v2$pprob %>%
  full_join(gm_df, by = 'group_id') %>%
  mutate(t = t+1) %>%
  group_by(class,t) %>%
  summarize(
    sd = sd(rLSM,na.rm = TRUE),
    rLSM = mean(rLSM)) %>%
  # ungroup() %>%
  mutate(class = as.factor(class)) %>%
  ggplot(aes(x = as.factor(t), y = rLSM, color = class, group = class)) + geom_point() + geom_line() + ggthemes::theme_tufte() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = 'Mean rw.rLSM.P over time within encounters by growth class',
       x = 'Decile of turns within encounter',
       y = 'Mean rw.LSM.P')

############# classes to outcomes

class_df <- m2.rLSM.P$pprob %>%
  full_join(gm_df, by = 'group_id') %>%
  rename(class.P = class) %>%
  select(group_id, class.P, File) %>%
  full_join(m2.rLSM.D$pprob, by = 'group_id') %>%
  rename(class.D = class) %>%
  # select(-c(prob1,prob2,group_id)) %>%
  distinct()
table(class_df$class.D,class_df$class.P)

#open survey data
ECHO_survey_data <- haven::read_dta(here(config$ECHO_survey_data_path, config$ECHO_survey_data_name))

#Open ID key file
ECHO_ID_key <- read_csv('ECHO_ID_key.csv')


#merge GROWTH_MODELING_FILE and ECHO_ID_key by "File"
class_df <- left_join(class_df, ECHO_ID_key, by = "File" ) 

#merge GROWTH_MODELING_FILE and ECHO_survey_data by "tapeid" to include survey data
class_df <- left_join(class_df, ECHO_survey_data, by = "tapeid")

# Transforming cdspeak from continuous to dichotomous variable "cdspeakhigh"
class_df <- class_df %>%
  rowwise() %>%
  mutate(cdspeakhigh = ifelse(cdspeak >= 3, 1, 0))

#Making the provider id variable
class_df  <- class_df %>%
  mutate(provider_id = str_sub(File, 1, 4))


#Making the site_name variable: only three sites in data set: JC, OC, SC
class_df <- class_df %>%
  mutate(site_name = str_sub(provider_id, 1, 2))

#creating site_id variable by converting JC==0, OC==1, SC==2
class_df <- class_df %>%
  mutate(site_id = ifelse(site_name == "JC", 0,
                          ifelse(site_name == "OC", 1,
                                 2)))


#changing site_id to factor
class_df <- class_df %>%
  mutate(site_id = factor(site_id))

class_df <- class_df %>%
  mutate(provider_id = factor(provider_id))

table(class_df$class.D,class_df$cdspeakhigh)
chisq.test(class_df$class.D,class_df$cdspeakhigh, correct = FALSE)

# gginference::ggchisqtest(
#   chisq.test(class_df$class.D,class_df$cdspeakhigh, correct = FALSE)
# )
table(class_df$class.P,class_df$cdspeakhigh)
chisq.test(class_df$class.P,class_df$cdspeakhigh, correct = FALSE)

class_df <- class_df %>% mutate(x = paste0(class.D,'D_',class.P,'P'))
table(class_df$x,class_df$cdspeakhigh)
chisq.test(class_df$x,class_df$cdspeakhigh, correct = FALSE)

library(gtsummary)
class_df %>%
  select(cdspeakhigh, x) %>%
  mutate(
    cdspeakhigh = as.factor(cdspeakhigh),
    x = as.factor(x)
  ) %>%
  drop_na() %>%
  gtsummary::tbl_summary(
    by = x
  ) %>%
  gtsummary::add_p(pvalue_fun = ~style_pvalue(.x, digits = 2))


### Pulling pprob
pprob_df <- m2.rLSM.P$pprob %>%
  full_join(gm_df, by = 'group_id') %>%
  rename(class.P = class, P.prob1 = prob1, P.prob2 = prob2) %>%
  select(-t,-rLSM) %>%
  full_join(m2.rLSM.D$pprob, by = 'group_id') %>%
  rename(class.D = class, D.prob1 = prob1, D.prob2 = prob2) %>%
  select(-c(group_id)) %>%
  distinct()

write_csv(pprob_df, 'ECHO_pprob.csv')
