#############
#### ECHO Scripts for running easltic net
library(tidyverse)
library(glmnet)
library(glmnetUtils)


###################
###################

# Define the IV's and DV's


# iv <- 'provassess'
iv <- 'iptrust'
iv <- 'provcomm'
iv <- 'overcomm'


###################
###################
# IP Style
# Matching
###################
###################
iv <- 'ipstylehigh'
iv <- 'ipstyle'

# Matching
dvs <- ECHO_Matching_variables_scaled
allVars <- c(iv,dvs)
# get data set of complete cases for iv and all dvs
df_NoNA <- ECHO_All_Matching[complete.cases(ECHO_All_Matching[,allVars]),allVars]
# df_NoNA$ipstylehigh <- as.factor(df_NoNA$ipstylehigh)
# This uses everything accept the iv as a predictor
ipstyle.matching.cv.glmmod <- glmnetUtils::cv.glmnet(ipstyle ~ ., alpha = .5, data = df_NoNA, family = 'gaussian')
plot(ipstyle.matching.cv.glmmod)
# ipstyle.best_lambda <- ipstyle.cv.glmmod$lambda.min # lambda.min or lambda.1se
coef(ipstyle.matching.cv.glmmod, s = "lambda.min")

# the above tries to optimize lambad with a given alpha; 
# this extention tries to optimize both alpha and lambda
# still working on intrepreting this
# cva <- glmnetUtils::cva.glmnet(iptrust ~., data = df_NoNA, family = 'gaussian')
# plot(cva)
# predict(cva, df_NoNA,alpha = .001)



###################
###################
# IP Style
# Speech features


iv <- 'ipstylehigh'
iv <- 'ipstyle'

# Matching
dvs <- ECHO_Speech_variables_scaled
allVars <- c(iv,dvs)
# get data set of complete cases for iv and all dvs
df_NoNA <- ECHO_All_Matching[complete.cases(ECHO_All_Matching[,allVars]),allVars]
# df_NoNA$ipstylehigh <- as.factor(df_NoNA$ipstylehigh)
# This uses everything accept the iv as a predictor
ipstyle.speech.cv.glmmod <- glmnetUtils::cv.glmnet(ipstyle ~ ., alpha = .2, data = df_NoNA, family = 'gaussian')
plot(ipstyle.speech.cv.glmmod)
# ipstyle.best_lambda <- ipstyle.cv.glmmod$lambda.min # lambda.min or lambda.1se
coef(ipstyle.speech.cv.glmmod, s = "lambda.min")

# the above tries to optimize lambad with a given alpha; 
# this extention tries to optimize both alpha and lambda
# still working on intrepreting this
# cva <- glmnetUtils::cva.glmnet(iptrust ~., data = df_NoNA, family = 'gaussian')
# plot(cva)
# predict(cva, df_NoNA,alpha = .001)

###################
###################


###################
###################
# provcomm
# Matching
###################
###################
iv <- 'provcommhigh'
iv <- 'provcomm'

# Matching
dvs <- ECHO_Matching_variables_scaled
allVars <- c(iv,dvs)
# get data set of complete cases for iv and all dvs
df_NoNA <- ECHO_All_Matching[complete.cases(ECHO_All_Matching[,allVars]),allVars]
# df_NoNA$ipstylehigh <- as.factor(df_NoNA$ipstylehigh)
# This uses everything accept the iv as a predictor
provcomm.matching.cv.glmmod <- glmnetUtils::cv.glmnet(provcomm ~ ., alpha = .5, data = df_NoNA, family = 'gaussian')
plot(provcomm.matching.cv.glmmod)
# ipstyle..matching.best_lambda <- provcomm.matching.cv.glmmod$lambda.min # lambda.min or lambda.1se
coef(provcomm.matching.cv.glmmod, s = "lambda.min")

# the above tries to optimize lambad with a given alpha; 
# this extention tries to optimize both alpha and lambda
# still working on intrepreting this
# cva <- glmnetUtils::cva.glmnet(iptrust ~., data = df_NoNA, family = 'gaussian')
# plot(cva)
# predict(cva, df_NoNA,alpha = .001)



###################
###################
# provcomm
# Speech features


iv <- 'provcommhigh'
iv <- 'provcomm'

# Matching
dvs <- ECHO_Speech_variables_scaled
allVars <- c(iv,dvs)
# get data set of complete cases for iv and all dvs
df_NoNA <- ECHO_All_Matching[complete.cases(ECHO_All_Matching[,allVars]),allVars]
# df_NoNA$ipstylehigh <- as.factor(df_NoNA$ipstylehigh)
# This uses everything accept the iv as a predictor
provcomm.speech.cv.glmmod <- glmnetUtils::cv.glmnet(provcomm ~ ., alpha = .2, data = df_NoNA, family = 'gaussian')
plot(provcomm.speech.cv.glmmod)
# ipstyle.best_lambda <- ipstyle.cv.glmmod$lambda.min # lambda.min or lambda.1se
coef(provcomm.speech.cv.glmmod, s = "lambda.min")

# the above tries to optimize lambad with a given alpha; 
# this extention tries to optimize both alpha and lambda
# still working on intrepreting this
# cva <- glmnetUtils::cva.glmnet(iptrust ~., data = df_NoNA, family = 'gaussian')
# plot(cva)
# predict(cva, df_NoNA,alpha = .001)

###################
###################

