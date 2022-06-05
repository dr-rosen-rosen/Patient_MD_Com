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
#iv <- 'ipstyle'

# Matching
dvs <- ECHO_Matching_variables_scaled
allVars <- c(iv,dvs)
# get data set of complete cases for iv and all dvs
df_NoNA <- ECHO_All_Matching[complete.cases(ECHO_All_Matching[,allVars]),allVars]
# df_NoNA$ipstylehigh <- as.factor(df_NoNA$ipstylehigh)
# This uses everything accept the iv as a predictor
ipstyle.matching.cv.glmmod <- glmnetUtils::cv.glmnet(ipstylehigh ~ ., alpha = .5, data = df_NoNA, family = 'binomial')
plot(ipstyle.matching.cv.glmmod)
# ipstyle.best_lambda <- ipstyle.cv.glmmod$lambda.min # lambda.min or lambda.1se
coef(ipstyle.matching.cv.glmmod, s = "lambda.min")

# the above tries to optimize lambad with a given alpha; 
# this extention tries to optimize both alpha and lambda
# still working on intrepreting this
# cva <- glmnetUtils::cva.glmnet(iptrust ~., data = df_NoNA, family = 'gaussian')
# plot(cva)
# predict(cva, df_NoNA,alpha = .001)


coef_df <- data.frame(coef_df.name = dimnames(coef(ipstyle.matching.cv.glmmod, s = "lambda.1se"))[[1]], coef_df.value = matrix(coef(ipstyle.matching.cv.glmmod, s = "lambda.min")))
coef_df <- subset(coef_df, abs(coef_df$coef_df.value) > .03)
coef_list.ipstyle.matching <- as.vector(coef_df[-1,'coef_df.name'])
coef_list.ipstyle.matching <- paste(coef_list.ipstyle.matching,collapse = '+')

###################
###################
# IP Style
# Speech features


iv <- 'ipstylehigh'
#iv <- 'ipstyle'

# Matching
dvs <- ECHO_Speech_variables_scaled
allVars <- c(iv,dvs)
# get data set of complete cases for iv and all dvs
df_NoNA <- ECHO_All_Matching[complete.cases(ECHO_All_Matching[,allVars]),allVars]
# df_NoNA$ipstylehigh <- as.factor(df_NoNA$ipstylehigh)
# This uses everything accept the iv as a predictor
ipstyle.speech.cv.glmmod <- glmnetUtils::cv.glmnet(ipstylehigh ~ ., alpha = .5, data = df_NoNA, family = 'binomial')
plot(ipstyle.speech.cv.glmmod)
# ipstyle.best_lambda <- ipstyle.cv.glmmod$lambda.min # lambda.min or lambda.1se
coef(ipstyle.speech.cv.glmmod, s = "lambda.min")

# the above tries to optimize lambad with a given alpha; 
# this extention tries to optimize both alpha and lambda
# still working on intrepreting this
# cva <- glmnetUtils::cva.glmnet(iptrust ~., data = df_NoNA, family = 'gaussian')
# plot(cva)
# predict(cva, df_NoNA,alpha = .001)


#ipstylehigh
coef_df <- data.frame(coef_df.name = dimnames(coef(ipstyle.speech.cv.glmmod, s = "lambda.1se"))[[1]], coef_df.value = matrix(coef(ipstyle.speech.cv.glmmod, s = "lambda.min")))
coef_df <- subset(coef_df, abs(coef_df$coef_df.value) > .03)
coef_list.ipstyle.speech <- as.vector(coef_df[-1,'coef_df.name'])
coef_list.ipstyle.speech <- paste(coef_list.ipstyle.speech,collapse = '+')

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
provcomm.matching.cv.glmmod <- glmnetUtils::cv.glmnet(provcommhigh ~ ., alpha = .5, data = df_NoNA, family = 'binomial')
plot(provcomm.matching.cv.glmmod)
# ipstyle..matching.best_lambda <- provcomm.matching.cv.glmmod$lambda.min # lambda.min or lambda.1se
coef(provcomm.matching.cv.glmmod, s = "lambda.min")

# the above tries to optimize lambad with a given alpha; 
# this extention tries to optimize both alpha and lambda
# still working on intrepreting this
# cva <- glmnetUtils::cva.glmnet(iptrust ~., data = df_NoNA, family = 'gaussian')
# plot(cva)
# predict(cva, df_NoNA,alpha = .001)



coef_df <- data.frame(coef_df.name = dimnames(coef(provcomm.matching.cv.glmmod, s = "lambda.1se"))[[1]], coef_df.value = matrix(coef(provcomm.matching.cv.glmmod, s = "lambda.min")))
coef_df <- subset(coef_df, abs(coef_df$coef_df.value) > .03)
coef_list.provcomm.matching <- as.vector(coef_df[-1,'coef_df.name'])
coef_list.provcomm.matching <- paste(coef_list.provcomm.matching,collapse = '+')


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
provcomm.speech.cv.glmmod <- glmnetUtils::cv.glmnet(provcommhigh ~ ., alpha = .5, data = df_NoNA, family = 'binomial')
plot(provcomm.speech.cv.glmmod)
# ipstyle.best_lambda <- ipstyle.cv.glmmod$lambda.min # lambda.min or lambda.1se
coef(provcomm.speech.cv.glmmod, s = "lambda.min")

# the above tries to optimize lambad with a given alpha; 
# this extention tries to optimize both alpha and lambda
# still working on intrepreting this
# cva <- glmnetUtils::cva.glmnet(iptrust ~., data = df_NoNA, family = 'gaussian')
# plot(cva)
# predict(cva, df_NoNA,alpha = .001)


coef_df <- data.frame(coef_df.name = dimnames(coef(provcomm.speech.cv.glmmod, s = "lambda.1se"))[[1]], coef_df.value = matrix(coef(provcomm.speech.cv.glmmod, s = "lambda.min")))
coef_df <- subset(coef_df, abs(coef_df$coef_df.value) > .03)
coef_list.provcomm.speech <- as.vector(coef_df[-1,'coef_df.name'])
coef_list.provcomm.speech <- paste(coef_list.provcomm.speech,collapse = '+')

###################
###################



###################
###################
#cdspeak
# Matching
###################
###################
iv <- 'cdspeak'


# Matching
dvs <- ECHO_Matching_variables_scaled
allVars <- c(iv,dvs)
# get data set of complete cases for iv and all dvs
df_NoNA <- ECHO_All_Matching[complete.cases(ECHO_All_Matching[,allVars]),allVars]
# df_NoNA$ipstylehigh <- as.factor(df_NoNA$ipstylehigh)
# This uses everything accept the iv as a predictor
cdspeak.matching.cv.glmmod <- glmnetUtils::cv.glmnet(cdspeak ~ ., alpha = .5, data = df_NoNA, family = 'gaussian')
plot(cdspeak.matching.cv.glmmod)
# ipstyle..matching.best_lambda <- provcomm.matching.cv.glmmod$lambda.min # lambda.min or lambda.1se
coef(cdspeak.matching.cv.glmmod, s = "lambda.min")

# the above tries to optimize lambad with a given alpha; 
# this extention tries to optimize both alpha and lambda
# still working on intrepreting this
# cva <- glmnetUtils::cva.glmnet(iptrust ~., data = df_NoNA, family = 'gaussian')
# plot(cva)
# predict(cva, df_NoNA,alpha = .001)




coef_df <- data.frame(coef_df.name = dimnames(coef(cdspeak.matching.cv.glmmod, s = "lambda.1se"))[[1]], coef_df.value = matrix(coef(cdspeak.matching.cv.glmmod, s = "lambda.min")))
coef_df <- subset(coef_df, abs(coef_df$coef_df.value) > .03)
coef_list.cdspeak.matching <- as.vector(coef_df[-1,'coef_df.name'])
coef_list.cdspeak.matching <- paste(coef_list.cdspeak.matching,collapse = '+')

###################
###################
# cdspeak
# Speech features


iv <- 'cdspeak'

# Speech features
dvs <- ECHO_Speech_variables_scaled
allVars <- c(iv,dvs)
# get data set of complete cases for iv and all dvs
df_NoNA <- ECHO_All_Matching[complete.cases(ECHO_All_Matching[,allVars]),allVars]
# df_NoNA$ipstylehigh <- as.factor(df_NoNA$ipstylehigh)
# This uses everything accept the iv as a predictor
cdspeak.speech.cv.glmmod <- glmnetUtils::cv.glmnet(cdspeak ~ ., alpha = .5, data = df_NoNA, family = 'gaussian')
plot(cdspeak.speech.cv.glmmod)
# cdspeak.best_lambda <- cdspeak.cv.glmmod$lambda.min # lambda.min or lambda.1se
coef(cdspeak.speech.cv.glmmod, s = "lambda.min")

# the above tries to optimize lambad with a given alpha; 
# this extention tries to optimize both alpha and lambda
# still working on intrepreting this
# cva <- glmnetUtils::cva.glmnet(iptrust ~., data = df_NoNA, family = 'gaussian')
# plot(cva)
# predict(cva, df_NoNA,alpha = .001)



coef_df <- data.frame(coef_df.name = dimnames(coef(cdspeak.speech.cv.glmmod, s = "lambda.1se"))[[1]], coef_df.value = matrix(coef(cdspeak.speech.cv.glmmod, s = "lambda.min")))
coef_df <- subset(coef_df, abs(coef_df$coef_df.value) > .03)
coef_list.cdspeak.speech <- as.vector(coef_df[-1,'coef_df.name'])
coef_list.cdspeak.speech <- paste(coef_list.cdspeak.speech,collapse = '+')

###################
###################
