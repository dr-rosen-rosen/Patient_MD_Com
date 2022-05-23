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
###################
###################
iv <- 'ipstylehigh'
iv <- 'ipstyle'

# Matching
dvs <- ECHO_Matching_variables
allVars <- c(iv,dvs)
# get data set of complete cases for iv and all dvs
df_NoNA <- ECHO_All_Matching[complete.cases(ECHO_All_Matching[,allVars]),allVars]
# df_NoNA$ipstylehigh <- as.factor(df_NoNA$ipstylehigh)
# This uses everything accept the iv as a predictor
ipstyle.cv.glmmod <- glmnetUtils::cv.glmnet(ipstyle ~ ., alpha = .2, data = df_NoNA, family = 'gaussian')
plot(ipstyle.cv.glmmod)
# ipstyle.best_lambda <- ipstyle.cv.glmmod$lambda.min # lambda.min or lambda.1se
coef(ipstyle.cv.glmmod, s = "lambda.min")

# the above tries to optimize lambad with a given alpha; 
# this extention tries to optimize both alpha and lambda
# still working on intrepreting this
# cva <- glmnetUtils::cva.glmnet(iptrust ~., data = df_NoNA, family = 'gaussian')
# plot(cva)
# predict(cva, df_NoNA,alpha = .001)

# Speach features

###################
###################
# IP Style
###################
###################

