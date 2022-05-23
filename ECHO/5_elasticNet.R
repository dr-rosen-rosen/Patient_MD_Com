#############
#### ECHO Scripts for running easltic net
library(tidyverse)
library(glmnet)
library(glmnetUtils)
library(rCom)

###################
###################

# Define the IV's and DV's

iv <- 'ipstylehigh'
iv <- 'ipstyle'
iv <- 'provassess'
iv <- 'iptrust'
iv <- 'provcomm'

dvs <- ECHO_Matching_variables
allVars <- c(iv,dvs)
# get data set of complete cases for iv and all dvs
df_NoNA <- ECHO_All_Matching[complete.cases(ECHO_All_Matching[,allVars]),allVars]
# This uses everything accept the iv as a predictor
cv.glmmod <- cv.glmnet(iptrust ~ ., alpha = .4, data = df_NoNA, family = 'gaussian') 
plot(cv.glmmod)
best_lambda <- cv.glmmod$lambda.min # lambda.min or lambda.1se
coef(cv.glmmod, s = "lambda.min")

# the above tries to optimize lambad with a given alpha; 
# this extention tries to optimize both alpha and lambda
# still working on intrepreting this
cva <- cva.glmnet(iptrust ~., data = df_NoNA, family = 'gaussian')
plot(cva)
predict(cva, df_NoNA,alpha = .001)
