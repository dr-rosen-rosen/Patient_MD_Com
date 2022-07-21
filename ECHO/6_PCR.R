#############
#### Attempt at PCR for ECHO data

library(pls)

###################
###################
# IP Style
# Matching
###################
###################
# iv <- 'ipstylehigh'
iv <- 'ipstyle'

# Matching
dvs <- c(ECHO_tbyt_rLSM_variables_scaled,ECHO_conv_LSM_variables_scaled,
         ECHO_conv_LSM_chunks_turns_variables_scaled,
         ECHO_tbyt_rLSM_chunks_turns_variables_scaled)   #ECHO_Matching_variables_scaled
allVars <- c(iv,dvs)
# get data set of complete cases for iv and all dvs
df_NoNA <- ECHO_All_Matching[complete.cases(ECHO_All_Matching[,allVars]),allVars]
set.seed(1)
f <- as.formula(paste0(iv,' ~ ',paste(dvs,collapse = ' + ')))
m.pcr.matching <- pcr(f, data = df_NoNA, scale = FALSE, validation = "CV")
summary(m.pcr.matching)
validationplot(m.pcr.matching)
validationplot(m.pcr.matching, val.type="MSEP")
validationplot(m.pcr.matching, val.type="R2")


###################
###################
# provcomm
# Matching
###################
###################
iv <- 'provcommhigh'
# iv <- 'provcomm'

# Matching
dvs <- c(ECHO_tbyt_rLSM_variables_scaled,ECHO_conv_LSM_variables_scaled,
         ECHO_conv_LSM_chunks_turns_variables_scaled,
         ECHO_tbyt_rLSM_chunks_turns_variables_scaled)   #ECHO_Matching_variables_scaled
allVars <- c(iv,dvs)
# get data set of complete cases for iv and all dvs
df_NoNA <- ECHO_All_Matching[complete.cases(ECHO_All_Matching[,allVars]),allVars]
set.seed(1)
f <- as.formula(paste0(iv,' ~ ',paste(dvs,collapse = ' + ')))
m.pcr.matching <- plsr(f, data = df_NoNA, scale = FALSE, validation = "CV")
summary(m.pcr.matching)
validationplot(m.pcr.matching)
validationplot(m.pcr.matching, val.type="MSEP")
validationplot(m.pcr.matching, val.type="R2")
