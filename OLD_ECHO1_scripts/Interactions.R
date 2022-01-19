library("glinternet")

IV <- df_sub$cdspeak
DVdvs <- c(conv_LSM,TbyT_metrics)
vars <- c(IV,DVdvs)
df_NoNA <- df_sub[complete.cases(df_sub[,vars]),vars]
DVs <- df_NoNA[DVdvs]
numlvls <- rep(1,length(DVs)) # only works if there are no factor variables
fit <- glinternet(DVs, IV, numlvls)
fit$activeSet

fit.cv <- glinternet.cv(DVs, IV, numlvls, nFolds = 10, lambda=NULL, nLambda=50, lambdaMinRatio=0.01,
                        screenLimit=NULL, family=c("gaussian"), tol=1e-5, maxIter=5000,
                        verbose=FALSE, numCores=1)
#plot(fit.cv)
fit.cv$activeSet