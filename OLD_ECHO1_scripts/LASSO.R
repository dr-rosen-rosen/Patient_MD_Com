library("glmnet")


#IV <- 'cdspeak'
#IV <- 'cdreason'
#IV <- 'cdstyle'
#IV <- 'cdvalue'
#IV <- 'cdspirit'
#IV <- 'cdethnic'
#IV <- 'cdtype'
#IV <- 'cdrace'
#IV <- 'cdculture'
#IV <- 'cdskin'

#IV <- 'cultdissmd1'
#IV <- 'cultdissmd2'
#IV <- 'cultdissmd3'
#IV <- 'cultdissmd4'
#IV <- 'cultdissmd5'
#IV <- 'cultdissmd6'
#IV <- 'cultdissmd7'
#IV <- 'cultdissmd8'
#IV <- 'cultdissmd9'
#IV <- 'cultdissmd10'

#IV <- 'racecat2'
IV <- 'ccomp'
IV <- 'ccomptert'
IV <- 'iptrust'
IV <- 'provknowcat'
IV <- 'provcomm'
IV <- 'provcommtert'
IV <- 'ipstyle'
IV <- 'ipstyletert'
IV <- 'provassess'


DVs <- c(conv_LSM,TbyT_metrics)#,c('RC1','RC2','RC3','RC4','RC5','RC6','RC7'))

vars <- c(DVs,IV)
df_NoNA <- df_sub[complete.cases(df_sub[,vars]),vars]
y <- as.vector(df_NoNA[,IV])
x <- data.matrix(df_NoNA[,DVs], rownames.force=TRUE)

cv.glmmod <- cv.glmnet(x=x,y=y,alpha=.4)
plot(cv.glmmod)
best_lambda <- cv.glmmod$lambda.min # lambda.min or lambda.1se
coef(cv.glmmod, s = "lambda.min")
#summary(cv.glmmod)
#coefficients(best_lambda)

tmp_coeffs <- coef(cv.glmmod, s = "lambda.min")
data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)