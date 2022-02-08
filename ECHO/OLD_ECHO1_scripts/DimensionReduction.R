vars <- c(TbyT_metrics,conv_LSM)
df_NoNA <- df_sub[complete.cases(df_sub[,vars]),vars]

drops <- c("TbyT_Vader_compound_autocorr","TbyT_function_autocorr",'Conv_LSM_Avg')
df_NoNA <- df_NoNA[ , !(names(df_NoNA) %in% drops)]

test <- prcomp(df_NoNA,scale = FALSE) # this is single value decomposition and it produces a lot of factors
library(factoextra)
fviz_eig(test)


test2 <- princomp(df_NoNA, cor = FALSE, scores = TRUE) # this is spectral / eigen decomposition and produces one main factor
fviz_eig(test2)
fviz_pca_var(test2, 
             col.var = 'contrib',
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)

#x <- data.frame(merge(df_sub,test2$scores, by="row.names", all = TRUE))

library(FactoMineR)
result <- PCA(df_NoNA)


library(nFactors)
ev <- eigen(cor(df_NoNA)) # get eigenvalues
ap <- parallel(subject=nrow(df_NoNA),var=ncol(df_NoNA),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)



############################
##### Rotation
############################
library(psych)
fa.parallel(df_NoNA, fm = 'minres', fa = 'fa')
sevenFactor <- fa(df_NoNA,nfactors = 10,rotate = "oblimin",fm="minres", scores="Bartlett")
print(sevenFactor$loadings,cutoff=.3)
print(sevenFactor)

rotated <- psych::principal(df_NoNA, rotate = 'varimax', nfactors = 7, scores = TRUE, eps=1e-14)
#write.csv(rotated$loadings,'Rotated_7.csv')
print(rotated)
x <- data.frame(merge(df_sub,rotated$scores, by="row.names", all = TRUE))
