##########################################################################
##########################################################################
################# Versions of Factor Analysis for lexical data
##########################################################################
##########################################################################
library(factoextra)
library(FactoMineR)
require(Factoshiny)

LIWC_df <- read.csv(here(config$liwc_f_path,config$liwc_f_name))
DR_LIWC_df_nona <- LIWC_df %>%
  filter(Source..C. == 'doctor') %>%
  select(-one_of(c("Source..A.","Source..B.","Source..C."))) %>%
  filter(complete.cases(.)) %>%
  scale(., center = TRUE, scale = TRUE)
PT_LIWC_df_nona <- LIWC_df %>%
  filter(Source..C. == 'patient') %>%
  select(-one_of(c("Source..A.","Source..B.","Source..C."))) %>%
  filter(complete.cases(.)) %>%
  scale(., center = TRUE, scale = TRUE)


# this is spectral / eigen decomposition
DR_spec_eigen_decomp <- princomp(DR_LIWC_df_nona, cor = FALSE, scores = TRUE)
fviz_eig(DR_sv_decomp)
fviz_pca_var(DR_sv_decomp, 
             col.var = 'contrib',
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)
PT_spec_eigen_decomp <- princomp(PT_LIWC_df_nona, cor = FALSE, scores = TRUE)
fviz_eig(PT_sv_decomp)
fviz_pca_var(PT_sv_decomp, 
             col.var = 'contrib',
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)

# Using factominer
DR_PCA <- PCA(DR_LIWC_df_nona)
summary(DR_PCA)
Factoshiny::Factoshiny(DR_PCA)
PT_PCA <- PCA(PT_LIWC_df_PT_nona)
summary(PT_PCA)
Factoshiny::Factoshiny(PT_PCA)

library(nFactors)
DR_ev <- eigen(cor(DR_LIWC_df_nona)) # get eigenvalues
DR_ap <- parallel(subject=nrow(DR_LIWC_df_nona),var=ncol(DR_LIWC_df_nona),
               rep=100,quantile =.05)
DR_nS <- nScree(x=DR_ev$values, aparallel=DR_ap$eigen$qevpea)
plotnScree(DR_nS)

PT_ev <- eigen(cor(PT_LIWC_df_nona)) # get eigenvalues
PT_ap <- parallel(subject=nrow(PT_LIWC_df_nona),var=ncol(PT_LIWC_df_nona),
                  rep=100,quantile =.05)
PT_nS <- nScree(x=PT_ev$values, aparallel=PT_ap$eigen$qevpea)
plotnScree(PT_nS)


############################
##### Rotation
############################
library(psych)
fa.parallel(PT_LIWC_df_nona, fm = 'minres', fa = 'fa')
sevenFactor <- fa(df_NoNA,nfactors = 10,rotate = "oblimin",fm="minres", scores="Bartlett")
print(sevenFactor$loadings,cutoff=.3)
print(sevenFactor)

rotated <- psych::principal(df_NoNA, rotate = 'varimax', nfactors = 7, scores = TRUE, eps=1e-14)
#write.csv(rotated$loadings,'Rotated_7.csv')
print(rotated)
x <- data.frame(merge(df_sub,rotated$scores, by="row.names", all = TRUE))