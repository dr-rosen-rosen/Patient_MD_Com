##########################################################################
##########################################################################
################# Versions of Factor Analysis for lexical data
##########################################################################
##########################################################################
library(factoextra)
library(FactoMineR)
require(Factoshiny)
library(EFAtools)

LIWC_df <- read.csv(here(config$liwc_f_path,config$liwc_f_name)) %>%
  #select(-one_of(c("AllPunc","Period","Comma","Colon","SemiC","QMark","Exclam","Dash","Quote","Apostro","Parenth","OtherP")))  
  LIWC_Final_Inclusion()

# Create subsets for DR and PT speech only (for separate FAs); then scale / center each variable
DR_LIWC_df_nona <- subset_and_center(
  df = LIWC_df,
  role = 'doctor'
)
PT_LIWC_df_nona <- subset_and_center(
  df = LIWC_df,
  role = 'patient'
)
# Select the subset of variables that are appropriate for inclusion in Factor Analysis
# This will drop all variables with a 
DR_LIWC_df_nona <- prep_items_for_FA(
  df = DR_LIWC_df_nona,
  cutoff = .7
)
PT_LIWC_df_nona <- prep_items_for_FA(
  df = PT_LIWC_df_nona,
  cutoff = .7
)

print('Elements in DR, but not PT...')
print(setdiff(names(DR_LIWC_df_nona), names(PT_LIWC_df_nona)))

print('Elements in PT, but not DR...')
print(setdiff(names(PT_LIWC_df_nona), names(DR_LIWC_df_nona)))

print("common variables")
print(length(intersect(names(PT_LIWC_df_nona),names(DR_LIWC_df_nona))))
##########################################################################
################# EFA Tools (new) Versions of Factor Analysis for lexical data
##########################################################################
########## PATIENT DATA
# Bartlett's test of sphericity
EFAtools::BARTLETT(PT_LIWC_df_nona)
# KMO criterion
KMO(PT_LIWC_df_nona)
# get estimates of number of factors
EFAtools::N_FACTORS(PT_LIWC_df_nona, method = "ULS")
# run EFA
PT_EFA <- EFA(PT_LIWC_df_nona, n_factors = 3, rotation = "promax", method = 'ULS')
PT_EFA
PT_EFA$rot_loadings
# EFAtools::COMPARE(
#   EFA(PT_LIWC_df_nona, n_factors = 8, rotation = "promax")$rot_loadings,
#   EFA(PT_LIWC_df_nona, n_factors = 8, rotation = "varimax")$rot_loadings
# )

########## MD DATA
# Bartlett's test of sphericity
EFAtools::BARTLETT(DR_LIWC_df_nona)
# KMO criterion
KMO(DR_LIWC_df_nona)
# get estimates of number of factors
EFAtools::N_FACTORS(DR_LIWC_df_nona, method = "ULS")
# run EFA
DR_EFA <- EFA(DR_LIWC_df_nona, n_factors = 5, rotation = "promax", method = 'ULS')
DR_EFA
DR_EFA$rot_loadings




##########################################################################
################# OLD Versions of Factor Analysis for lexical data
##########################################################################


# this is spectral / eigen decomposition
DR_spec_eigen_decomp <- princomp(DR_LIWC_df_nona, cor = FALSE, scores = TRUE)
fviz_eig(DR_spec_eigen_decomp)
fviz_pca_var(DR_spec_eigen_decomp, 
             col.var = 'contrib',
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)
PT_spec_eigen_decomp <- princomp(PT_LIWC_df_nona, cor = FALSE, scores = TRUE)
fviz_eig(PT_spec_eigen_decomp)
fviz_pca_var(PT_spec_eigen_decomp, 
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
DR_ap <- nFactors::parallel(subject=nrow(DR_LIWC_df_nona),var=ncol(DR_LIWC_df_nona),
               rep=100,quantile =.05)
DR_nS <- nFactors::nScree(x=DR_ev$values, aparallel=DR_ap$eigen$qevpea)
nFactors::plotnScree(DR_nS)

PT_ev <- eigen(cor(PT_LIWC_df_nona)) # get eigenvalues
PT_ap <- nFactors::parallel(subject=nrow(PT_LIWC_df_nona),var=ncol(PT_LIWC_df_nona),
                  rep=100,quantile =.05)
PT_nS <- nFactors::nScree(x=PT_ev$values, aparallel=PT_ap$eigen$qevpea)
nFactors::plotnScree(PT_nS)


############################
##### Rotation
############################
library(psych)
fa.parallel(DR_LIWC_df_nona, fm = 'minres', fa = 'fa')
ThirteenFactor <- fa(DR_LIWC_df_nona,nfactors = 9,rotate = "oblimin",fm="minres", scores="Bartlett")
print(ThirteenFactor$loadings,cutoff=.3)
print(ThirteenFactor)

library(REdaS)
x <- DR_LIWC_df_nona %>%
  # select(-one_of(c('Dic','Authentic','ppron','pronoun','i','we','you','shehe','article','adverb','conj',
  #                  'anx','anger','sad','insight','see','hear','sexual','ingest','affiliation','achieve','focuspast',
  #                  'focusfuture','leisure'))) %>%
  REdaS::KMOS(., use = "pairwise.complete.obs")

REdaS::bart_spher(DR_LIWC_df_nona)

rotated <- psych::principal(df_NoNA, rotate = 'varimax', nfactors = 7, scores = TRUE, eps=1e-14)
#write.csv(rotated$loadings,'Rotated_7.csv')
print(rotated)
x <- data.frame(merge(df_sub,rotated$scores, by="row.names", all = TRUE))