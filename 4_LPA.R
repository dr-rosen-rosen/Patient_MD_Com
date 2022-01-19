library(tidyLPA)
library(tidyverse)
library(lme4)
library(ggthemes)
library(outliers)

LSM_df <- read.csv('LIWC_df_LSM_Calculations.csv')
sjPlot::view_df(LSM_df)
outlier(LSM_df[which(LSM_df$LSM_function_mean > 0.67),'LSM_function_mean'])
p_LSM <- LSM_df %>%
  select(
    LSM_Analytic, LSM_Clout, LSM_Authentic, LSM_Tone, LSM_function_mean, LSM_cogproc, LSM_percept,LSM_affect
    ) %>%
  single_imputation() %>%
  # estimate_profiles(1:5, 
  #                   variances = c("equal", "varying"),
  #                   covariances = c("zero", "varying")) %>%
  # compare_solutions(statistics = c("AIC", "BIC"))
  estimate_profiles(3) %>%
  plot_profiles() + 
  theme_tufte() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = 'LPA for LSM across roles.')

LSM_df %>%
  select(
    #LSM_auxverb, LSM_article, LSM_adverb, LSM_ipron, LSM_prep, LSM_negate, LSM_conj, LSM_quant, LSM_ppron
    
   LSM_Analytic, LSM_Clout, LSM_Authentic, LSM_Tone, LSM_function_mean, LSM_cogproc, LSM_percept,LSM_affect
   # Analytic, Clout, Authentic, Tone, function., cogproc, percept,
    # LSM_posemo, LSM_negemo,#LSM_affect
    
    #LSM_bio
    #LSM_Sixltr, LSM_interrog, LSM_number, LSM_quant
  ) %>%
  single_imputation() %>%
  estimate_profiles(2:5) %>%
  plot_profiles() + theme_tufte() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

########## MLM
library(glca) # This is CLASS analysis (on categorical) and therefore not appropriate herre
f <- item(LSM_function_mean, LSM_Analytic, LSM_Clout, LSM_Authentic, LSM_posemo, LSM_negemo) ~ 1
lca.2 <- glca(f, data = LSM_df, nclass = 2, seed = 1, verbose = FALSE)
lca.3 <- glca(f, data = LSM_df, nclass = 3, seed = 1, verbose = FALSE)
lca.4 <- glca(f, data = LSM_df, nclass = 4, seed = 1, verbose = FALSE)
gofglca(lca.2, lca.3, lca.4, test = "boot", seed = 1)
plot(lca.4)
########## MLM
x.0 <- lmer(LSM_function_mean ~ 1 + (1|doctor_id), data = LSM_df)
summary(x.0)