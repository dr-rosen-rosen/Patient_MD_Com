#########################################
#### ECHO Scripts for LPA
#########################################
library(tidyverse)

# https://cran.r-project.org/web/packages/tidyLPA/vignettes/Introduction_to_tidyLPA.html
df_lpa <- read.csv('/Users/mrosen44/Johns Hopkins/Salar Khaleghzadegan - Patient _Provider_Communication_Projects/ECHO1_Study/ECHO_LSM_MLM_V3.csv') %>%
  mutate(provider_id = str_sub(File, 1, 4)) %>%
  filter(Speaker == 'D')
skimr::skim(df_lpa)

#########################################
library(tidyLPA)
lpa_fit <- df_lpa %>%
  #select(Analytic, Clout, Authentic) %>%
  select(Drives, Cognition, Affect, Social, Culture, Lifestyle, Physical, Perception) %>%
  # select(Cognition, Affect, Social, Physical, Perception) %>%
  single_imputation() %>%
  scale() %>%
  # estimate_profiles(2:12,
  #                   variances = c("equal", "varying"),
  #                   covariances = c("zero", "varying")) %>%
  # compare_solutions(statistics = c("AIC", "BIC")) #%>%
  estimate_profiles(4,
                   variances = 'equal',
                   covariances = 'equal')

get_estimates(lpa_fit)
lpa_fit %>%
  plot_profiles() +
  ggthemes::theme_tufte()

#########################################
library(MplusAutomation)
# https://cran.r-project.org/web/packages/MplusAutomation/vignettes/vignette.html

df_lpa %>%
  select(Cognition, Affect, Social, Physical, Perception, provider_id) %>%
  prepareMplusData(
    filename = 'ECHO_mPlus.dat'
  )
  
#########################################
library(lme4)
M_null <- lm(Social ~ 1, data = df_lpa)
test_for_md_var <- lme4::lmer(Social ~ 1 + (1|provider_id), data = df_lpa)
anova(test_for_md_var,M_null)
sjPlot::tab_model(test_for_md_var)
