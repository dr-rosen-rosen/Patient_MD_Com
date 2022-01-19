LIWC_df <- read.csv(here(config$liwc_f_path,config$liwc_f_name))

speaker <- 'doctor'
p_pt <- LIWC_df %>%
  filter(Source..C. == speaker) %>%
  select(
    Analytic, Clout, Authentic, Tone, function., cogproc, percept, affect
  ) %>%
  center
  single_imputation() %>%
  # estimate_profiles(2:5,
  #                   variances = c("equal", "varying"),
  #                   covariances = c("zero", "varying")) %>%
  # compare_solutions(statistics = c("AIC", "BIC")) %>%
  estimate_profiles(2) %>%
  plot_profiles() +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = paste("LPA for raw LIWC data from ",speaker,"speech."))

library(patchwork)
p_pt / p_md / p_LSM
  
  