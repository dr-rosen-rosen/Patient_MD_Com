LIWC_df <- read.csv(here(config$liwc_f_path,config$liwc_f_name))

speaker <- 'doctor'
p_pt <- LIWC_df %>%
  filter(Source..C. == speaker) %>%
  select(
    # Clout, Authentic, affect, function.#Analytic,Tone,  , cogproc#, percept
    # auxverb, article, adverb, ipron, prep, negate, conj, quant, ppron
    function., Analytic, Clout, Authentic, Tone, WPS, Sixltr, Dic
  ) %>%
  mutate(across(everything(), ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE)))) %>%
  get_mahalanobis_distance(.,auto_drop = TRUE, re_center = TRUE) %>%
  single_imputation() %>%
  # estimate_profiles(2:10,
  #                   variances = c("equal", "varying"),
  #                   covariances = c("zero", "varying")) %>%
  # compare_solutions(statistics = c("AIC", "BIC")) %>%
  estimate_profiles(3) %>%
  plot_profiles() +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = paste("LPA for raw LIWC data from",speaker,"speech."))

library(patchwork)
p_pt / p_md / p_LSM
  
  