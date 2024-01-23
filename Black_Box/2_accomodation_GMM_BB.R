############################################################################################################
############# Growth modeling for cross study data
############################################################################################################

library(tidyverse)
library(config)
library(here)
library(lcmm)
# Sys.setenv(R_CONFIG_ACTIVE = "mike") # 'default')#
# config <- config::get()
demo_df <- read.csv(here('/Users/mrosen44/Johns\ Hopkins/Salar\ Khaleghzadegan\ -\ Patient\ _Provider_Communication_Projects/cross_study_analyses/profile_data_3_L1_and_3_L2_combined_trait_role.csv'))
demo_df <- demo_df %>% 
  filter(patient_race %in% c('Black/African American','White/Caucasian')) %>%
  mutate(File = paste0(study,'_',file)) %>%
  select(File, patient_race) %>%
  rename(pt_race = patient_race)
table(demo_df$patient_race)

bb_gm_df <- read.csv(here('/Users/mrosen44/Johns\ Hopkins/Salar\ Khaleghzadegan\ -\ Patient\ _Provider_Communication_Projects/cross_study_analyses/combined_smoothed_rw.rLSM.csv')) %>%
  filter(study == 'BB')

table(bb_gm_df$Speaker)

bb_gm_df <- bb_gm_df %>%
  group_by(File) %>%
  filter(n() > 100) %>% ungroup() %>% # drops all transctips with fewer than 100 total turns
  group_by(File) %>%
  slice(15:n()) %>% # drops leading rows with no data (this is for a window of 8... frist 7 rows for both speakers are empty)
  ungroup() %>%
  mutate(
    File = paste0(study,'_',File),
    pcp_id = paste0(study,'_',provider_id))

tot_wc_df <- bb_gm_df %>%
  select(File, Speaker, WC) %>%
  group_by(File) %>%
  summarise(tot_wc = sum(WC)) %>%
  ungroup()
  # pivot_wider(id_cols = File, values_from = WC, names_from = Speaker)

# bb_gm_df %>% group_by(File) %>% summarize(n = n()) %>% min()
## Need to drop the leading NA's for each file. 
############# rLSM.D models



############################################################################################################
############# Using MPLUS; Example 10.10 seems close to what we need
############################################################################################################

library(MplusAutomation)

bb_gm_df3 <- bb_gm_df %>%
  drop_na(text_agg) %>%
  filter(Speaker == 'doctor') %>%
  select(File,pcp_id,rw.rLSM) %>%
  group_by(File) %>%
  mutate(
    id = row_number(),
    t = case_when(id < (max(id)/4) ~ 0,
              id < (max(id/4))*2 ~ 1,
              id < (max(id/4))*3 ~ 2,
              TRUE ~ 3)
    # t = case_when(id < (max(id)/10) ~ 0,
    #               id < (max(id/10))*2 ~ 1,
    #               id < (max(id/10))*3 ~ 2,
    #               id < (max(id/10))*4 ~ 3,
    #               id < (max(id/10))*5 ~ 4,
    #               id < (max(id/10))*6 ~ 5,
    #               id < (max(id/10))*7 ~ 6,
    #               id < (max(id/10))*8 ~ 7,
    #               id < (max(id/10))*9 ~ 8,
    #               TRUE ~ 9)
  ) %>%
  ungroup() %>%
  select(-id) %>%
  group_by(File,t) %>%
  summarize(rLSM = mean(rw.rLSM)) %>%
  ungroup() %>%
  left_join(unique(bb_gm_df[c('pcp_id','File')]), by = 'File',relationship = "many-to-many") %>%
  pivot_wider(
    # id_cols = File, 
    values_from = rLSM, 
    names_from = t,
    names_prefix = "rLSM_t") %>%
  mutate(row_num = row_number()) %>%
  left_join(tot_wc_df, by = 'File') #%>%
  # right_join(demo_df, by = 'File')
  #mutate(group_id = group_indices(., File))

skimr::skim(bb_gm_df3)
skimr::skim(demo_df)


bb_gm_df4 <- bb_gm_df3 %>% 
  # select(-File,-pcp_id,-tot_wc) 
  group_by(pcp_id) %>% 
  filter(length(unique(File)) > 4) %>%
  ungroup() %>%
  select(-File,-tot_wc)
# bb_gm_df4 %>%
#   prepareMplusData(
#   filename = 'cross_study_analysis/mplus/crossStudyAccom.dat'
# )

print(length(unique(bb_gm_df4$pcp_id)))

# bb_gm_df4 <- bb_gm_df4 %>% 


bb_gm_df4 <- bb_gm_df4 %>%
  select(starts_with('rLSM_t')) %>%
  careless::mahad() %>% #+ geom_hline(yintercept = 40, color = "red")
  cbind(bb_gm_df4) %>%
  rename('mahad' = '.') %>%
  filter(mahad < 40) %>%
  select(-mahad)

###############################
######## APPRAOCH FROM WICKRAMA et al
################################


# Build LCGAs first

lapply(1:10, function(k)
{
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("LCGA_{k};"),
    DATA = 'LISTWISE = ON;',
    VARIABLE = glue::glue("IDVARIABLE IS row_num;\nCLASSES = c({k});\n"),
    # DEFINE = "STANDARDIZE rLSM_t0 rLSM_t1 rLSM_t2 rLSM_t3 rLSM_t4\nrLSM_t5 rLSM_t6 rLSM_t7 rLSM_t8 rLSM_t9;",
    DEFINE = "STANDARDIZE rLSM_t0 rLSM_t1 rLSM_t2 rLSM_t3;",
    ANALYSIS = "TYPE = MIXTURE;
    STARTS=1000 200;
    STITERATIONS=50;
    LRTBOOTSTRAP = 100;
    PROCESSORS=6;",
    MODEL = glue::glue(
      '%OVERALL%\n',
      'i s | rLSM_t0@0 rLSM_t1@1 rLSM_t2@2 rLSM_t3@3\n'#,
      # ' rLSM_t4@4 rLSM_t5@5 rLSM_t6@6 rLSM_t7@7 rLSM_t8@8 rLSM_t9@9;\n',
      # 'i-s@0;\n'
      ),
    OUTPUT = "SAMPSTAT STANDARDIZED MOD (5.00) TECH7 TECH11 TECH13 TECH14;",
    SAVEDATA = glue::glue("FILE IS LCGA_{k}_out.dat;\n","save=cprob;"),
    # PLOT = "type = plot3;",
    usevariables = colnames(bb_gm_df4),#'rLSM_t0 rLSM_t1 rLSM_t2 rLSM_t3 rLSM_t4 rLSM_t5 rLSM_t6 rLSM_t7 rLSM_t8 rLSM_t9 tot_wc',
    rdata = bb_gm_df4
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("Black_Box/mplus/gmm/wickrama/lcga/LCGA_{k}.dat"),
                                              modelout = glue::glue("Black_Box/mplus/gmm/wickrama/lcga/LCGA_{k}.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
})

beepr::beep()

# Build GMMs next
class_str <- character()
lapply(1:10, function(k)
{
  
    for (x in 1:k) {
    if (x == 1) {
      
      class_str <<- glue::glue("%C#{x}%\n",
                               "[i-s] (M1-M2);\n",
                               "i-s (V1-V2);\n",
                               "i WITH s (COV1);\n")
    } else {
      first <- (x*2)-1
      second <- (x*2)
      print(first)
      print(second)
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%\n",
                                     "[i-s] (M{first}-M{second});\n",
                                     "i-s (V{first}-V{second});\n",
                                     "i WITH s (COV{x});\n"
                                     ), sep = '\n')
    }
    print(class_str)
  }


  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("GMM_{k};"),
    DATA = 'LISTWISE = ON;',
    VARIABLE = glue::glue("IDVARIABLE IS row_num;\nCLASSES = c({k});\n"),
    # DEFINE = "STANDARDIZE rLSM_t0 rLSM_t1 rLSM_t2 rLSM_t3 rLSM_t4\nrLSM_t5 rLSM_t6 rLSM_t7 rLSM_t8 rLSM_t9;",
    DEFINE = "STANDARDIZE rLSM_t0 rLSM_t1 rLSM_t2 rLSM_t3;",
    ANALYSIS = "TYPE = MIXTURE;
    STARTS=1000 200;
    STITERATIONS=50;
    LRTBOOTSTRAP = 100;
    PROCESSORS=6;",
    MODEL = glue::glue(
      '%OVERALL%\n',
      'i s | rLSM_t0@0 rLSM_t1@1 rLSM_t2@2 rLSM_t3@3;\n',
      # 'i s ON pt_race;\n',
      # 'rLSM_t5@5 rLSM_t6@6 rLSM_t7@7 rLSM_t8@8 rLSM_t9@9;\n',
      class_str),
    OUTPUT = "SAMPSTAT STANDARDIZED MOD (5.00) TECH7 TECH11 TECH13 TECH14;",
    SAVEDATA = glue::glue("FILE IS GMM_{k}_out.dat;\n","save=cprob;"),
    # PLOT = "type = plot3;",
    usevariables = colnames(bb_gm_df4),#'rLSM_t0 rLSM_t1 rLSM_t2 rLSM_t3 rLSM_t4 rLSM_t5 rLSM_t6 rLSM_t7 rLSM_t8 rLSM_t9 tot_wc',
    rdata = bb_gm_df4
  )

  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("Black_Box/mplus/gmm/wickrama/gmm_4/GMM_{k}.dat"),
                                              modelout = glue::glue("Black_Box/mplus/gmm/wickrama/gmm_4/GMM_{k}.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
})
beepr::beep()

output_enum <- readModels(here("Black_Box/mplus/gmm/wickrama/gmm_4"), quiet = TRUE)
enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols=c("Title",
                                             "LL",
                                             "BIC",
                                             "aBIC"),
                                  sortBy = "Title")
enum_summary %>%
  gt::gt()

enum_summary %>%
  as.data.frame() %>%
  # filter(str_detect(Title, pattern = 'GMM_')) %>%
  pivot_longer(
    !Title, names_to = 'criteria', values_to = 'value'
  ) %>%
  mutate(
    num_profiles = readr::parse_number(str_remove(Title,'GMM_'))
  ) %>%
  filter(criteria != 'LL') %>%
  ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
  geom_point() + geom_line() +
  scale_x_continuous(breaks= scales::pretty_breaks()) + labs(title='Means and variances')


# Quadratic L1 search
class_str <- character()
lapply(1:10, function(k)
{
  
  for (x in 1:k) {
    if (x == 1) {
      
      class_str <<- glue::glue("%C#{x}%\n",
                               "[i-s] (M1-M2);\n",
                               "i-s (V1-V2);\n",
                               # "[i s q];\n",
                               # "i s q;\n",
                               "i WITH s (COV1);\n")
    } else {
      first <- (x*2)-1
      second <- (x*2)
      print(first)
      print(second)
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%\n",
                                     "[i-s] (M{first}-M{second});\n",
                                     "i-s (V{first}-V{second});\n",
                                     # "[i s q];\n",
                                     # "i s q;\n",
                                     "i WITH s (COV{x});\n"
                          ), sep = '\n')
    }
    print(class_str)
  }
  
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("GMM_{k};"),
    DATA = 'LISTWISE = ON;',
    VARIABLE = glue::glue("IDVARIABLE IS row_num;\nCLASSES = c({k});\n"),
    # DEFINE = "STANDARDIZE rLSM_t0 rLSM_t1 rLSM_t2 rLSM_t3 rLSM_t4\nrLSM_t5 rLSM_t6 rLSM_t7 rLSM_t8 rLSM_t9;",
    DEFINE = "STANDARDIZE rLSM_t0 rLSM_t1 rLSM_t2 rLSM_t3;",
    ANALYSIS = "TYPE = MIXTURE;
    STARTS=1000 200;
    STITERATIONS=50;
    LRTBOOTSTRAP = 100;
    PROCESSORS=6;",
    MODEL = glue::glue(
      '%OVERALL%\n',
      'i s | rLSM_t0@0 rLSM_t1@1 rLSM_t2@2 rLSM_t3@3;\n',
      # 'i s q | rLSM_t0@0 rLSM_t1@1 rLSM_t2@2 rLSM_t3@3;\n',
      # 'rLSM_t5@5 rLSM_t6@6 rLSM_t7@7 rLSM_t8@8 rLSM_t9@9;\n',
      # 'i s ON pt_race;\n',
      class_str),
    OUTPUT = "SAMPSTAT STANDARDIZED MOD (5.00) TECH7 TECH11 TECH13 TECH14;",
    SAVEDATA = glue::glue("FILE IS GMM_{k}_out.dat;\n","save=cprob;"),
    # PLOT = "type = plot3;",
    usevariables = colnames(bb_gm_df4),#'rLSM_t0 rLSM_t1 rLSM_t2 rLSM_t3 rLSM_t4 rLSM_t5 rLSM_t6 rLSM_t7 rLSM_t8 rLSM_t9 tot_wc',
    rdata = bb_gm_df4
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("cross_study_analysis/mplus/gmm/wickrama/gmm_quadratic_4/GMM_{k}.dat"),
                                              modelout = glue::glue("cross_study_analysis/mplus/gmm/wickrama/gmm_quadratic_4/GMM_{k}.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
})
beepr::beep()


# L1 cubic search
class_str <- character()
lapply(1:10, function(k)
{
  
  for (x in 1:k) {
    if (x == 1) {
      
      class_str <<- glue::glue("%C#{x}%\n",
                               # "[i-s] (M1-M2);\n",
                               # "i-s (V1-V2);\n",
                               "[i s q];\n",
                               "i s q;\n",
                               "i WITH s (COV1);\n")
    } else {
      first <- (x*2)-1
      second <- (x*2)
      print(first)
      print(second)
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%\n",
                                     # "[i-s] (M{first}-M{second});\n",
                                     # "i-s (V{first}-V{second});\n",
                                     "[i s q];\n",
                                     "i s q;\n",
                                     "i WITH s (COV{x});\n"
                          ), sep = '\n')
    }
    print(class_str)
  }
  
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("GMM_{k};"),
    DATA = 'LISTWISE = ON;',
    VARIABLE = glue::glue("IDVARIABLE IS row_num;\nCLASSES = c({k});\n"),
    DEFINE = "STANDARDIZE rLSM_t0 rLSM_t1 rLSM_t2 rLSM_t3 rLSM_t4\nrLSM_t5 rLSM_t6 rLSM_t7 rLSM_t8 rLSM_t9;",
    # DEFINE = "STANDARDIZE rLSM_t0 rLSM_t1 rLSM_t2 rLSM_t3;",
    ANALYSIS = "TYPE = MIXTURE;
    STARTS=1000 200;
    STITERATIONS=50;
    LRTBOOTSTRAP = 100;
    PROCESSORS=6;",
    MODEL = glue::glue(
      '%OVERALL%\n',
      'i s q | rLSM_t0@0 rLSM_t1@1 rLSM_t2@2 rLSM_t3@3 rLSM_t4@4 \n',
      'rLSM_t5@5 rLSM_t6@6 rLSM_t7@7 rLSM_t8@8 rLSM_t9@9;\n',
      class_str),
    OUTPUT = "SAMPSTAT STANDARDIZED MOD (5.00) TECH7 TECH11 TECH13 TECH14;",
    SAVEDATA = glue::glue("FILE IS GMM_{k}_out.dat;\n","save=cprob;"),
    # PLOT = "type = plot3;",
    usevariables = colnames(bb_gm_df4),#'rLSM_t0 rLSM_t1 rLSM_t2 rLSM_t3 rLSM_t4 rLSM_t5 rLSM_t6 rLSM_t7 rLSM_t8 rLSM_t9 tot_wc',
    rdata = bb_gm_df4
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("cross_study_analysis/mplus/gmm/wickrama/gmm_quadratic/GMM_{k}.dat"),
                                              modelout = glue::glue("cross_study_analysis/mplus/gmm/wickrama/gmm_quadratic/GMM_{k}.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
})
beepr::beep()

# Adding L2 search to GMM


k <- 2 # L1 profiles
class_str <- character()
ml_lpa2 <- lapply(1:4, function(j) # L2 profiles
{
  for (x in 1:k) {
    if (x == 1) {
          class_str <<- glue::glue("%C#{x}%\n",
                                   "[i s];\n",
                                   "i s;\n",
                                   "i WITH s;\n"
                                   )
          
    } else {
      first <- (x*2)-1
      second <- (x*2)
      print(first)
      print(second)
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%\n",
                                     "[i s];\n",
                                     "i s;\n",
                                     "i WITH s;\n"
                          ), sep = "\n")
    }
  }
  print(class_str)

  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("L2_{j}_L1_{k};"),
    VARIABLE = glue::glue("IDVARIABLE IS row_num;\n",
                          "CLASSES = BC({j}) c({k});\n",
                          "CLUSTER IS pcp_id;\n",
                          "WITHIN ARE rLSM_t0 rLSM_t1 rLSM_t2 rLSM_t3;\n",
                          "BETWEEN ARE BC;\n"),
    DEFINE = "STANDARDIZE rLSM_t0 rLSM_t1 rLSM_t2 rLSM_t3;",
    ANALYSIS = "TYPE = MIXTURE TWOLEVEL;
    !DISTRIBUTION = SKEWT;
    ESTIMATOR=MLR;
    STARTS=1000 200;
    STITERATIONS=50;
    LRTSTARTS=1000 50 1000 50;
    PROCESSORS=4;",
    MODEL = paste("%WITHIN%",
                       "%OVERALL%",
                       "i s | rLSM_t0@0 rLSM_t1@1 rLSM_t2@2 rLSM_t3@3;",
                       # "i s ON pt_race;",
                       "%BETWEEN%",
                       "%OVERALL%",
                       "c ON BC;",
                       "MODEL c:",
                       "%WITHIN%", 
                  class_str, sep = "\n"),
    OUTPUT = "SAMPSTAT STANDARDIZED TECH7;",
    PLOT = "type = plot3;",
    SAVEDATA = glue::glue("FILE IS L2_{j}_L1_{k}_out.dat;\n",
               "FORMAT IS FREE;\n",
               "save = CPROBABILITIES;\n",
               "TECH4 IS tech4.dat;\n"),
    usevariables = colnames(bb_gm_df4),
    rdata = bb_gm_df4
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("Black_Box/mplus/gmm/wickrama/ml_gmm_4/L2_{j}_L1_{k}.dat"),
                                              modelout = glue::glue("Black_Box/mplus/gmm/wickrama/ml_gmm_4/L2_{j}_L1_{k}.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
})



output_enum <- readModels(here("Black_Box/mplus/gmm/wickrama/ml_gmm_4"), quiet = TRUE)
enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols=c("Title",
                                             "LL",
                                             "BIC",
                                             "aBIC"),
                                  sortBy = "Title")
enum_summary %>%
  gt::gt()

enum_summary %>%
  as.data.frame() %>%
  # filter(str_detect(Title, pattern = 'GMM_')) %>%
  pivot_longer(
    !Title, names_to = 'criteria', values_to = 'value'
  ) %>%
  mutate(
    num_profiles = readr::parse_number(str_remove(Title,'L2_'))
  ) %>%
  filter(criteria != 'LL') %>%
  ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
  geom_point() + geom_line() +
  scale_x_continuous(breaks= scales::pretty_breaks()) + labs(title='Means and variances')


L1 <- 2
L2 <- 2
out_f_dir <- 'mplus/gmm/wickrama/ml_gmm_4'
mlLPA_results <- MplusAutomation::readModels(glue::glue("Black_Box/{out_f_dir}/L2_{L2}_L1_{L1}.out"), what="savedata")$savedata


mlLPA_results %>%
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  select(rlsm_t0:rlsm_t3, l1) %>%
  pivot_longer(
    cols = rlsm_t0:rlsm_t3,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(l1 = as.factor(l1),
         variable = ordered(variable, levels = c("rlsm_t0", "rlsm_t1","rlsm_t2","rlsm_t3"))) %>%
  group_by(l1, variable) %>%
  summarize(
    m = mean(value)) %>%
  ungroup() %>%
  ggplot(aes(x = variable, y = m, color = l1, group = l1)) + 
  geom_point() + geom_line() + ggthemes::theme_tufte() +
  scale_fill_brewer(palette = "Set2")

table(mlLPA_results$BC,mlLPA_results$C)

mlLPA_results %>%
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  group_by(l2) %>%
  summarize(
    n = n(),
    c1_prop = sum(l1 == 1)/n(),
    c2_prop = sum(l1 == 2)/n(),
    c3_prop = sum(l1 == 3)/n(),
    c4_prop = sum(l1 == 4)/n()
    )

mlLPA_results %>%
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  full_join(bb_gm_df3, by = 'row_num') %>%
  write.csv('crossStudyGMMAccomodation_with_File.csv')


L1 <- 4
# L2 <- 2
out_f_dir <- 'mplus/gmm/wickrama/gmm_quadratic_4'
mlLPA_results <- MplusAutomation::readModels(glue::glue("cross_study_analysis/{out_f_dir}/GMM_{L1}.out"), what="savedata")$savedata


mlLPA_results %>%
  rename('L1' = 'C') %>%
  janitor::clean_names() %>%
  # select(rlsm_t0:rlsm_t9, l1) %>%
  select(rlsm_t0:rlsm_t3, l1) %>%
  pivot_longer(
    cols = rlsm_t0:rlsm_t3,
    # cols = rlsm_t0:rlsm_t9,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(l1 = as.factor(l1),
         # variable = ordered(variable, levels = c("rlsm_t0", "rlsm_t1","rlsm_t2","rlsm_t3","rlsm_t4","rlsm_t5","rlsm_t6","rlsm_t7","rlsm_t8","rlsm_t9"))) %>%
         variable = ordered(variable, levels = c("rlsm_t0", "rlsm_t1","rlsm_t2","rlsm_t3"))) %>%
  group_by(l1, variable) %>%
  summarize(
    m = mean(value)) %>%
  ungroup() %>%
  ggplot(aes(x = variable, y = m, color = l1, group = l1)) + 
  geom_point() + geom_line() + ggthemes::theme_tufte() +
  scale_fill_brewer(palette = "Set2")


