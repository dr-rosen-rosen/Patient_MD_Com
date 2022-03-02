#############
#### ECHO Scripts for data cleaning

library(tidyverse)
library(here)
library(config)

Sys.setenv(R_CONFIG_ACTIVE = "salar") # 'default')#
config <- config::get()

#open file
ECHO_LSM_MLM <- read_csv(here(config$ECHO_LSM_MLM_path, config$ECHO_LSM_MLM_name))

#making a race concordance variable "raceconc" between patient and provider race
ECHO_LSM_MLM <- ECHO_LSM_MLM %>%
  rowwise() %>%
  mutate(raceconc = if_else(racecat2== provrace, 1, 0)) #%>%
 #running the code below resulted in "NaN" in all cells...tried fixing it but was unsuccessful
 #mutate(LSM_function_mean_scaled = scale(LSM_function_mean, center = TRUE, scale = TRUE))
  

#adding LIWC outputs for function words for the doctors
ECHO_LSM_MLM <- ECHO_LSM_MLM %>%
  rowwise() %>%
  mutate(function_sum_doctors = sum(auxverb_D, article_D, adverb_D, ipron_D, 
                                    prep_D, negate_D, conj_D, quant_D, ppron_D))

#getting sd of function_sum_doctors for all patient visits for individual doctors
ECHO_LSM_MLM <- ECHO_LSM_MLM %>%
  group_by(provider_id) %>%
  mutate(provider_style_sd = sd(function_sum_doctors))

#Making a subscale score for the 4 cultural dissimilarity items (cdspeak, cdreason, cdstyle, cdrace)
ECHO_LSM_MLM <- ECHO_LSM_MLM %>%
  rowwise() %>%
  mutate(cd_subscale = mean(c(cdspeak, cdreason, cdstyle, cdrace)))



#Instead of attempt above, did scaling for LSM_function_mean using this method below...the values seem to be right
#when I calculate values manually ((x- mean(x))/ sd(x))
ECHO_LSM_MLM$LSM_function_mean_scaled <- scale(ECHO_LSM_MLM$LSM_function_mean, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$LSM_WC_D_scaled <- scale(ECHO_LSM_MLM$WC_D, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$WC_P_scaled <- scale(ECHO_LSM_MLM$WC_P, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$WC_D_scaled <- scale(ECHO_LSM_MLM$WC_D, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$ WPS_P_scaled <- scale(ECHO_LSM_MLM$ WPS_P, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$WPS_D_scaled <- scale(ECHO_LSM_MLM$WPS_D, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$Sixltr_D_scaled <- scale(ECHO_LSM_MLM$Sixltr_D, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$Sixltr_P_scaled <- scale(ECHO_LSM_MLM$Sixltr_P, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$affiliation_D_scaled <- scale(ECHO_LSM_MLM$affiliation_D, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$affiliation_P_scaled <- scale(ECHO_LSM_MLM$affiliation_P, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$i_D_scaled <- scale(ECHO_LSM_MLM$i_D, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$i_P_scaled <- scale(ECHO_LSM_MLM$i_P, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$Clout_D_scaled <- scale(ECHO_LSM_MLM$Clout_D, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$Clout_P_scaled <- scale(ECHO_LSM_MLM$Clout_P, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$differ_D_scaled <- scale(ECHO_LSM_MLM$differ_D, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$differ_P_scaled <- scale(ECHO_LSM_MLM$differ_P, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$Clout_D_scaled <- scale(ECHO_LSM_MLM$Clout_D, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$Clout_P_scaled <- scale(ECHO_LSM_MLM$Clout_P, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$differ_D_scaled <- scale(ECHO_LSM_MLM$differ_D, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$differ_P_scaled <- scale(ECHO_LSM_MLM$differ_P, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$insight_D_scaled <- scale(ECHO_LSM_MLM$insight_D, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$cause_D_scaled <- scale(ECHO_LSM_MLM$cause_D, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$cause_P_scaled <- scale(ECHO_LSM_MLM$cause_P, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$insight_P_scaled <- scale(ECHO_LSM_MLM$insight_P, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$negemo_P_scaled <- scale(ECHO_LSM_MLM$negemo_P, center = TRUE, scale = TRUE)
ECHO_LSM_MLM$negemo_D_scaled <- scale(ECHO_LSM_MLM$negemo_D, center = TRUE, scale = TRUE)





############################################################################################################
#LSM(outcome) will be [lower with racial/ethnic minority patients]
H1.1.1_df <- ECHO_LSM_MLM %>%
  dplyr::select(provider_id, LSM_function_mean, racecat2) %>%
  dplyr::filter(racecat2 !=4) %>%
  #added levels to the factor because the order without it was 2, 1, 3
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  #mutate(across(!provider_id & !racecat2, ~scale(.,center = TRUE, scale = TRUE))) %>%
  tidyr::drop_na()



############################################################################################################
#Hypothesis: LSM(outcome) is lower with racial/ethnic minority patients]
#Added 
H1.1.2_df <- ECHO_LSM_MLM %>%
  dplyr::select(provider_id, LSM_function_mean, racecat2, 
                cdspeak, cdreason, cdstyle, cdrace,
                age, gender, working, marital, hsdegree) %>%
  dplyr::filter(racecat2 !=4) %>%
  #added levels to the factor because the order without it was 2, 1, 3
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  #mutate(across(!provider_id & !racecat2, ~scale(.,center = TRUE, scale = TRUE))) %>%
  tidyr::drop_na()

############################################################################################################
#Hypothesis: LSM (outcome) is higher within dyads characterized by race concordance
H1.2_df <- ECHO_LSM_MLM %>%
  dplyr::select(provider_id, LSM_function_mean, raceconc) %>%
  mutate(raceconc = factor(raceconc)) %>%
  tidyr::drop_na()

############################################################################################################
#Hypothesis: LSM (outcome) is higher for those who rate each other as culturally similar
#need to add the dissimilarity distance variable
H1.3_df <- ECHO_LSM_MLM %>%
  dplyr::select(provider_id, LSM_function_mean, racecat2, 
                cultdiss, cultdissmd,
                cdspeak,cdreason,cdstyle,cdvalue,cdspirit,cdethnic,cdtype,cdrace,cdculture,cdskin,
                cultdissmd1, cultdissmd2, cultdissmd3, cultdissmd4, cultdissmd5, cultdissmd6, cultdissmd7, cultdissmd8, cultdissmd9, cultdissmd10) %>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  tidyr::drop_na() %>% # take only rows with no NA;
  mutate(
    cdAvg_dist_abs = abs(cultdiss - cultdissmd),
    cdspeak_dist_abs = abs(cdspeak - cultdissmd1),
    cdreason_dist_abs = abs(cdreason - cultdissmd2),
    cdstyle_dist_abs = abs(cdstyle - cultdissmd3),
    cdvalue_dist_abs = abs(cdvalue - cultdissmd4),
    cdspirit_dist_abs = abs(cdspirit - cultdissmd5),
    cdethnic_dist_abs = abs(cdethnic - cultdissmd6),
    cdtype_dist_abs = abs(cdtype - cultdissmd7),
    cdrace_dist_abs = abs(cdrace - cultdissmd8),
    cdculture_dist_abs = abs(cdculture - cultdissmd9),
    cdskin_dist_abs = abs(cdskin - cultdissmd10))
    
H1.3_df <- H1.3_df %>%
  mutate(cdAvg_dist = (cultdiss - cultdissmd),
    cdspeak_dist = (cdspeak - cultdissmd1),
    cdreason_dist = (cdreason - cultdissmd2),
    cdstyle_dist = (cdstyle - cultdissmd3),
    cdvalue_dist = (cdvalue - cultdissmd4),
    cdspirit_dist = (cdspirit - cultdissmd5),
    cdethnic_dist = (cdethnic - cultdissmd6),
    cdtype_dist = (cdtype - cultdissmd7),
    cdrace_dist = (cdrace - cultdissmd8),
    cdculture_dist = (cdculture - cultdissmd9),
    cdskin_dist = (cdskin - cultdissmd10)
  )


############################################################################################################
#Using LSM_function_mean to predict communication quality with provider (provcomm)
H3a.1_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, provider_style_sd, 
                provcomm, provcommhigh, pcwords, pcfast, pctime,	
                pclisten, pcignore,	pcinfo,	pchealthprob,	
                pcanytest, pcwhytest,	pchowtest, pcexamine,	
                pcconfuse, pccarehome, pcsymp, pchowmeds, 	
                pcgoovermeds,	pcwritemeds, pcreasonmeds,	
                pcsemeds,	pcdiff,	pcactivities,	pcinvolvedec,	
                pcfelttreat, pcprefopin, pcpressure, pcaskprob, pcunderprob,
                WC_D_scaled, WC_P_scaled, WPS_D_scaled, WPS_P_scaled, Sixltr_D_scaled, Sixltr_P_scaled, 
                affiliation_D_scaled,affiliation_P_scaled, i_D_scaled, i_P_scaled, Clout_D_scaled, 
                Clout_P_scaled, differ_D_scaled, differ_P_scaled, Clout_D_scaled, Clout_P_scaled, 
                insight_D_scaled, insight_P_scaled, cause_D_scaled, cause_P_scaled, negemo_P_scaled, 
                negemo_D_scaled, racecat2) %>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  tidyr::drop_na()
  
############################################################################################################
#Using LSM_function_mean to predict overall communication quality (overcomm)
H3a.2_df <- ECHO_LSM_MLM %>%
  dplyr::select( provider_id, LSM_function_mean, provider_style_sd, overcomm, overcommhigh, ocexplain,	
                 ocgive, octell, occare, ocunderstand, WC_D_scaled, WC_P_scaled, WPS_D_scaled, WPS_P_scaled, Sixltr_D_scaled, Sixltr_P_scaled, 
                 affiliation_D_scaled,affiliation_P_scaled, i_D_scaled, i_P_scaled, Clout_D_scaled, 
                 Clout_P_scaled, differ_D_scaled, differ_P_scaled, Clout_D_scaled, Clout_P_scaled, 
                 insight_D_scaled, insight_P_scaled, cause_D_scaled, cause_P_scaled, negemo_P_scaled, 
                 negemo_D_scaled, racecat2) %>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  tidyr::drop_na()

############################################################################################################
#Using LSM_function_mean to predict provider interpersonal style (ipstyle)
H3a.3_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, provider_style_sd, ipstyle, ipstylehigh, 
                ipfriend,	ipwelcome, iprude, ipcare,	
                ipname,	iptalkfront, ippriv, ipinferior,	
                ipnegattitude, ipdiscrimrace, ipdiscrimeduc,	
                iplessworry, ipcompliment, ipcompassion, 
                WC_D_scaled, WC_P_scaled, WPS_D_scaled, WPS_P_scaled, Sixltr_D_scaled, Sixltr_P_scaled, 
                affiliation_D_scaled,affiliation_P_scaled, i_D_scaled, i_P_scaled, Clout_D_scaled, 
                Clout_P_scaled, differ_D_scaled, differ_P_scaled, Clout_D_scaled, Clout_P_scaled, 
                insight_D_scaled, insight_P_scaled, cause_D_scaled, cause_P_scaled, negemo_P_scaled, 
                negemo_D_scaled, racecat2) %>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  tidyr::drop_na()

############################################################################################################
#Using LSM_function_mean to predict interpersonal trust (iptrust)
H3a.4_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, provider_style_sd, iptrust, iptrusttert, 
                iptdoubtcare,	iptconsiderate,	iptadvice,	
                ipttrue, iptdistrust, iptjudge,	iptnotdo,	
                iptaboveall, iptwellqual, iptmistake, iptinfopriv,
                WC_D_scaled, WC_P_scaled, WPS_D_scaled, WPS_P_scaled, Sixltr_D_scaled, Sixltr_P_scaled, 
                affiliation_D_scaled,affiliation_P_scaled, i_D_scaled, i_P_scaled, Clout_D_scaled, 
                Clout_P_scaled, differ_D_scaled, differ_P_scaled, Clout_D_scaled, Clout_P_scaled, 
                insight_D_scaled, insight_P_scaled, cause_D_scaled, cause_P_scaled, negemo_P_scaled, 
                negemo_D_scaled, racecat2) %>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  tidyr::drop_na()

############################################################################################################
#Using LSM_function_mean to predict if patient reports that provider knows them as a person (provknowcat)
H3a.5_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, provider_style_sd, provknowcat, 
                WC_D_scaled, WC_P_scaled, WPS_D_scaled, WPS_P_scaled, Sixltr_D_scaled, Sixltr_P_scaled, 
                affiliation_D_scaled,affiliation_P_scaled, i_D_scaled, i_P_scaled, Clout_D_scaled, 
                Clout_P_scaled, differ_D_scaled, differ_P_scaled, Clout_D_scaled, Clout_P_scaled, 
                insight_D_scaled, insight_P_scaled, cause_D_scaled, cause_P_scaled, negemo_P_scaled, 
                negemo_D_scaled, racecat2) %>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  tidyr::drop_na()


############################################################################################################
#Using LSM_function_mean to predict overall patient satisfaction (overallsat)
H3a.6_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, provider_style_sd, overallsat,
                WC_D, WC_P, WPS_D, WPS_P, Sixltr_D, Sixltr_P, 
                affiliation_D,affiliation_P, i_D, i_P, Clout_D, 
                Clout_P, differ_D, differ_P, Clout_D, Clout_P, 
                insight_D, insight_P, cause_D, cause_P, negemo_P, 
                negemo_D, racecat2) %>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  tidyr::drop_na()


############################################################################################################
#Using LSM_function_mean to predict viral suppression
H3b.1_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, vlsup75,
                WC_D_scaled, WC_P_scaled, WPS_D_scaled, WPS_P_scaled, Sixltr_D_scaled, Sixltr_P_scaled, 
                affiliation_D_scaled,affiliation_P_scaled, i_D_scaled, i_P_scaled, Clout_D_scaled, 
                Clout_P_scaled, differ_D_scaled, differ_P_scaled, Clout_D_scaled, Clout_P_scaled, 
                insight_D_scaled, insight_P_scaled, cause_D_scaled, cause_P_scaled, negemo_P_scaled, 
                negemo_D_scaled, racecat2) %>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  tidyr::drop_na()


############################################################################################################
#Using LSM_function_mean to predict adherence measures
#adhard, adeasy, adunable, adfollow,adpast4, adpast30, pctarv, adrate, missany(binomial)
H4_df <- ECHO_LSM_MLM %>%
  dplyr::select(LSM_function_mean, provider_id, provider_style_sd, racecat2, adhard, adeasy, adunable, adfollow,
                adpast4, adpast30, pctarv, adrate, missany, WC_D_scaled, WC_P_scaled, WPS_D_scaled, WPS_P_scaled, Sixltr_D_scaled, Sixltr_P_scaled, 
                affiliation_D_scaled,affiliation_P_scaled, i_D_scaled, i_P_scaled, Clout_D_scaled, 
                Clout_P_scaled, differ_D_scaled, differ_P_scaled, Clout_D_scaled, Clout_P_scaled, 
                insight_D_scaled, insight_P_scaled, cause_D_scaled, cause_P_scaled, negemo_P_scaled, 
                negemo_D_scaled) %>%
  mutate(racecat2 = factor(racecat2, levels = c("1","2","3"))) %>%
  tidyr::drop_na()
