####################################################################################
####################################################################################
###################################
###################################                   provassess
###################################
####################################################################################
####################################################################################


fit_provassess <- lm(ipstyle ~ 
                     Comp.1 +
                   Comp.2 +
                     Comp.3 +
                     Comp.4 +
                     Comp.5 +
                     Comp.6 +
                     Comp.7 +
                     Comp.8 +
                     Comp.9 +
                     Comp.10 +
                     Comp.11,
                   data = x, na.action = na.omit
)

summary(fit_provassess)


####################################################################################
####################################################################################
###################################
###################################                   cdreason
###################################
####################################################################################
####################################################################################


fit_cdreason <- lm(cdreason ~ 
                     TbyT_Clout_autocorr
                      ,
                      data = df_NoNA, na.action = na.omit
)

summary(fit_cdreason)


####################################################################################
####################################################################################
###################################
###################################                   cdstyle
###################################
####################################################################################
####################################################################################


fit_cdstyle <- lm(cdstyle ~ 
                    TbyT_Tone_var
                   ,
                   data = df_NoNA, na.action = na.omit
)

summary(fit_cdstyle)


####################################################################################
####################################################################################
###################################
###################################                   cdethnic
###################################
####################################################################################
####################################################################################


fit_cdethnic <- lm(cdethnic ~ 
                     #Conv_LSM_ppron +
                   #Conv_LSM_auxverb +
                   Conv_LSM_adverb +
                   Conv_LSM_negate +
                   TbyT_Vader_compound_var +
                   #TbyT_Vader_neg_autocorr +
                   #TbyT_WC_var +
                   #TbyT_Authentic_autocorr +
                   #TbyT_Tone_autocorr +
                   TbyT_Tone_var +
                   #TbyT_Sixltr_autocorr +
                   TbyT_Sixltr_var +
                   TbyT_function_var +
                   TbyT_ipron_autocorr +
                   #TbyT_ipron_var +
                   #TbyT_article_autocorr +
                   #TbyT_prep_autocorr +
                   #TbyT_auxverb_autocorr +
                   TbyT_adverb_autocorr +
                   TbyT_negate_var# +
                   #TbyT_conj_autocor
                  ,
                  data = df_NoNA, na.action = na.omit
)

summary(fit_cdethnic)

colnames(df_NoNA)


####################################################################################
####################################################################################
###################################
###################################                   cdtype
###################################
####################################################################################
####################################################################################


fit_cdtype <- lm(cdtype ~ 
                   #Conv_LSM_article +
                 #Conv_LSM_prep +
                 TbyT_Clout_var +
                 #TbyT_Authentic_var +
                 TbyT_Tone_var +
                 #TbyT_article_autocorr +
                 TbyT_article_var #+
                 #TbyT_conj_autocorr
                   ,
                   data = df_NoNA, na.action = na.omit
)

summary(fit_cdtype)


####################################################################################
####################################################################################
###################################
###################################                   cdrace
###################################
####################################################################################
####################################################################################


fit_cdrace <- lm(cdrace ~ 
                   Conv_LSM_ppron +
                 #Conv_LSM_prep +
                 Conv_LSM_auxverb +
                 Conv_LSM_adverb +
                 Conv_LSM_negate +
                 #TbyT_Vader_pos_autocorr +
                 #TbyT_WC_autocorr +
                 TbyT_WC_var +
                 #TbyT_Analytic_autocorr +
                 #TbyT_Tone_var +
                 TbyT_Sixltr_autocorr +
                 #TbyT_function_var +
                 TbyT_ipron_autocorr +
                 #TbyT_article_autocorr +
                 TbyT_article_var +
                 #TbyT_adverb_autocorr +
                 TbyT_negate_var +
                 TbyT_conj_autocorr +
                 TbyT_quant_autocorr +
                 TbyT_quant_var
                 ,
                 data = df_NoNA, na.action = na.omit
)

summary(fit_cdrace)


####################################################################################
####################################################################################
###################################
###################################                   cdculture
###################################
####################################################################################
####################################################################################


fit_cdculture <- lm(cdculture ~ 
                      #Conv_LSM_ppron +
                    #Conv_LSM_auxverb +
                    Conv_LSM_adverb +
                    Conv_LSM_adverb + 
                    Conv_LSM_negate +
                    TbyT_Vader_pos_autocorr +
                    #TbyT_WC_autocorr +
                    #TbyT_WC_var +
                    TbyT_Tone_var +
                    #TbyT_Sixltr_autocorr +
                    TbyT_function_var +
                    #TbyT_ppron_var +
                    #TbyT_ipron_autocorr +
                    TbyT_ipron_var +
                    #TbyT_article_autocorr +
                    TbyT_article_var +
                    #TbyT_prep_autocorr +
                    TbyT_auxverb_var +
                    #TbyT_adverb_autocorr +
                    #TbyT_adverb_var +
                    TbyT_conj_autocorr
                 ,
                 data = df_NoNA, na.action = na.omit
)

summary(fit_cdculture)

####################################################################################
####################################################################################
###################################
###################################                   cdskin
###################################
####################################################################################
####################################################################################


fit_cdskin <- lm(cdskin ~ 
                   #Conv_LSM_auxverb +
                 Conv_LSM_adverb +
                 Conv_LSM_negate +
                 TbyT_WC_var +
                 #TbyT_Authentic_var +
                 TbyT_Tone_autocorr +
                 #TbyT_Sixltr_autocorr +
                 #TbyT_ipron_autocorr +
                 #TbyT_ipron_var +
                 #TbyT_article_autocorr +
                 #TbyT_ipron_var +
                 #TbyT_article_autocorr +
                 TbyT_article_var +
                 #TbyT_adverb_autocorr +
                 TbyT_negate_autocorr +
                 TbyT_conj_autocorr +
                 TbyT_quant_autocorr +
                 TbyT_quant_var
                    ,
                    data = df_NoNA, na.action = na.omit
)

summary(fit_cdskin)

####################################################################################
####################################################################################
###################################
###################################                   cultdissmd1
###################################
####################################################################################
####################################################################################


fit_cultdissmd1 <- lm(cultdissmd1 ~ 
                        Conv_LSM_article +
                        #Conv_LSM_auxverb +
                        Conv_LSM_adverb +
                        Conv_LSM_conj +
                        Conv_LSM_quant +
                        TbyT_Vader_neg_autocorr +
                        #TbyT_Vader_pos_autocorr +
                        TbyT_Clout_autocorr +
                        TbyT_Tone_autocorr +
                        TbyT_Tone_var +
                        #TbyT_Sixltr_autocorr +
                        #TbyT_Sixltr_var +
                        #TbyT_function_var +
                        #TbyT_ppron_var +
                        TbyT_article_autocorr +
                        #TbyT_adverb_autocorr +
                        #TbyT_adverb_var +
                        TbyT_negate_autocorr# +
                        #TbyT_conj_autocorr
                      ,
                      data = df_NoNA, na.action = na.omit
                      )

summary(fit_cultdissmd1)

####################################################################################
####################################################################################
###################################
###################################                   cultdissmd2
###################################
####################################################################################
####################################################################################


fit_cultdissmd2 <- lm(cultdissmd2 ~ 
                        Conv_LSM_article +
                        TbyT_Vader_neu_var +
                        #TbyT_Vader_pos_var +
                        TbyT_Clout_autocorr +
                        TbyT_Tone_var +
                        #TbyT_Sixltr_autocorr +
                        #TbyT_function_autocorr +
                        TbyT_article_autocorr +
                        TbyT_negate_autocorr
                      ,
                      data = df_NoNA, na.action = na.omit
)

summary(fit_cultdissmd2)


####################################################################################
####################################################################################
###################################
###################################                   cultdissmd3
###################################
####################################################################################
####################################################################################


fit_cultdissmd3 <- lm(cultdissmd3 ~ 
                        Conv_LSM_article +
                        #Conv_LSM_auxverb +
                        Conv_LSM_adverb +
                       #Conv_LSM_negate +
                        Conv_LSM_conj +
                        Conv_LSM_quant +
                        TbyT_Vader_neu_var +
                        #TbyT_Vader_pos_autocorr +
                        TbyT_Clout_autocorr +
                        #TbyT_Clout_var +
                        #TbyT_Tone_autocorr +
                        #TbyT_Tone_var +
                        #TbyT_Sixltr_autocorr +
                        #TbyT_function_autocorr +
                        TbyT_article_autocorr +
                        #TbyT_auxverb_autocorr +
                        #TbyT_adverb_autocorr +
                        TbyT_adverb_var #+
                        #TbyT_conj_autocorr +
                        #TbyT_conj_var
                        
                      ,
                      data = df_NoNA, na.action = na.omit
)

summary(fit_cultdissmd3)

####################################################################################
####################################################################################
###################################
###################################                   cultdissmd4
###################################
####################################################################################
####################################################################################


fit_cultdissmd4 <- lm(cultdissmd4 ~ 
                        
                        TbyT_Vader_neu_var +
                        #TbyT_Vader_pos_var +
                        TbyT_Clout_autocorr +
                        TbyT_Clout_var +
                        TbyT_Tone_autocorr +
                        #TbyT_Tone_var +
                        #TbyT_function_autocorr +
                        #TbyT_article_autocorr +
                        TbyT_negate_autocorr 
                      
                      ,
                      data = df_NoNA, na.action = na.omit
)

summary(fit_cultdissmd4)


####################################################################################
####################################################################################
###################################
###################################                   cultdissmd5
###################################
####################################################################################
####################################################################################

fit_cultdissmd5 <- lm(cultdissmd5 ~ 
                        
                        #Conv_LSM_ppron +
                        #Conv_LSM_adverb +
                        Conv_LSM_negate +
                        #TbyT_Vader_compound_autocorr +
                        TbyT_Vader_neu_var +
                        #TbyT_Vader_pos_autocorr +
                        #TbyT_Vader_pos_var +
                        TbyT_Clout_autocorr +
                        TbyT_Tone_autocorr +
                        #TbyT_Tone_var +
                        #TbyT_Sixltr_autocorr +
                        #TbyT_Sixltr_var +
                        #TbyT_function_autocorr +
                        #TbyT_ipron_autocorr +
                        #TbyT_article_autocorr +
                        TbyT_negate_autocorr #+
                        #TbyT_quant_var
                      
                      ,
                      data = df_NoNA, na.action = na.omit
)

summary(fit_cultdissmd5)

####################################################################################
####################################################################################
###################################
###################################                   cultdissmd6
###################################
####################################################################################
####################################################################################

fit_cultdissmd6 <- lm(cultdissmd6 ~ 
                        
                        #Conv_LSM_ppron +
                        Conv_LSM_auxverb +
                        Conv_LSM_adverb +
                        #Conv_LSM_negate +
                        #Conv_LSM_quant +
                        #TbyT_Vader_neg_autocorr +
                        TbyT_Vader_neg_var +
                        #TbyT_WC_var +
                        TbyT_Analytic_autocorr +
                        #TbyT_Analytic_var +
                        #TbyT_Authentic_autocorr +
                        #TbyT_Tone_autocorr +
                        TbyT_Tone_var +
                        #TbyT_Sixltr_autocorr +
                        TbyT_function_var +
                        #TbyT_ppron_autocorr +
                        #TbyT_ipron_autocorr +
                        #TbyT_article_autocorr +
                        #TbyT_prep_autocorr +
                        #TbyT_prep_var +
                        #TbyT_auxverb_var +
                        #TbyT_adverb_autocorr +
                        #TbyT_conj_autocorr +
                        TbyT_quant_var
                      
                      ,
                      data = df_NoNA, na.action = na.omit
)

summary(fit_cultdissmd6)


####################################################################################
####################################################################################
###################################
###################################                   cultdissmd7
###################################
####################################################################################
####################################################################################

fit_cultdissmd7 <- lm(cultdissmd7 ~ 
                        
                        Conv_LSM_ppron +
                        #Conv_LSM_adverb +
                        Conv_LSM_negate +
                        #Conv_LSM_quant +
                        TbyT_Clout_autocorr +
                        #TbyT_Clout_var +
                        #TbyT_Sixltr_autocorr +
                        #TbyT_Sixltr_var +
                        
                        #TbyT_function_autocorr  +
                        TbyT_ppron_autocorr +
                        #TbyT_ipron_autocorr +
                        #TbyT_prep_var +
                        #TbyT_adverb_autocorr +
                        #TbyT_adverb_var +
                        TbyT_conj_autocorr# +
                        #TbyT_quant_autocorr
                      
                      ,
                      data = df_NoNA, na.action = na.omit
)

summary(fit_cultdissmd7)

####################################################################################
####################################################################################
###################################
###################################                   cultdissmd8
###################################
####################################################################################
####################################################################################

fit_cultdissmd8 <- lm(cultdissmd8 ~ 
                        
                        #Conv_LSM_ppron +
                        Conv_LSM_auxverb +
                        Conv_LSM_adverb +
                        #Conv_LSM_quant +
                        #TbyT_Clout_autocorr +
                        TbyT_WC_var +
                        #TbyT_Analytic_autocorr +
                        #TbyT_Authentic_autocorr +
                        TbyT_Tone_autocorr +
                        #TbyT_Tone_var +
                        TbyT_Sixltr_autocorr +
                        #TbyT_Sixltr_var +
                        TbyT_function_var +
                        #TbyT_ppron_autocorr +
                        #TbyT_prep_autocorr +
                        TbyT_negate_autocorr +
                        TbyT_negate_var +
                        TbyT_conj_autocorr +
                        TbyT_quant_var
                      
                      ,
                      data = df_NoNA, na.action = na.omit
)

summary(fit_cultdissmd8)

library("jtools", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
plot_coefs(fit_cultdissmd8)

####################################################################################
####################################################################################
###################################
###################################                   cultdissmd9
###################################
####################################################################################
####################################################################################

fit_cultdissmd9 <- lm(cultdissmd9 ~ 
                        
                        #Conv_LSM_auxverb +
                        #Conv_LSM_adverb +
                        #Conv_LSM_negate +
                        #TbyT_Vader_neg_autocorr +
                        #TbyT_Vader_neg_var +
                        TbyT_Analytic_autocorr +
                        #TbyT_Analytic_var +
                        #TbyT_Clout_autocorr +
                        #TbyT_Authentic_autocorr +
                        TbyT_Tone_autocorr +
                        TbyT_Tone_var +
                        TbyT_Sixltr_autocorr +
                        TbyT_Sixltr_autocorr +
                        TbyT_function_var +
                        #TbyT_ppron_autocorr +
                        #TbyT_ipron_autocorr +
                        #TbyT_article_autocorr +
                        #TbyT_prep_autocorr +
                        #TbyT_prep_var +
                        #TbyT_conj_autocorr +
                        TbyT_quant_var
                      
                      ,
                      data = df_NoNA, na.action = na.omit
)

summary(fit_cultdissmd9)

####################################################################################
####################################################################################
###################################
###################################                   cultdissmd10
###################################
####################################################################################
####################################################################################

fit_cultdissmd10 <- lm(cultdissmd10 ~ 
                        
                        #Conv_LSM_auxverb +
                        #Conv_LSM_adverb +
                        #Conv_LSM_negate +
                        #TbyT_Vader_neg_autocorr +
                        #TbyT_Vader_neg_var +
                        #TbyT_Analytic_autocorr +
                        #TbyT_Analytic_var +
                        #TbyT_Clout_autocorr +
                        #TbyT_Authentic_autocorr +
                        #TbyT_Tone_autocorr +
                        TbyT_Tone_var +
                        #TbyT_Sixltr_autocorr +
                        #TbyT_Sixltr_autocorr +
                        TbyT_function_var +
                        #TbyT_ppron_autocorr +
                        #TbyT_ipron_autocorr +
                        #TbyT_article_autocorr +
                        #TbyT_prep_autocorr +
                        #TbyT_prep_var +
                        #TbyT_conj_autocorr +
                        TbyT_quant_var
                      
                      ,
                      data = df_NoNA, na.action = na.omit
)

summary(fit_cultdissmd10)

####################################################################################
####################################################################################
###################################
###################################                   cdstyle
###################################
####################################################################################
####################################################################################

fit_cdstyle <- lm(cdstyle ~ 
                        
                        
                        TbyT_Tone_var
                      
                      ,
                      data = df_NoNA, na.action = na.omit
)

summary(fit_cdstyle)

####################################################################################
####################################################################################
###################################
###################################                   cdethnic
###################################
####################################################################################
####################################################################################

fit_cdethnic <- lm(cdethnic ~ 
                    
                     #Conv_LSM_ppron +
                     #Conv_LSM_prep +
                     #Conv_LSM_auxverb +
                     #Conv_LSM_adverb +
                     Conv_LSM_negate +
                     #TbyT_Vader_compound_autocorr +
                     #TbyT_Vader_pos_autocorr +
                     TbyT_Vader_pos_var +
                     #TbyT_WC_autocorr +
                     #TbyT_WC_var +
                     #TbyT_Analytic_autocorr +
                     TbyT_Tone_var +
                     TbyT_Sixltr_autocorr +
                     TbyT_function_var +
                     #TbyT_ipron_autocorr +
                     #TbyT_ipron_var +
                     #TbyT_article_autocorr +
                     #TbyT_article_var +
                     #TbyT_adverb_autocorr +
                     #TbyT_negate_autocorr +
                     #TbyT_negate_var +
                     TbyT_conj_autocorr# +
                     #TbyT_conj_var +
                     #TbyT_quant_autocorr +
                     #TbyT_quant_var

                  ,
                  data = df_NoNA, na.action = na.omit
)

summary(fit_cdethnic)

####################################################################################
####################################################################################
###################################
###################################                   cdrace
###################################
####################################################################################
####################################################################################


fit_cdrace <- lm(cdrace ~ 
                     
                     #Conv_LSM_ppron +
                     #Conv_LSM_prep +
                     #Conv_LSM_auxverb +
                     Conv_LSM_adverb +
                     #Conv_LSM_negate +
                     #TbyT_Vader_pos_autocorr +
                     TbyT_Vader_pos_var +
                     #TbyT_WC_var +
                     #TbyT_Analytic_autocorr +
                     TbyT_Tone_var +
                     TbyT_Sixltr_autocorr +
                     TbyT_function_var +
                     #TbyT_ipron_autocorr +
                     #TbyT_article_autocorr +
                     #TbyT_article_var +
                     #TbyT_adverb_autocorr +
                     #TbyT_negate_autocorr +
                     #TbyT_negate_var +
                     TbyT_conj_autocorr +
                  # TbyT_quant_autocorr +
                   TbyT_quant_var
                   
                   ,
                   data = df_NoNA, na.action = na.omit
)

summary(fit_cdrace)

####################################################################################
####################################################################################
###################################
###################################                   cdculture
###################################
####################################################################################
####################################################################################


fit_cdculture <- lm(cdculture ~ 
                   
                   #Conv_LSM_ppron +
                   #Conv_LSM_auxverb +
                  Conv_LSM_negate +
                   TbyT_Vader_pos_autocorr +
                    #TbyT_WC_autocorr +
                    #TbyT_WC_var +
                    #TbyT_Clout_autocorr +
                    TbyT_Tone_var +
                    #TbyT_Sixltr_autocorr +
                   #TbyT_function_var +
                    TbyT_ppron_var +
                    #TbyT_ipron_autocorr +
                    TbyT_ipron_var +
                    #TbyT_article_autocorr +
                    TbyT_article_var +
                    #TbyT_prep_autocorr +
                    TbyT_auxverb_var +
                    #TbyT_adverb_autocorr +
                    #TbyT_adverb_var +
                    TbyT_conj_autocorr
                 
                 ,
                 data = df_NoNA, na.action = na.omit
)

summary(fit_cdculture)
plot_coefs(fit_cdculture)


####################################################################################
####################################################################################
###################################
###################################                   cdskin
###################################
####################################################################################
####################################################################################


fit_cdskin <- lm(cdskin ~ 
                      
                      #Conv_LSM_auxverb +
                   Conv_LSM_adverb +
                      Conv_LSM_negate +
                      TbyT_WC_var +
                        TbyT_Tone_autocorr +
                        #TbyT_Sixltr_autocorr +
                        #TbyT_ipron_autocorr +
                        #TbyT_ipron_var +
                        #TbyT_article_autocorr +
                        TbyT_article_var +
                        TbyT_negate_autocorr +
                        TbyT_conj_autocorr +
                        TbyT_quant_var

                    
                    ,
                    data = df_NoNA, na.action = na.omit
)

summary(fit_cdskin)