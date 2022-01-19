save.image('ECHO1.Rdata')

pth <- '/Users/mrosen44/OneDrive - Johns Hopkins University/Data_Analysis/ECHO_1/'
file <-'ECHO1_df_complete.csv'
inf <- paste(c(pth,file),sep='',collapse = '')
df <- read.csv(inf,header=TRUE,sep=',')

# These are complicated transcripts with more than one D
to_exclude <- c(
  'JC08P03',
  'JC08P06',
  'JC08P07',
  'JC08P10',
  'JC09P06',
  'JC10P01',
  'JC11P01',
  'JC11P02',
  'JC11P03',
  'JC11P04',
  'JC11P05',
  'JC11P06',
  'JC11P07',
  'JC11P08',
  'JC11P09',
  'OC03P01',
  'SC24P114'
)

# Remove excluded files
df_sub <- df[!df$File %in% to_exclude, ]

new_ivs <- c(
  'racecat2',
  #'ccomp',
  #'ccomptert',
  'cultcomp',
  'cultcomptert',
  'iptrust',
  'provknowcat',
  'provcomm',
  'provcommtert',
  'ipstyle',
  #'ipstyletert',
  'ipstylehigh',
  'provassess'
)

new_ivs_cont <- c(
  'cultcomp',
  'iptrust',
  'provcomm',
  'ipstyle',
  'provassess',
  'cultdiss',
  'cultdissmd'
)

docCom <- c(
  'WC_D',
  'Analytic_D',
  'Clout_D',
  'Authentic_D',
  'Tone_D',
  'Sixltr_D',
  'function_D',
  'pronoun_D',
  'article_D',
  'prep_D',
  'auxverb_D',
  'adverb_D',
  'conj_D',
  'negate_D',
  'quant_D'
)

pt_ratings <- c(
  'cdspeak',
  'cdreason',
  'cdstyle',
  'cdvalue',
  'cdspirit',
  'cdethnic',
  'cdtype',
  'cdrace',
  'cdculture',
  'cdskin'
)

clin_ratings <- c(
  'cultdissmd1',
  'cultdissmd2',
  'cultdissmd3',
  'cultdissmd4',
  'cultdissmd5',
  'cultdissmd6',
  'cultdissmd7',
  'cultdissmd8',
  'cultdissmd9',
  'cultdissmd10'
)

conv_LSM <- c(
  'Conv_LSM_ppron',
  'Conv_LSM_ipron',
  'Conv_LSM_article',
  'Conv_LSM_prep',
  'Conv_LSM_auxverb',
  'Conv_LSM_adverb',
  'Conv_LSM_negate',
  'Conv_LSM_conj',
  'Conv_LSM_quant',
  'Conv_LSM_Avg'
)
length(conv_LSM)

TbyT_metrics <- c(
  'TbyT_Vader_compound_autocorr',
  #'TbyT_Vader_compound_var',
  'TbyT_Vader_neg_autocorr',
  #'TbyT_Vader_neg_var',
  'TbyT_Vader_neu_autocorr',
  #'TbyT_Vader_neu_var',
  'TbyT_Vader_pos_autocorr',
  #'TbyT_Vader_pos_var',
  'TbyT_WC_autocorr',
  #'TbyT_WC_var',
  'TbyT_Analytic_autocorr',
  #'TbyT_Analytic_var',
  'TbyT_Clout_autocorr',
  #'TbyT_Clout_var',
  'TbyT_Authentic_autocorr',
  #'TbyT_Authentic_var',
  'TbyT_Tone_autocorr',
  #'TbyT_Tone_var',
  'TbyT_Sixltr_autocorr',
  #'TbyT_Sixltr_var',
  'TbyT_function_autocorr',
  #'TbyT_function_var',
  'TbyT_ppron_autocorr',
  #'TbyT_ppron_var',
  'TbyT_ipron_autocorr',
  #'TbyT_ipron_var',
  'TbyT_article_autocorr',
  #'TbyT_article_var',
  'TbyT_prep_autocorr',
  #'TbyT_prep_var',
  'TbyT_auxverb_autocorr',
  #'TbyT_auxverb_var',
  'TbyT_adverb_autocorr',
  #'TbyT_adverb_var',
  'TbyT_negate_autocorr',
  #'TbyT_negate_var',
  'TbyT_conj_autocorr',
  #'TbyT_conj_var',
  'TbyT_quant_autocorr'#,
  #'TbyT_quant_var'
)

length(TbyT_metrics)

# centers variables
continuous_vars <- c(pt_ratings,clin_ratings,conv_LSM,TbyT_metrics,docCom,new_ivs_cont)
for (vr in continuous_vars){
  df_sub[,vr] <- as.numeric(df_sub[,vr])
  df_sub[[vr]] <- scale(df_sub[[vr]], center= TRUE, scale = TRUE)
}

for (vr in pt_ratings){
  hist(df_sub[,vr],main = vr)
}

for (vr in clin_ratings){
  hist(df_sub[,vr],main = vr)
}

for (vr in new_ivs){
  hist(df_sub[,vr],main = vr)
}
