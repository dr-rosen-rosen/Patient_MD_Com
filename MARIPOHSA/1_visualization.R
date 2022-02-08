library(tidyverse)


########################################################################
########################################################################
########################################################################
#DATA VISUALIZATION OF LIWC CATEGORIES VS SUBCATEGORIES#################
########################################################################
########################################################################
########################################################################

#plot for function word variable
ggplot(LIWC_df, aes(x= funct, y = funct_sum)) + geom_point() + 
  xlim(0, 100)+ylim(0, 100) + geom_abline(intercept = 0, 
                                          slope = 1, linetype = "dashed", color= "red") + 
  theme_bw() +labs(title="Function Words VS. Sum of Function Words Subcategories")+
  # move the title text to the middle
  theme(plot.title=element_text(hjust=0.5))

#plot for pronoun variable
ggplot(LIWC_df, aes(x= pronoun, y = pronoun_sum)) + 
  geom_point() + xlim(0, 50)+ylim(0, 50) + geom_abline(intercept = 0, 
                                                       slope = 1, linetype = "dashed", color= "red") + 
  theme_bw() +labs(title="Pronoun VS. Sum of Pronoun Subcategories")+
  # move the title text to the middle
  theme(plot.title=element_text(hjust=0.5))

#plot for personal pronount variable
ggplot(LIWC_df, aes(x= ppron, y = ppronoun_sum)) + 
  geom_point() + xlim(0, 50)+ylim(0, 50) + geom_abline(intercept = 0, 
                                                       slope = 1, linetype = "dashed", color= "red") + 
  theme_bw() +labs(title="Personal Pronoun VS. Sum of personal Pronoun Subcategories")+
  # move the title text to the middle
  theme(plot.title=element_text(hjust=0.5))

#plot for social processes variable
ggplot(LIWC_df, aes(x= social, y = social_sum)) + geom_point() + 
  xlim(0, 100)+ylim(0, 100) + geom_abline(intercept = 0, 
                                          slope = 1, linetype = "dashed", color= "red") + 
  theme_bw() +labs(title="Social Processes VS. Sum of Social Processes Subcategories")+
  # move the title text to the middle
  theme(plot.title=element_text(hjust=0.5)) 

#plot for cognitive processes variable
ggplot(LIWC_df, aes(x= cogproc, y = cogproc_sum)) + geom_point() + 
  xlim(0, 40)+ylim(0, 40) + geom_abline(intercept = 0, 
                                        slope = 1, linetype = "dashed", color= "red") + 
  theme_bw() +labs(title="Cognitive Processes VS. Sum of Cognitive Processes Subcategories")+
  # move the title text to the middle
  theme(plot.title=element_text(hjust=0.5))

#plot for perceptive variable
ggplot(LIWC_df, aes(x= percept, y = percept_sum)) + geom_point() + 
  xlim(0, 20)+ylim(0, 20) + geom_abline(intercept = 0, 
                                        slope = 1, linetype = "dashed", color= "red") + 
  theme_bw() +labs(title="Perceptual Processes VS. Sum of Perceptual Processes Subcategories")+
  # move the title text to the middle
  theme(plot.title=element_text(hjust=0.5))

#plot for affect variable
ggplot(LIWC_df, aes(x= bio, y = bio_sum)) + geom_point() + 
  xlim(0, 15)+ylim(0, 15) + geom_abline(intercept = 0, 
                                        slope = 1, linetype = "dashed", color= "red") + 
  theme_bw() +labs(title="Biological Processes VS. Sum of Biological Processes Subcategories")+
  # move the title text to the middle
  theme(plot.title=element_text(hjust=0.5))

#plot for drives variable
ggplot(LIWC_df, aes(x= drives, y = drives_sum)) + geom_point()+ 
  xlim(0, 100)+ylim(0, 100) + geom_abline(intercept = 0, 
                                          slope = 1, linetype = "dashed", color= "red") + 
  theme_bw() +labs(title="Drives VS. Sum of Drives Subcategories")+
  # move the title text to the middle
  theme(plot.title=element_text(hjust=0.5))

#plot for relativity variable
ggplot(LIWC_df, aes(x= relativ, y = relativ_sum)) + geom_point()+ 
  xlim(0, 50)+ylim(0, 50) + geom_abline(intercept = 0, 
                                        slope = 1, linetype = "dashed", color= "red") + 
  theme_bw() +labs(title="Relativity VS. Sum of Relativity Subcategories")+
  # move the title text to the middle
  theme(plot.title=element_text(hjust=0.5))

#plot for informal variable
ggplot(LIWC_df, aes(x= informal, y = informal_sum)) + geom_point() + 
  xlim(0, 100)+ylim(0, 100) + geom_abline(intercept = 0, 
                                          slope = 1, linetype = "dashed", color= "red") + 
  theme_bw() +labs(title="Informal Language VS. Sum of Informal Language Subcategories")+
  # move the title text to the middle
  theme(plot.title=element_text(hjust=0.5))

#plot for affect variable
ggplot(LIWC_df, aes(x= affect, y = affect_sum)) + geom_point() + 
  xlim(0, 100)+ylim(0, 100) + geom_abline(intercept = 0, 
                                          slope = 1, linetype = "dashed", color= "red") + 
  theme_bw() +labs(title="Affective Processes VS. Sum of Affective Processes Subcategories")+
  # move the title text to the middle
  theme(plot.title=element_text(hjust=0.5))

#plot for negative emotions variable
ggplot(LIWC_df, aes(x= negemo, y = negemo_sum)) + geom_point() + 
  xlim(0, 20)+ylim(0, 20) + geom_abline(intercept = 0, 
                                        slope = 1, linetype = "dashed", color= "red") + 
  theme_bw() +labs(title="Negative Emotions VS. Sum of Negative Emotions Subcategories")+
  # move the title text to the middle
  theme(plot.title=element_text(hjust=0.5))
