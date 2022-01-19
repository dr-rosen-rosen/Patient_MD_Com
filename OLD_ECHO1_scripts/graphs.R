library(ggplot2)
library(patchwork)

ggplot(data = x, mapping = aes(x=Sixltr_D,y=cultdissmd9,show.legend=TRUE)) +
  geom_line()+
  facet_wrap(facets = vars(factor(racecat2))) +
  geom_smooth(method = 'lm') +
  theme_light()

table(x$provid)

for (provider in unique(x$provid)){
  for (race in c(1,2)){
    
    test <- ggplot(data = subset(x,racecat2 == race, provid == provider), mapping = aes(x=RC6,y=cultdissmd9,show.legend=TRUE)) +
      geom_line()+
      #facet_grid(rows = vars(provid)) +
      geom_smooth(method = 'lm') +
      theme_light()
    print(test)
    
  }
}