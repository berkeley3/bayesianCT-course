# library(wikipediatrend)

#Adaptive_clinical_trial#Bayesian_designs
# page.view=wp_trend('Adaptive_clinical_trial')
# 
# library(ggplot2)
# ggplot(page.view, aes(x=date, y=count)) + 
#   geom_line(size=1.5, colour="steelblue") + 
#   geom_smooth(method="loess", colour="#00000000", fill="#001090", alpha=0.1) +
#   scale_y_continuous( breaks=seq(0, 100, 10) , 
#                       label=seq(0, 100, 10)) + 
#   theme_bw()


timeline=read.csv('timeline.csv', header=T)
library(ggplot2)
ggplot(subset(timeline, year!=2015), aes(x=year, y=count)) + 
  geom_line(size=1.5, colour="steelblue") + 
  geom_smooth(method="loess", colour="#00000000", fill="#001090", alpha=0.1) +
  scale_y_continuous( breaks=seq(0, 250, 20) , 
                      label=seq(0, 250, 20)) + 
  xlab('year published')+
  ggtitle('Bayesian Clinical Trials hits in PubMed')
  theme_bw()
