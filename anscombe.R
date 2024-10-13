library(tidyverse)
library(ggpubr)
anscombe_data <- read_csv('anscombe.csv')

ggplot(anscombe_data,aes(x,y))+
  geom_point()+
  facet_wrap('group')+
  theme_minimal()+
  geom_smooth(method = lm,se = F)+
  stat_cor(method="pearson")+
  ylim(0,20)+
  xlim(0,20)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


sum_anscombe_data <- group_by(anscombe_data,group)%>%
  summarise(mean_val = mean(y),
            se_val = sd(y)/sqrt(n()))


ggplot(sum_anscombe_data,aes(x = group,y=mean_val))+
  geom_col(fill='lightgrey')+
  geom_errorbar(aes(ymin=mean_val-se_val,ymax=mean_val+se_val),width=.1)+
  ylim(0,12)+
  theme_minimal()+
  ylab('Mean value')

vals <- c(rnorm(100,500,150),rnorm(100,2000,150))
bmdf <- as.data.frame(vals)

ggplot(bmdf,(aes(1,vals)))+geom_boxplot(width = 1)+
  ylim(0,2500)+
  theme_minimal()+
  geom_jitter(width = .3,colour='grey')+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  xlab("")+
  ylab("Reaction Time")+
  xlim(0,2)

ggplot(bmdf,(aes(x=1,y=mean(vals))))+geom_col(width=.5,colour='grey',fill='grey')+
  ylim(0,1800)+
  theme_minimal()+
  geom_errorbar(aes(ymin=1201,ymax = 1301),width=.1)+
  xlab("")+
  ylab("Reaction Time")+
  xlab("")+
  ylab("Reaction Time")+
  xlim(0,2)+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
