library(R.matlab)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(reshape2)
library(RColorBrewer)
library(ggthemes)
library(robustbase)
library(ggridges)
library(wesanderson)


subs<-sprintf('%d ',1:15)
pop_names=paste("Sub-population ",subs)
#pop_names=factor(subs)

data=readMat('data.mat')
data=data$data
data=data[1:35,]

colnames(data)=pop_names
m=melt(data)

p1<-ggplot(m,aes(x=Var1,y=value))+
  geom_line(colour = "#666666",size=0.8)+
  facet_wrap(~as.factor(Var2),ncol=5)+
  theme_minimal() +
  ylab("Prevalance")+
  xlab("Day")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14))+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))
p1



ggsave('data.png',height = 6,width = 12,last_plot())

