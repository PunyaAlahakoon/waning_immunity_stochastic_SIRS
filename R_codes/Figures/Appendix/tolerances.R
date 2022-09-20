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
pop_names=paste("Sub-popuation ",subs)


betas=readMat('beta_unif.mat')
betas=betas$beta.smc

betas=data.frame(betas,rep('betas',1000))
colnames(betas)<-c(pop_names,"para")
#b=melt(betas)


gammas=readMat('gamma_unif.mat')
gammas=gammas$gamma.smc

gammas=data.frame(gammas,rep('gammas',1000))
colnames(gammas)<-c(pop_names,"para")
#g=melt(gammas)


ep=readMat('epsilon_unif.mat')
ep=ep$epsilon.smc

ep=data.frame(ep,rep('ep',1000))
colnames(ep)<-c(pop_names,"para")
#e=melt(ep)


sx=readMat('sx_unif.mat')
sx=sx$s.x

sx=data.frame(sx,rep("sx",1000))
colnames(sx)<-c(pop_names,"para")
s=melt(sx)



dd=rbind(melt(betas),melt(gammas),melt(ep))
dt=data.frame(dd,rep(s$value,3))
colnames(dt)<-c("para","pop","x","y")






p1<-ggplot(data=dt,aes(x=x,y=y,color=para))+
  geom_point()+
  facet_wrap(~factor(pop),ncol=5)+
  scale_color_manual(values=c("#3B9AB2","#9A8822","#0B775E"),labels=c(expression(beta),expression(mu),expression(gamma)))+
  theme_classic(12) +
  # ylim(0,35)+
  ylab("Distance metric")+
  xlab("Parameter space")+
  #  ggtitle("Hyper-parameters")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        strip.text.x = element_text(),
        strip.text.y = element_text())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust =1.1))
p1







ggsave('unif_tol.png',height=6,width=10)
