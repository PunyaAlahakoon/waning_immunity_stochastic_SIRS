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


hb=readMat('inf_hb_all.mat')
hb=hb$hb

hb=t(hb)
colnames(hb)<-c(expression(Psi[beta]),expression(Psi[gamma]),expression(Psi[mu]))
hbs=melt(hb)

sig=readMat('inf_hb_sig_all.mat')
sig=sig$hb.sig
sig=t(sig)
colnames(sig)=c(expression(sigma[beta]),expression(sigma[gamma]),expression(sigma[mu]))

sigs=melt(sig)

p1=ggplot(hbs,aes(x=Var1,y=value,color=factor(Var2)))+
  geom_line()+
  scale_color_manual(values=c("#3B9AB2","#9A8822","#0B775E"),labels=c(expression(Psi[beta]),expression(Psi[gamma]),expression(Psi[mu])))+
  facet_wrap(~factor(Var2),labeller="label_parsed",scales="free_y")+
ylab("Parameter space")+
  xlab("Iteration")+
  theme_classic()+
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
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))
p1


p2=ggplot(sigs,aes(x=Var1,y=value,color=factor(Var2)))+
  geom_line()+
  scale_color_manual(values=c("#3B9AB2","#9A8822","#0B775E"))+
  facet_wrap(~factor(Var2),labeller="label_parsed",scales="free_y")+
  ylab("Parameter space")+
  xlab("Iteration")+
  theme_classic()+
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
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p2

plot_grid(p1,p2,ncol=1,labels = c("A","B"))

ggsave("inf_mcmc.png",height = 6,width = 8,last_plot())
