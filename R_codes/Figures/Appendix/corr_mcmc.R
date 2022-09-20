#library(R.matlab)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(reshape2)
library(RColorBrewer)
library(ggthemes)
library(robustbase)
library(ggridges)
library(wesanderson)
library(GGally)

library(trialr)
hb=read.csv('hb4.csv')
hb=data.frame(hb[,1:4])

#hb=t(hb)
colnames(hb)<-c("Iteration",expression(Psi[beta]),expression(Psi[gamma]),expression(Psi[mu]))
hbs=melt(hb,id="Iteration")

sig=read.csv('hb_sig4.csv')
sig=data.frame(sig[,1:4])
#sig=t(sig)
colnames(sig)=c("Iteration",expression(sigma[beta]),expression(sigma[gamma]),expression(sigma[mu]))

sigs=melt(sig,id="Iteration")

ros=read.csv('ros4.csv')
ros=data.frame(ros$X,ros[,3:4],ros$V6)
colnames(ros)=c("Iteration",expression(rho[beta*gamma]),expression(rho[beta*mu]),expression(rho[gamma*mu]))
ross=melt(ros,id="Iteration")

p1=ggplot(hbs,aes(x=Iteration,y=value,color=factor(variable)))+
  geom_line()+
  scale_color_manual(values=c("#3B9AB2","#9A8822","#0B775E"),labels=c(expression(Psi[beta]),expression(Psi[gamma]),expression(Psi[mu])))+
  facet_wrap(~factor(variable),labeller="label_parsed",scales="free_y")+
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


p2=ggplot(sigs,aes(x=Iteration,y=value,color=factor(variable)))+
  geom_line()+
  scale_color_manual(values=c("#3B9AB2","#9A8822","#0B775E"))+
  facet_wrap(~factor(variable),labeller="label_parsed",scales="free_y")+
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

p3=ggplot(ross,aes(x=Iteration,y=value,color=factor(variable)))+
  geom_line()+
  scale_color_manual(values=c("#3B9AB2","#9A8822","#0B775E"))+
  facet_wrap(~factor(variable),labeller="label_parsed",scales="free_y")+
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

p3

hbb=hb[5001:10000,2:4]
colnames(hbb)<-c("Transmsion","Recovery","Waning immunity")

p=ggpairs(hbb, columns = 1:3,
          #upper = list(continuous = "density", combo = "box_no_facet"),
        #  upper =  "blank",
          #legend=3,
          diag=list(continuous = wrap('densityDiag',alpha=0.7)),
          lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot", alpha = 0.7)))  +
   scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")+
  theme_classic(10) +
  ylab("Parameter space")+
  xlab("Parameter space")+
 # ggtitle("Sub-population 17")+
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
  theme(plot.title = element_text(hjust = 0.5))
p


R=rlkjcorr(1e5, 3, eta = 2)
Rd=data.frame(1:1e4,R[,1,2],R[,1,2],R[,1,2])
colnames(Rd)=c("Iteration",expression(rho[beta*gamma]),expression(rho[beta*mu]),expression(rho[gamma*mu]))
Rrd=melt(Rd,id="Iteration")

ros=read.csv('ros4.csv')
ros=data.frame(ros[5001:10000,1],ros[5001:10000,3:4],ros[5001:10000,7])
colnames(ros)=c("Iteration",expression(rho[beta*gamma]),expression(rho[beta*mu]),expression(rho[gamma*mu]))
ross=melt(ros,id="Iteration")


pr=ggplot(ross)+
  geom_histogram(aes(x=value,y=..density..,fill=variable),binwidth = 0.05,alpha=0.8,position="identity")+
  geom_density(data=Rrd,aes(x=value,y=..density..),alpha=0.8,position="identity")+
  scale_fill_manual(values=c("#3B9AB2","#9A8822","#0B775E"))+
  facet_wrap(~factor(variable),labeller="label_parsed",scales="free_y")+
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

pr

plot_grid(p3,pr,ncol=1,labels = c("Trace plots","Posteriors"))

plot_grid(p1,p2,ncol=1,labels = c("A","B"))
ggsave("cor_ind_mcmc.png",height = 6,width = 8,last_plot())

ggsave("rkj2_cor_ind_mcmc.png",height = 6,width = 8,last_plot())



ggsave("corr_plot3d.png",p)
