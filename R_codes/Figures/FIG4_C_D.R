library(ggpubr)

#load  data:
names=c("Sub-population","Independent estimation", "Hierarchical estimation")

#HPS+-0.5
betas=read.csv("HPD_percents_beta.csv")
colnames(betas)<-names

#hdps+-0.025
mus=read.csv("HPD_percents_mu.csv")
colnames(mus)<-names

#subs that had fade-outs:
fades=c(4,8,10,11,12,14,15)
no_fades=c(1:3,5:7,9,13)



d1=data.frame(betas[fades,2],betas[fades,3])
names=c("Independent estimation", "Hierarchical estimation")
colnames(d1)<-names

p1= ggpaired(d1, cond1  = "Independent estimation", cond2= "Hierarchical estimation",line.color = "gray", line.size = 0.4,
             fill = "condition", palette = c("#D69C4E","#046C9A" ))+
  xlab("")+
  ylim(0,100)+
  ylab("ROPE percentage ")+
  theme_minimal(14)+
  ggtitle("Fade-outs")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        strip.text.x = element_text(),
        strip.text.y = element_text())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))


p1

d1=data.frame(betas[no_fades,2],betas[no_fades,3])
names=c("Independent estimation", "Hierarchical estimation")
colnames(d1)<-names


p11= ggpaired(d1, cond1  = "Independent estimation", cond2= "Hierarchical estimation",line.color = "gray", line.size = 0.4,
              fill = "condition", palette = c("#D69C4E","#046C9A" ))+
  xlab("")+
  ylim(0,100)+
  ylab("ROPE percentage ")+
  theme_minimal(14)+
  ggtitle("Non fade-outs")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        strip.text.x = element_text(),
        strip.text.y = element_text())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))


p11


bet<-ggarrange(p1,p11,ncol = 2,common.legend = T,legend = 'none',labels = c("(G)","(H)"))
bet

all_bet<-annotate_figure(bet, top = text_grob("Comparison of ROPE percentages", 
                                              color = "black", face = "bold", size = 14 ,vjust =-0.5))
all_bet


u_epsilon=readMat('mode_inf_ind_beta.mat')
u_epsilon=u_epsilon$ans


u_epsilon=data.frame(u_epsilon,rep("Independent estimation",15))
colnames(u_epsilon)<-c("Mode","Method")

i_epsilon=readMat('mode_inf_hie_beta.mat')
i_epsilon=i_epsilon$ans

i_epsilon=data.frame(i_epsilon,rep("Hierarchical estimation",15))
colnames(i_epsilon)=c("Mode","Method")


d2=data.frame(u_epsilon[fades,1], i_epsilon[fades,1])
names=c("Independent estimation", "Hierarchical estimation")
colnames(d2)<-names

pb= ggpaired(d2, cond1  = "Independent estimation", cond2= "Hierarchical estimation",line.color = "gray", line.size = 0.4
             ,fill = "condition", palette = c("#D69C4E","#046C9A" ))+
  xlab("")+
  ylim(1,4)+
  ylab("Posterior mode ")+
  theme_minimal(14)+
  ggtitle("Fade-outs")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        strip.text.x = element_text(),
        axis.text.x=element_blank(),
        strip.text.y = element_text())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))
pb

d2=data.frame(u_epsilon[no_fades,1], i_epsilon[no_fades,1])
names=c("Independent estimation", "Hierarchical estimation")
colnames(d2)<-names

pbb= ggpaired(d2, cond1  = "Independent estimation", cond2= "Hierarchical estimation",line.color = "gray", line.size = 0.4
              ,fill = "condition", palette = c("#D69C4E","#046C9A" ))+
  xlab("")+
  ylim(1,4)+
  ylab("Posterior mode")+
  theme_minimal(14)+
  ggtitle("Non fade-outs")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        strip.text.x = element_text(),
        axis.text.x=element_blank(),
        strip.text.y = element_text())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))
pbb


bet_m<-ggarrange(pb,pbb,ncol = 2,common.legend = T,legend = 'bottom',labels = c("(I)","(J)"))
bet_m

all_bet_m<-annotate_figure(bet_m ,top = text_grob("Comparison of posterior modes", 
                                              color = "black", face = "bold", size = 14,vjust =-0.5))
all_bet_m

tranm<-ggarrange(all_bet,all_bet_m,ncol = 1,common.legend = T,legend = 'bottom')+
  theme(plot.margin = margin(0.5, 0.5, 0, 0, "cm"))

tranm2<-annotate_figure(tranm ,top = text_grob("Transmission rates", color = "black", face = "bold", size = 16))   
                                                                                                        
tranm3<-tranm2 + theme(plot.margin = margin(1, 1, 1, 1, "cm"))
tranm3

d2=data.frame(mus[fades,2],mus[fades,3])
names=c("Independent estimation", "Hierarchical estimation")
colnames(d2)<-names

p2= ggpaired(d2, cond1  = "Independent estimation", cond2= "Hierarchical estimation",line.color = "gray", line.size = 0.4
             ,fill = "condition", palette = c("#D69C4E","#046C9A" ))+
  xlab("")+
  ylim(0,100)+
  ylab("ROPE percentage ")+
  theme_minimal(14)+
  ggtitle("Fade-outs")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        strip.text.x = element_text(),
        axis.text.x=element_blank(),
        strip.text.y = element_text())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))
p2

d2=data.frame(mus[no_fades,2],mus[no_fades,3])
names=c("Independent estimation", "Hierarchical estimation")
colnames(d2)<-names

p22= ggpaired(d2, cond1  = "Independent estimation", cond2= "Hierarchical estimation",line.color = "gray", line.size = 0.4
              ,fill = "condition", palette = c("#D69C4E","#046C9A" ))+
  xlab("")+
  ylim(0,100)+
  ylab("ROPE percentage ")+
  theme_minimal(14)+
  ggtitle("Non fade-outs")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        strip.text.x = element_text(),
        axis.text.x=element_blank(),
        strip.text.y = element_text())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))
p22






u_epsilon=readMat('mode_inf_ind_epsilon.mat')
u_epsilon=u_epsilon$ans


u_epsilon=data.frame(u_epsilon,rep("Independent estimation",15))
colnames(u_epsilon)<-c("Mode","Method")

i_epsilon=readMat('mode_inf_hie_epsilon.mat')
i_epsilon=i_epsilon$ans

i_epsilon=data.frame(i_epsilon,rep("Hierarchical estimation",15))
colnames(i_epsilon)=c("Mode","Method")


d2=data.frame(u_epsilon[fades,1], i_epsilon[fades,1])
names=c("Independent estimation", "Hierarchical estimation")
colnames(d2)<-names

p3= ggpaired(d2, cond1  = "Independent estimation", cond2= "Hierarchical estimation",line.color = "gray", line.size = 0.4
             ,fill = "condition", palette = c("#D69C4E","#046C9A" ))+
  xlab("")+
  ylim(0,0.1)+
  ylab("Posterior mode ")+
  theme_minimal(14)+
  ggtitle("Fade-outs")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        strip.text.x = element_text(),
        axis.text.x=element_blank(),
        strip.text.y = element_text())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))
p3

d2=data.frame(u_epsilon[no_fades,1], i_epsilon[no_fades,1])
names=c("Independent estimation", "Hierarchical estimation")
colnames(d2)<-names

p33= ggpaired(d2, cond1  = "Independent estimation", cond2= "Hierarchical estimation",line.color = "gray", line.size = 0.4
              ,fill = "condition", palette = c("#D69C4E","#046C9A" ))+
  xlab("")+
  ylim(0,0.1)+
  ylab("Posterior mode")+
  theme_minimal(14)+
  ggtitle("Non fade-outs")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.title.y=element_blank(),
        strip.text.x = element_text(),
        axis.text.x=element_blank(),
        strip.text.y = element_text())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))
p33




m1<-ggarrange(p2,p22,ncol = 2, common.legend = TRUE,legend = "none",labels = c("(C)","(D)"))
m1
m11<-annotate_figure(m1, top = text_grob("Comparison of ROPE percentages", 
                                            color = "black", face = "bold", size = 14,vjust =-0.5))
m11

m2<-ggarrange(p3,p33,ncol = 2, common.legend = TRUE,legend = "none",labels = c("(E)","(F)"))
m2
m22<-annotate_figure(m2, top = text_grob("Comparison of posterior modes", 
                                         color = "black", face = "bold", size = 14,vjust =-0.5))
m22

all_m=ggarrange(m11,m22,ncol = 1, common.legend = TRUE,legend = "none")+
  theme(plot.margin = margin(0.5, 0.5, 0, 0, "cm"))
all_m



mu2<-annotate_figure(all_m ,top = text_grob("Waning immunity rates", color = "black", face = "bold", size = 16))   

mu3<-mu2 + theme(plot.margin = margin(1, 1, 1, 1, "cm"))

mu3

p0x<-ggarrange(mu3,tranm3,ncol = 1, common.legend = TRUE,legend = "bottom")

#p0x<-plot_grid(mu3,tranm3,ncol=1,rel_heights = c(1, 1))
p0x

ggarrange(rid,p0x,ncol=2)

#ggsave('hie_esti_ROPE.png',last_plot(),height = 18,width = 15)
