
u_epsilon=readMat('ind_inf_pred_epsilon.mat')
u_epsilon=u_epsilon$epsilon.smc[,1]

u_epsilon=data.frame(u_epsilon,rep("Independent estmation",5000))
colnames(u_epsilon)<-c(pop_names[1],"Method")

i_epsilon=readMat('hie_inf_pred_epsilon.mat')
i_epsilon=i_epsilon$epsilon.smc[,1]

i_epsilon=data.frame(i_epsilon,rep("Hierarchical estimation",5000))
colnames(i_epsilon)=c(pop_names[1],"Method")

epsilons=rbind(u_epsilon,i_epsilon)
m=melt(epsilons)


t_epsilons=readMat('true_sc2_mus.mat')
t_epsilons=t_epsilons$t.mus
t_epsilons=data.frame(t_epsilons)
colnames(t_epsilons)<-pop_names
m2=melt(t_epsilons)

m$Method=factor(m$Method)

p4=ggplot(m)+
  geom_density(aes(x=value,y=..density..,fill=as.factor(Method)),alpha=0.8,position="identity")+
  #  geom_histogram(aes(x=value,y=..density..,fill=as.factor(Method)),binwidth = 0.0025,alpha=0.8,position="identity")+
  #facet_wrap(~factor(variable),ncol=5)+
  geom_segment(data=NULL,aes(x=0.06422233,y=0,xend=0.06422233,yend=50,colour =   "#3F5151"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c(  "#3F5151"),
                      labels = c("True parameter value"))+
  scale_fill_manual(values =c("#046C9A", "#D69C4E"))+
  theme_minimal(12) +
  # ylim(0,35)+
  ylab("Density")+
  xlab("Waning immunity rate")+
  ggtitle("Sub-population 16")+
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
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+
  theme(plot.title = element_text(hjust = 0.5))
p4



u_epsilon=readMat('ind_inf_pred_epsilon.mat')
u_epsilon=u_epsilon$epsilon.smc[,2]

u_epsilon=data.frame(u_epsilon,rep("Independent estmation",5000))
colnames(u_epsilon)<-c(pop_names[2],"Method")

i_epsilon=readMat('hie_inf_pred_epsilon.mat')
i_epsilon=i_epsilon$epsilon.smc[,2]

i_epsilon=data.frame(i_epsilon,rep("Hierarchical estimation",5000))
colnames(i_epsilon)=c(pop_names[2],"Method")

epsilons=rbind(u_epsilon,i_epsilon)
m=melt(epsilons)


t_epsilons=readMat('true_sc2_mus.mat')
t_epsilons=t_epsilons$t.mus
t_epsilons=data.frame(t_epsilons)
colnames(t_epsilons)<-pop_names
m2=melt(t_epsilons)

m$Method=factor(m$Method)

p5=ggplot(m)+
  geom_density(aes(x=value,y=..density..,fill=as.factor(Method)),alpha=0.8,position="identity")+
  #  geom_histogram(aes(x=value,y=..density..,fill=as.factor(Method)),binwidth = 0.0025,alpha=0.8,position="identity")+
  #facet_wrap(~factor(variable),ncol=5)+
  geom_segment(data=NULL,aes(x=0.04425183,y=0,xend=0.04425183,yend=50,colour =   "#3F5151"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c(  "#3F5151"),
                      labels = c("True parameter value"))+
  scale_fill_manual(values =c("#046C9A", "#D69C4E"))+
  theme_minimal(12) +
  # ylim(0,35)+
  ylab("Density")+
  xlab("Waning immunity rate")+
  ggtitle("Sub-population 17")+ 
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
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+
  theme(plot.title = element_text(hjust = 0.5))
p5


post=ggarrange(p4,p5,common.legend = TRUE,legend = "bottom")
post

post2<-annotate_figure(post ,top = text_grob("Marginal posterior distributions of waning immunity rate", color = "black", face = "bold", size = 14))   

post3<-post2 + theme(plot.margin = margin(1, 1, 0.35, 0.35, "cm"))
post3

#betas:

u_epsilon=readMat('ind_inf_pred_beta.mat')
u_epsilon=u_epsilon$beta.smc[,1]

u_epsilon=data.frame(u_epsilon,rep("Independent estmation",5000))
colnames(u_epsilon)<-c(pop_names[1],"Method")

i_epsilon=readMat('hie_inf_pred_beta.mat')
i_epsilon=i_epsilon$beta.smc[,1]

i_epsilon=data.frame(i_epsilon,rep("Hierarchical estimation",5000))
colnames(i_epsilon)=c(pop_names[1],"Method")

epsilons=rbind(u_epsilon,i_epsilon)
m=melt(epsilons)


t_epsilons=readMat('true_sc2_betas.mat')
t_epsilons=t_epsilons$t.betas
t_epsilons=data.frame(t_epsilons)
colnames(t_epsilons)<-pop_names
m2=melt(t_epsilons)

m$Method=factor(m$Method)

p4=ggplot(m)+
  geom_density(aes(x=value,y=..density..,fill=as.factor(Method)),alpha=0.8,position="identity")+
  #  geom_histogram(aes(x=value,y=..density..,fill=as.factor(Method)),binwidth = 0.0025,alpha=0.8,position="identity")+
  #facet_wrap(~factor(variable),ncol=5)+
  geom_segment(data=NULL,aes(x=2.635468,y=0,xend=2.635468,yend=3.5,colour =   "#3F5151"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c(  "#3F5151"),
                      labels = c("True parameter value"))+
  scale_fill_manual(values =c("#046C9A", "#D69C4E"))+
  theme_minimal(12) +
  xlim(1,4)+
  # ylim(0,35)+
  ylab("Density")+
  xlab("Transmission rate")+
  ggtitle("Sub-population 16")+
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
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+
  theme(plot.title = element_text(hjust = 0.5))
p4



u_epsilon=readMat('ind_inf_pred_beta.mat')
u_epsilon=u_epsilon$beta.smc[,2]

u_epsilon=data.frame(u_epsilon,rep("Independent estmation",5000))
colnames(u_epsilon)<-c(pop_names[2],"Method")

i_epsilon=readMat('hie_inf_pred_beta.mat')
i_epsilon=i_epsilon$beta.smc[,2]

i_epsilon=data.frame(i_epsilon,rep("Hierarchical estimation",5000))
colnames(i_epsilon)=c(pop_names[2],"Method")

epsilons=rbind(u_epsilon,i_epsilon)
m=melt(epsilons)




m$Method=factor(m$Method)

p5=ggplot(m)+
  geom_density(aes(x=value,y=..density..,fill=as.factor(Method)),alpha=0.8,position="identity")+
  #  geom_histogram(aes(x=value,y=..density..,fill=as.factor(Method)),binwidth = 0.0025,alpha=0.8,position="identity")+
  #facet_wrap(~factor(variable),ncol=5)+
  geom_segment(data=NULL,aes(x=2.036372,y=0,xend=2.036372,yend=3.5,colour =   "#3F5151"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c(  "#3F5151"),
                      labels = c("True parameter value"))+
  scale_fill_manual(values =c("#046C9A", "#D69C4E"))+
  theme_minimal(12) +
  xlim(1,4)+
  # ylim(0,35)+
  ylab("Density")+
  xlab("Transmission rate")+
  ggtitle("Sub-population 17")+ 
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
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+
  theme(plot.title = element_text(hjust = 0.5))
p5


post=ggarrange(p4,p5,common.legend = TRUE,legend = "bottom")
post

post2<-annotate_figure(post ,top = text_grob("Marginal posterior distributions of transmission rate", color = "black", face = "bold", size = 14))   

post4<-post2 + theme(plot.margin = margin(1, 1, 0.35, 0.35, "cm"))
post4

