#load data:

subs<-sprintf('%d ',1:15)
pop_names=paste("Sub-population ",subs)

u_beta=readMat('ind_inf_beta.mat')
u_beta=u_beta$beta.smc

u_beta=data.frame(u_beta,rep("Independent estimation",5000))
colnames(u_beta)<-c(pop_names,"Method")

i_beta=readMat('hie_inf_beta.mat')
i_beta=i_beta$beta.smc

i_beta=data.frame(i_beta,rep("Hierarchical estimation",5000))
colnames(i_beta)=c(pop_names,"Method")

betas=rbind(u_beta,i_beta)
m=melt(betas)

t_betas=readMat('true_betas.mat')
t_betas=t_betas$t.betas
t_betas=data.frame(t_betas)
colnames(t_betas)<-pop_names
m2=melt(t_betas)


p1=ggplot(m)+
 # geom_histogram(aes(x=value,y=..density..,fill=as.factor(Method)),binwidth = 0.06,alpha=0.8,position="identity")+
  geom_density(aes(x=value,fill=factor(Method)),alpha=0.8,adjust =0.5)+
  facet_wrap(~factor(variable),ncol=5)+
  geom_segment(data=m2,aes(x=value,y=0,xend=value,yend=2,colour =   "#3F5151"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c(  "#3F5151"),
                      labels = c("True parameter value"))+
  scale_fill_manual(values =c("#9A8822","#3B9AB2" ) )+
  theme_classic(16) +
  ylab("Density")+
  xlab(expression(beta))+
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
  theme(plot.title = element_text(hjust = 0.5))
p1

ggsave("hie_ind_inf_betas.png",height =6,width = 12,last_plot())


subs<-sprintf('%d ',1:15)
pop_names=paste("Sub-population ",subs)

u_gamma=readMat('ind_inf_gamma.mat')
u_gamma=u_gamma$gamma.smc

u_gamma=data.frame(u_gamma,rep("Independent estimation",5000))
colnames(u_gamma)<-c(pop_names,"Method")

i_gamma=readMat('hie_inf_gamma.mat')
i_gamma=i_gamma$gamma.smc

i_gamma=data.frame(i_gamma,rep("Hierarchical estimation",5000))
colnames(i_gamma)=c(pop_names,"Method")

gammas=rbind(u_gamma,i_gamma)
m=melt(gammas)

t_gammas=readMat('true_gammas.mat')
t_gammas=t_gammas$t.gammas
t_gammas=data.frame(t_gammas)
colnames(t_gammas)<-pop_names
m2=melt(t_gammas)

p2=ggplot(m)+
  geom_histogram(aes(x=value,y=..density..,fill=as.factor(Method)),binwidth = 0.015,alpha=0.8,position="identity")+
  facet_wrap(~factor(variable),ncol=5)+
  geom_segment(data=m2,aes(x=value,y=0,xend=value,yend=8,colour =   "#3F5151"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c(  "#3F5151"),
                      labels = c("True parameter value"))+
  scale_fill_manual(values =c("#9A8822","#3B9AB2" ),labels=c("Informative prior","Unifomative prior") )+
  theme_classic(10) +
  ylab("Density")+
  xlab(expression(gamma))+
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
  theme(plot.title = element_text(hjust = 0.5))
p2

ggsave("hie_ind_inf_gammas.png",height = 6,width = 10,last_plot())



#subs<-sprintf('%d ',1:15)
#pop_names=paste("Sub-population ",subs)
pop_names=c(paste("Sub-population ",sprintf('%d ',1:3)), "Sub-population 4*", paste("Sub-population ",sprintf('%d ',5:7)),
            "Sub-population 8*", "Sub-population 9","Sub-population 10*","Sub-population 11*","Sub-population 12*",
            "Sub-population 13","Sub-population 14*","Sub-population 15*")
  
  
u_epsilon=readMat('ind_inf_epsilon.mat')
u_epsilon=u_epsilon$epsilon.smc


u_epsilon=data.frame(u_epsilon,rep("Independent estimation",5000))
colnames(u_epsilon)<-c(pop_names,"Method")

i_epsilon=readMat('hie_inf_epsilon.mat')
i_epsilon=i_epsilon$epsilon.smc

i_epsilon=data.frame(i_epsilon,rep("Hierarchical estimation",5000))
colnames(i_epsilon)=c(pop_names,"Method")

epsilons=rbind(u_epsilon,i_epsilon)
m=melt(epsilons)

t_epsilons=readMat('true_mus.mat')
t_epsilons=t_epsilons$t.mus
t_epsilons=data.frame(t_epsilons)
colnames(t_epsilons)<-pop_names
m2=melt(t_epsilons)

p3=ggplot(m)+
 # geom_histogram(aes(x=value,y=..density..,fill=as.factor(Method)),binwidth = 0.0015,alpha=0.8,position="identity")+
  geom_density(aes(x=value,fill=factor(Method)),alpha=0.8,adjust =0.5)+
  facet_wrap(~factor(variable),ncol=5)+
  geom_segment(data=m2,aes(x=value,y=0,xend=value,yend=65,colour =   "#3F5151"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c(  "#3F5151"),
                      labels = c("True parameter value"))+
  scale_fill_manual(values =c("#046C9A", "#D69C4E" ) )+
  theme_minimal(16)+
  #ylim(0,35)+
  ylab("Density")+
  xlab("Waning immunity rate")+
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
  theme(plot.title = element_text(hjust = 0.5))
p3





ggsave("ECMTB_hie_ind_inf_epsilons_density.png",bg = "transparent",height = 6,width = 10,last_plot())





#subs<-sprintf('%d ',1:15)
#pop_names=paste("Sub-population ",subs)
pop_names=c( "Sub-population 4*",
            "Sub-population 8*","Sub-population 10*","Sub-population 11*","Sub-population 12*",
            "Sub-population 14*","Sub-population 15*")


u_epsilon=readMat('ind_inf_epsilon.mat')
u_epsilon=u_epsilon$epsilon.smc
u_epsilon=u_epsilon[,c(4,8,10,11,12,14,15)]

u_epsilon=data.frame(u_epsilon,rep("Independent estimation",5000))
colnames(u_epsilon)<-c(pop_names,"Method")

i_epsilon=readMat('hie_inf_epsilon.mat')
i_epsilon=i_epsilon$epsilon.smc
i_epsilon=i_epsilon[,c(4,8,10,11,12,14,15)]

i_epsilon=data.frame(i_epsilon,rep("Hierarchical estimation",5000))
colnames(i_epsilon)=c(pop_names,"Method")

epsilons=rbind(u_epsilon,i_epsilon)
m=melt(epsilons)

t_epsilons=readMat('true_mus.mat')
t_epsilons=t_epsilons$t.mus
t_epsilons=t_epsilons[1,c(4,8,10,11,12,14,15)]
t_epsilons=data.frame(t_epsilons,pop_names)
#colnames(t_epsilons)<-pop_names
m2=melt(t_epsilons)
m2=data.frame("variable"=m2$pop_names,"value"=m2$value)

p3=ggplot(m)+
  # geom_histogram(aes(x=value,y=..density..,fill=as.factor(Method)),binwidth = 0.0015,alpha=0.8,position="identity")+
  geom_density(aes(x=value,fill=factor(Method)),alpha=0.8,adjust =0.5)+
  facet_wrap(~factor(variable),ncol=4)+
  geom_segment(data=m2,aes(x=value,y=0,xend=value,yend=65,colour =   "#3F5151"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c(  "#3F5151"),
                      labels = c("True parameter value"))+
  scale_fill_manual(values =c("#9A8822","#3B9AB2" ) )+
  theme_classic(16) +
  #ylim(0,35)+
  ylab("Density")+
  xlab(expression(mu))+
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
  theme(plot.title = element_text(hjust = 0.5))
p3

ggsave("hie_ind_inf_epsilons_density_fades.png",height = 6,width = 12,last_plot())
