#load data:

subs<-sprintf('%d ',1:15)
pop_names=paste("Sub-population ",subs)

u_beta=readMat('ind_unif_beta.mat')
u_beta=u_beta$beta.smc

u_beta=data.frame(u_beta,rep("Independent \n (Less informative prior)",5000))
colnames(u_beta)<-c(pop_names,"Method")

i_beta=readMat('ind_inf_beta.mat')
i_beta=i_beta$beta.smc

i_beta=data.frame(i_beta,rep("Independent \n (Strong informative prior)",5000))
colnames(i_beta)=c(pop_names,"Method")


u_hie_beta=readMat('hie_unif_beta.mat')
u_hie_beta=u_hie_beta$beta.smc

u_hie_beta=data.frame(u_hie_beta,rep("Hierarchical \n (Less informative prior)",5000))
colnames(u_hie_beta)<-c(pop_names,"Method")

i_hie_beta=readMat('hie_inf_beta.mat')
i_hie_beta=i_hie_beta$beta.smc

i_hie_beta=data.frame(i_hie_beta,rep("Hierarchical \n (Strong informative prior)",5000))
colnames(i_hie_beta)=c(pop_names,"Method")

betas=rbind(u_beta,i_beta,u_hie_beta,i_hie_beta)
m=melt(betas)

t_betas=readMat('true_betas.mat')
t_betas=t_betas$t.betas
t_betas=data.frame(t_betas)
colnames(t_betas)<-pop_names
m2=melt(t_betas)


p1=ggplot(m)+
 # geom_histogram(aes(x=value,y=..density..,fill=as.factor(Method)),binwidth = 0.05,alpha=0.7,position="identity")+
  geom_density(aes(x=value,fill=factor(Method)),alpha=0.8,adjust =0.5)+
  facet_wrap(~factor(variable),ncol=5)+
  geom_segment(data=m2,aes(x=value,y=0,xend=value,yend=1.5,colour =   "#3F5151"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c(  "#3F5151"),
                      labels = c("True parameter \n value"))+
  scale_fill_manual(values =c("#9A8822","#0B775E", "#D8A499","#3B9AB2","#046C9A") )+
  theme_minimal(16) +
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

ggsave("hie_all_betas.png",height = 7,width = 14,last_plot())


#load data:

subs<-sprintf('%d ',1:15)
pop_names=paste("Sub-population ",subs)

u_gamma=readMat('ind_unif_gamma.mat')
u_gamma=u_gamma$gamma.smc

u_gamma=data.frame(u_gamma,rep("Independent \n (Less informative prior)",1000))
colnames(u_gamma)<-c(pop_names,"Method")

i_gamma=readMat('ind_inf_gamma.mat')
i_gamma=i_gamma$gamma.smc

i_gamma=data.frame(i_gamma,rep("Independent \n (Strong informative prior)",1000))
colnames(i_gamma)=c(pop_names,"Method")


u_hie_gamma=readMat('hie_unif_gamma.mat')
u_hie_gamma=u_hie_gamma$gamma.smc

u_hie_gamma=data.frame(u_hie_gamma,rep("Hierarchical \n (Less informative prior)",1000))
colnames(u_hie_gamma)<-c(pop_names,"Method")

i_hie_gamma=readMat('hie_inf_gamma.mat')
i_hie_gamma=i_hie_gamma$gamma.smc

i_hie_gamma=data.frame(i_hie_gamma,rep("Hierarchical \n (Strong informative prior)",1000))
colnames(i_hie_gamma)=c(pop_names,"Method")

gammas=rbind(u_gamma,i_gamma,u_hie_gamma,i_hie_gamma)
m=melt(gammas)

t_gammas=readMat('true_gammas.mat')
t_gammas=t_gammas$t.gammas
t_gammas=data.frame(t_gammas)
colnames(t_gammas)<-pop_names
m2=melt(t_gammas)


p2=ggplot(m)+
  #geom_histogram(aes(x=value,y=..density..,fill=as.factor(Method)),binwidth = 0.005,alpha=0.7,position="identity")+
  geom_density(aes(x=value,fill=factor(Method)),alpha=0.8,adjust =0.5)+
  facet_wrap(~factor(variable),ncol=5)+
  geom_segment(data=m2,aes(x=value,y=0,xend=value,yend=10,colour =   "#3F5151"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c(  "#3F5151"),
                      labels = c("True parameter value"))+
  scale_fill_manual(values =c("#9A8822","#0B775E", "#D8A499","#3B9AB2","#046C9A") )+
  theme_minimal(16) +
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

ggsave("hie_all_gammas.png",height = 7,width = 14,last_plot())


#load data:

subs<-sprintf('%d ',1:15)
pop_names=paste("Sub-population ",subs)

u_epsilon=readMat('ind_unif_epsilon.mat')
u_epsilon=u_epsilon$epsilon.smc

u_epsilon=data.frame(u_epsilon,rep("Independent \n (Less informative prior)",1000))
colnames(u_epsilon)<-c(pop_names,"Method")

i_epsilon=readMat('ind_inf_epsilon.mat')
i_epsilon=i_epsilon$epsilon.smc

i_epsilon=data.frame(i_epsilon,rep("Independent \n (Strong informative prior)",1000))
colnames(i_epsilon)=c(pop_names,"Method")


u_hie_epsilon=readMat('hie_unif_epsilon.mat')
u_hie_epsilon=u_hie_epsilon$epsilon.smc

u_hie_epsilon=data.frame(u_hie_epsilon,rep("Hierarchical \n (Less informative prior)",1000))
colnames(u_hie_epsilon)<-c(pop_names,"Method")

i_hie_epsilon=readMat('hie_inf_epsilon.mat')
i_hie_epsilon=i_hie_epsilon$epsilon.smc

i_hie_epsilon=data.frame(i_hie_epsilon,rep("Hierarchical \n (Strong informative prior)",1000))
colnames(i_hie_epsilon)=c(pop_names,"Method")

epsilons=rbind(u_epsilon,i_epsilon,u_hie_epsilon,i_hie_epsilon)
m=melt(epsilons)

t_epsilons=readMat('true_mus.mat')
t_epsilons=t_epsilons$t.mus
t_epsilons=data.frame(t_epsilons)
colnames(t_epsilons)<-pop_names
m2=melt(t_epsilons)


p3=ggplot(m)+
  #geom_histogram(aes(x=value,y=..density..,fill=as.factor(Method)),binwidth = 0.0015,alpha=0.7,position="identity")+
  geom_density(aes(x=value,fill=factor(Method)),alpha=0.8,adjust =0.5)+
  facet_wrap(~factor(variable),ncol=5)+
  geom_segment(data=m2,aes(x=value,y=0,xend=value,yend=65,colour =   "#3F5151"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c(  "#3F5151"),
                      labels = c("True parameter \n value"))+
  scale_fill_manual(values =c("#9A8822","#0B775E", "#D8A499","#3B9AB2","#046C9A") )+
  theme_minimal(16) +
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

ggsave("hie_all_epsilons.png",height = 7,width = 14,last_plot())



