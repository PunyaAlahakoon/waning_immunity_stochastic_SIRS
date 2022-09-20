library(GGally)
subs<-sprintf('%d ',16:17)
pop_names=paste("Sub-popuation ",subs)


u_beta=readMat('ind_inf_pred_beta.mat')
u_beta=u_beta$beta.smc[,1]


u_beta=data.frame(u_beta,rep("Independent estimation",5000))
colnames(u_beta)<-c(pop_names[1],"Method")

i_beta=readMat('hie_inf_pred_beta.mat')
i_beta=i_beta$beta.smc[,1]

i_beta=data.frame(i_beta,rep("Hierarchical estimation",5000))
colnames(i_beta)=c(pop_names[1],"Method")

cu_beta=readMat('ind_inf_beta_comp.mat')
cu_beta=cu_beta$beta.smc[,1]
cu_beta=data.frame(cu_beta,rep("Independent estimation \n (complete data)",5000))
colnames(cu_beta)<-c(pop_names[1],"Method")


ci_beta=readMat('hie_inf_pred_beta_comp.mat')
ci_beta=ci_beta$beta.smc[,1]

ci_beta=data.frame(ci_beta,rep("Hierarchical estimation \n (complete data)",5000))
colnames(ci_beta)=c(pop_names[1],"Method")


betas=rbind(u_beta,i_beta,cu_beta,ci_beta)
m=melt(betas)

t_betas=readMat('true_sc2_betas.mat')
t_betas=t_betas$t.betas
t_betas=data.frame(t_betas)
colnames(t_betas)<-pop_names
mt1=melt(t_betas)

u_gamma=readMat('ind_inf_pred_gamma.mat')
u_gamma=u_gamma$gamma.smc[,1]

u_gamma=data.frame(u_gamma,rep("Independent estimation",5000))
colnames(u_gamma)<-c(pop_names[1],"Method")


cu_gamma=readMat('ind_inf_gamma_comp.mat')
cu_gamma=cu_gamma$gamma.smc[,1]

cu_gamma=data.frame(cu_gamma,rep("Independent estimation \n (complete data)",5000))
colnames(cu_gamma)<-c(pop_names[1],"Method")


i_gamma=readMat('hie_inf_pred_gamma.mat')
i_gamma=i_gamma$gamma.smc[,1]

i_gamma=data.frame(i_gamma,rep("Hierarchical estimation",5000))
colnames(i_gamma)=c(pop_names[1],"Method")

ci_gamma=readMat('hie_inf_pred_gamma_comp.mat')
ci_gamma=ci_gamma$gamma.smc[,1]

ci_gamma=data.frame(ci_gamma,rep("Hierarchical estimation \n (complete data)",5000))
colnames(ci_gamma)=c(pop_names[1],"Method")

gammas=rbind(u_gamma,i_gamma,cu_gamma,ci_gamma)
m2=melt(gammas)


u_epsilon=readMat('ind_inf_pred_epsilon.mat')
u_epsilon=u_epsilon$epsilon.smc[,1]

u_epsilon=data.frame(u_epsilon,rep("Independent estmation",5000))
colnames(u_epsilon)<-c(pop_names[1],"Method")

cu_epsilon=readMat('ind_inf_epsilon_comp.mat')
cu_epsilon=cu_epsilon$epsilon.smc[,1]

cu_epsilon=data.frame(cu_epsilon,rep("Independent estmation \n (complete data",5000))
colnames(cu_epsilon)<-c(pop_names[1],"Method")

i_epsilon=readMat('hie_inf_pred_epsilon.mat')
i_epsilon=i_epsilon$epsilon.smc[,1]

i_epsilon=data.frame(i_epsilon,rep("Hierarchical estimation",5000))
colnames(i_epsilon)=c(pop_names[1],"Method")

ci_epsilon=readMat('hie_inf_pred_epsilon_comp.mat')
ci_epsilon=ci_epsilon$epsilon.smc[,1]

ci_epsilon=data.frame(ci_epsilon,rep("Hierarchical estimation",5000))
colnames(ci_epsilon)=c(pop_names[1],"Method")



epsilons=rbind(u_epsilon,i_epsilon,cu_epsilon,ci_epsilon)
m3=melt(epsilons)


t_epsilons=readMat('true_sc2_mus.mat')
t_epsilons=t_epsilons$t.mus
t_epsilons=data.frame(t_epsilons)
colnames(t_epsilons)<-pop_names
mt3=melt(t_epsilons)



pars=data.frame(m$Method,m$value,m2$value,m3$value)
colnames(pars)=c("Method","Transmsion","Recovery","waning immunity")
mda=melt(pars)



p0=ggplot(mda)+
  geom_density(aes(x=value,fill=factor(Method)),alpha=0.8,adjust =0.5)+
  facet_wrap(~factor(variable), scales="free")+
scale_color_brewer(palette = "Dark2")+
theme_classic()

p0



p0=ggpairs(pars, columns = 2:4,
           #upper = list(continuous = "density", combo = "box_no_facet"),
           upper =  "blank",
           #legend=3,
           diag=list(continuous = wrap('densityDiag',alpha=0.7)),
           lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot", alpha = 0.7)),
           ggplot2::aes(colour=Method))  + scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")+
  theme_classic(10) +
  ylab("Parameter space")+
  xlab("Parameter space")+
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
  theme(plot.title = element_text(hjust = 0.5))
p0

p0[1,1]=p0[1,1]+geom_segment(data=NULL,aes(x=2.6355,y=0,xend=2.6355,yend=3),colour = "#3F5151",size=0.55, linetype ="twodash")
p0[2,2]=p0[2,2]+geom_segment(data=NULL,aes(x=1.0169,y=0,xend=1.0169,yend=10),colour = "#3F5151",size=0.55, linetype ="twodash")
p0[3,3]=p0[3,3]+geom_segment(data=NULL,aes(x=0.0642 ,y=0,xend=0.0642,yend=45),colour = "#3F5151",size=0.55, linetype ="twodash")

p0


#sub 17
subs<-sprintf('%d ',16:17)
pop_names=paste("Sub-popuation ",subs)


u_beta=readMat('ind_inf_pred_beta.mat')
u_beta=u_beta$beta.smc[,2]

u_beta=data.frame(u_beta,rep("Independent estimation",5000))
colnames(u_beta)<-c(pop_names[2],"Method")

i_beta=readMat('hie_inf_pred_beta.mat')
i_beta=i_beta$beta.smc[,2]

i_beta=data.frame(i_beta,rep("Hierarchical estimation",5000))
colnames(i_beta)=c(pop_names[2],"Method")

betas=rbind(u_beta,i_beta)
m=melt(betas)

t_betas=readMat('true_sc2_betas.mat')
t_betas=t_betas$t.betas
t_betas=data.frame(t_betas)
colnames(t_betas)<-pop_names
mt1=melt(t_betas)

u_gamma=readMat('ind_inf_pred_gamma.mat')
u_gamma=u_gamma$gamma.smc[,2]

u_gamma=data.frame(u_gamma,rep("Independent estimation",5000))
colnames(u_gamma)<-c(pop_names[2],"Method")

i_gamma=readMat('hie_inf_pred_gamma.mat')
i_gamma=i_gamma$gamma.smc[,2]

i_gamma=data.frame(i_gamma,rep("Hierarchical estimation",5000))
colnames(i_gamma)=c(pop_names[2],"Method")

gammas=rbind(u_gamma,i_gamma)
m2=melt(gammas)


u_epsilon=readMat('ind_inf_pred_epsilon.mat')
u_epsilon=u_epsilon$epsilon.smc[,2]

u_epsilon=data.frame(u_epsilon,rep("Independent estmation",5000))
colnames(u_epsilon)<-c(pop_names[2],"Method")

i_epsilon=readMat('hie_inf_pred_epsilon.mat')
i_epsilon=i_epsilon$epsilon.smc[,2]

i_epsilon=data.frame(i_epsilon,rep("Hierarchical estimation",5000))
colnames(i_epsilon)=c(pop_names[2],"Method")

epsilons=rbind(u_epsilon,i_epsilon)
m3=melt(epsilons)


t_epsilons=readMat('true_sc2_mus.mat')
t_epsilons=t_epsilons$t.mus
t_epsilons=data.frame(t_epsilons)
colnames(t_epsilons)<-pop_names
mt3=melt(t_epsilons)



pars=data.frame(m$Method,m$value,m2$value,m3$value)
colnames(pars)=c("Method","Transmsion","Recovery","waning immunity")


p=ggpairs(pars, columns = 2:4,
          #upper = list(continuous = "density", combo = "box_no_facet"),
          upper =  "blank",
          #legend=3,
          diag=list(continuous = wrap('densityDiag',alpha=0.7)),
          lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot", alpha = 0.7)),
          ggplot2::aes(colour=Method))  + scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")+
  theme_classic(10) +
  ylab("Parameter space")+
  xlab("Parameter space")+
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
  theme(plot.title = element_text(hjust = 0.5))
p

p[1,1]=p[1,1]+geom_segment(data=NULL,aes(x=2.0364,y=0,xend=2.0364,yend=2.5),colour = "#3F5151",size=0.4, linetype ="twodash")
p[2,2]=p[2,2]+geom_segment(data=NULL,aes(x=0.9352,y=0,xend=0.9352,yend=10),colour = "#3F5151",size=0.4, linetype ="twodash")
p[3,3]=p[3,3]+geom_segment(data=NULL,aes(x=0.0443,y=0,xend=0.0443,yend=45),colour = "#3F5151",size=0.4, linetype ="twodash")

p

p2<-plot_grid(
  ggmatrix_gtable(p0),
  ggmatrix_gtable(p),
  ncol = 2
)

library(gridExtra)
p_all<-grid.arrange(pd,arrangeGrob(p2),nrow=2)

ggsave('predictions.png',p_all,width = 10,height = 8)
