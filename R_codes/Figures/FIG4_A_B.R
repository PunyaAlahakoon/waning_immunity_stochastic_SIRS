#load data:

subs<-sprintf('%d ',1:15)
#pop_names=paste("Sub-population ",subs)
#pop_names=c(sprintf('%d ',1:3), "4*", sprintf('%d ',5:7),
 #           "8*", "9","10*","11*","12*",
  #          "13","14*","15*")
pop_names=subs
u_beta=readMat('ind_inf_beta.mat')
u_beta=u_beta$beta.smc

u_beta=data.frame(u_beta,rep("Independent estimation",5000))
colnames(u_beta)<-c(pop_names,"Method")

i_beta=readMat('hie_inf_beta.mat')
i_beta=i_beta$beta.smc

i_beta=data.frame(i_beta,rep("Hierarchical estimation",5000))
colnames(i_beta)=c(pop_names,"Method")

betas=rbind(i_beta,u_beta)
m=melt(betas)
lvs=c("Independent estimation","Hierarchical estimation")

m$Method=factor(m$Method,levels=lvs)

t_betas=readMat('true_betas.mat')
t_betas=t_betas$t.betas
t_betas=data.frame(t_betas)
colnames(t_betas)<-pop_names
m2=melt(t_betas)
m2=data.frame(m2,"Method"=rep(NA,15))


p1=ggplot(m,aes(x=value,y=factor(variable),fill=factor(Method)))+
            geom_density_ridges(scale=1,alpha=0.8,color = NA)+
            #geom_density_ridges(stat = "binline", bins = 250,scale=1, draw_baseline = FALSE,alpha=0.8,color = NA)+
            scale_vline_size_continuous(limits = c(0,4))+
            geom_segment(data =m2,mapping=aes(x =value, xend = value, y = as.numeric(variable),yend =as.numeric(variable)+0.85),size=0.4, linetype ="twodash")+
            scale_fill_manual(values = c("#046C9A", "#D69C4E", "#8D8680"))+
             #geom_vline(data =m2,aes(xintercept=value), linetype="dotted")+
            #facet_wrap(~Pop)+
            theme_ridges()+
            xlim(0,5.5)+
            theme_minimal(base_size = 14) +
            ylab("Sub-population")+
            xlab("Transmission rate")+
            #ggtitle(expression(paste("Marginal posteriors of ",beta,"")))+
            theme(plot.background=element_blank(),
                  strip.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line = element_line(colour = "black"),
                  axis.title.y=element_text(size = 14),
                  axis.title.x=element_text(size = 14),
                  strip.text.x = element_text(size = 14),
                  strip.text.y = element_text(size = 14))+
            theme(legend.text=element_text(size=rel(1)))+
            theme(legend.title=element_blank(),legend.background = element_blank()) +
            theme(legend.position="bottom") 
p1

ggsave("hie_ind_inf_betas.png",height =6,width = 12,last_plot())



#subs<-sprintf('%d ',1:15)
#pop_names=paste("Sub-population ",subs)
#pop_names=c(sprintf('%d ',1:3), "4*", sprintf('%d ',5:7),
           # "8*", "9","10*","11*","12*",
           # "13","14*","15*")


u_epsilon=readMat('ind_inf_epsilon.mat')
u_epsilon=u_epsilon$epsilon.smc


u_epsilon=data.frame(u_epsilon,rep("Independent estimation",5000))
colnames(u_epsilon)<-c(pop_names,"Method")

i_epsilon=readMat('hie_inf_epsilon.mat')
i_epsilon=i_epsilon$epsilon.smc

i_epsilon=data.frame(i_epsilon,rep("Hierarchical estimation",5000))
colnames(i_epsilon)=c(pop_names,"Method")

epsilons=rbind(i_epsilon,u_epsilon)
m=melt(epsilons)
lvs=c("Independent estimation","Hierarchical estimation")

m$Method=factor(m$Method,levels=lvs)

t_epsilons=readMat('true_mus.mat')
t_epsilons=t_epsilons$t.mus
t_epsilons=data.frame(t_epsilons)
colnames(t_epsilons)<-pop_names
m2=melt(t_epsilons)
m2=data.frame(m2,"Method"=rep(NA,15))

p3=ggplot(m,aes(x=value,y=factor(variable),fill=factor(Method)))+
  geom_density_ridges(scale=1,alpha=0.8,color = NA)+
  #geom_density_ridges(stat = "binline", bins = 250,scale=1, draw_baseline = FALSE,alpha=0.8,color = NA)+
  scale_vline_size_continuous(limits = c(0,4))+
  geom_segment(data =m2,mapping=aes(x =value, xend = value, y = as.numeric(variable),yend =as.numeric(variable)+0.85),size=0.4, linetype ="twodash")+
  scale_fill_manual(values = c("#046C9A", "#D69C4E", "#8D8680"))+
  #geom_vline(data =m2,aes(xintercept=value), linetype="dotted")+
  #facet_wrap(~Pop)+
  theme_ridges()+
  xlim(0,0.2)+
  theme_minimal(base_size = 14) +
  ylab("Sub-population")+
  xlab("Waning immunity rate")+
  #ggtitle(expression(paste("Marginal posteriors of ",beta,"")))+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.y=element_text(size = 14),
        axis.title.x=element_text(size = 14),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") 
p3


rid<-ggpubr::ggarrange(p3,p1, common.legend = TRUE,legend = "bottom",labels =c ("(A)","(B)"))
rid


