


params=c("Transmission rate","Recovery rate","Waning immunity rate")

#load data:
inf_hb=readMat('inf_hb_all.mat')
inf_hb=inf_hb$hb
inf_hb=t(inf_hb[,5001:10000])

inf_h=data.frame(inf_hb,rep('Informatve prior',1000))
colnames(inf_h)=c(params,"Method")


unif_hb=readMat('unif_hb_all.mat')
unif_hb=unif_hb$hb
unif_hb=t(unif_hb[,5001:10000])

unif_h=data.frame(unif_hb,rep('Uninformatve prior',1000))
colnames(unif_h)=c(params,"Method")


m2=melt(inf_h)
m1=melt(unif_h)

m=rbind(m1,m2)

#priors
x_b<- seq(0, 11, length=1000)
y_b <- dunif(x_b, min = 0.001, max = 10) #uniform denisty for all  hyper_betas

x_g=seq(0, 4, length=1000)
y_g= dunif(x_g, min =0.00001, max = 3) #uniform denisty for all  hyper_gammas

x_e_u=seq(0, 0.25, length=1000)
y_g_u= dunif(x_e_u, min =0, max = 0.2) #uniform denisty for epsilon 1

y_g_i= dunif(x_e_u, min =0.01, max = 0.2) #uniform denisty for epsilon 2

tr_v=data.frame("variable"=params,"value"=c(2.5,1,0.06))
me_v=data.frame("variable"=params,"value"=c(2.5235, 1.0198,0.0606))


levels(m$Method)<-c("Uninformative prior","Informative prior")

p1=ggplot(m)+
  geom_histogram(aes(x=value,y=..density..,fill=as.factor(Method)),alpha=0.8,position="identity")+
 # geom_vline(data=tr_v, aes(xintercept =value,colour =  "#D69C4E" ),size=0.4, linetype ="twodash")+
  geom_segment(data=tr_v,aes(x=value,y=0,xend=value,yend=c(10,15,80),colour =  "#D69C4E"),size=0.4, linetype ="twodash")+
  geom_segment(data=me_v,aes(x=value,y=0,xend=value,yend=c(10,15,80),colour =  "#08519C"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c( "#D69C4E","#08519C"),
                      labels = c("Mean of the group","True parameter value"))+
  scale_fill_manual(values =c("#1B9E77", "#7570B3"),labels=c("Informative prior","Less infomative prior") )+
  facet_grid(~factor(variable),scales="free")+
  theme_classic(10) +
  ylab("Density")+
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
  theme(plot.title = element_text(hjust = 0.5))
p1

ggsave('hyper_means.png',last_plot(),dpi=1000)



#load data:
inf_hb_sig=readMat('inf_hb_sig_all.mat')
inf_hb_sig=inf_hb_sig$hb.sig
inf_hb_sig=t(inf_hb_sig[,5001:10000])

inf_sig=data.frame(inf_hb_sig,rep('Informatve prior',1000))
colnames(inf_sig)=c(params,"Method")


unif_hb_sig=readMat('unif_hb_sig_all.mat')
unif_hb_sig=unif_hb_sig$hb.sig
unif_hb_sig=t(unif_hb_sig[,5001:10000])

unif_sig=data.frame(unif_hb_sig,rep('Uninformatve prior',1000))
colnames(unif_sig)=c(params,"Method")


m2=melt(inf_sig)
m1=melt(unif_sig)

m=rbind(m1,m2)

tr_v=data.frame("variable"=params,"value"=c(.25,0.05,0.01))
me_v=data.frame("variable"=params,"value"=c(0.3476, 0.0498,0.0073))


p2=ggplot(m)+
  geom_histogram(aes(x=value,y=..density..,fill=as.factor(Method)),binwidth = 0.0025,alpha=0.8,position="identity")+
  # geom_vline(data=tr_v, aes(xintercept =value,colour =  "#D69C4E" ),size=0.4, linetype ="twodash")+
  geom_segment(data=tr_v,aes(x=value,y=0,xend=value,yend=c(10,15,80),colour =  "#D69C4E"),size=0.4, linetype ="twodash")+
  geom_segment(data=me_v,aes(x=value,y=0,xend=value,yend=c(10,25,80),colour =  "#08519C"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c( "#D69C4E","#08519C"),
                      labels = c("Standard deviation of the group","True parameter value"))+
  scale_fill_manual(values =c("#1B9E77", "#7570B3"),labels=c("Informative prior","Less infomative prior") )+
  facet_grid(~factor(variable),scales="free")+
  theme_classic(10) +
  ylab("Density")+
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
  theme(plot.title = element_text(hjust = 0.5))
p2

ggsave('hyper_sigs.png',last_plot(),dpi=1000)

plot_grid(p1,p2,ncol=1,labels = c("A","B"))
ggsave('hyper_all.png',last_plot(),height= 9,width = 7)
