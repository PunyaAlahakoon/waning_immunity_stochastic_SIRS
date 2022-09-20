
params=c("Transmission rate","Recovery rate","Waning immunity rate")

#load data:
inf_hb=readMat('inf_hb_all.mat')
inf_hb=inf_hb$hb
inf_hb=t(inf_hb[,5001:10000])

inf_h=data.frame(inf_hb,rep('Informatve prior',5000))
colnames(inf_h)=c(params,"Method")


unif_hb=readMat('unif_hb_all.mat')
unif_hb=unif_hb$hb
unif_hb=t(unif_hb[,5001:10000])

unif_h=data.frame(unif_hb,rep('Uninformatve prior',5000))
colnames(unif_h)=c(params,"Method")

tr_v=data.frame("variable"=params,"value"=c(2.5,1,0.06))
me_v=data.frame("variable"=params,"value"=c(2.5235, 1.0198,0.0606))



#load data:
inf_hb_sig=readMat('inf_hb_sig_all.mat')
inf_hb_sig=inf_hb_sig$hb.sig
inf_hb_sig=t(inf_hb_sig[,5001:10000])

inf_h_sig=data.frame(inf_hb_sig,rep('Informatve prior',5000))
colnames(inf_h_sig)=c(params,"Method")


unif_hb_sig=readMat('unif_hb_sig_all.mat')
unif_hb_sig=unif_hb_sig$hb.sig
unif_hb_sig=t(unif_hb_sig[,5001:10000])

unif_h_sig=data.frame(unif_hb_sig,rep('Uninformatve prior',5000))
colnames(unif_h_sig)=c(params,"Method")

tr_v_sig=data.frame("variable"=params,"value"=c(.25,0.05,0.01))
me_v_sig=data.frame("variable"=params,"value"=c(0.3476, 0.0498,0.0073))


g1=data.frame(unif_hb[,1],inf_hb[,1])
colnames(g1)<-c("Less infomative prior","Strong informative prior")
g1<-melt(g1)


p1<-ggplot(g1)+
  #geom_histogram(aes(x=value,y=..density..,fill=as.factor(variable)),binwidth = 0.0095,alpha=0.8,position="identity")+
  geom_density(aes(x=value,y=..density..,fill=as.factor(variable)),alpha=0.8,position="identity")+
  scale_fill_manual(values =c("#66A61E", "#7570B3"))+
  geom_segment(data=NULL,aes(x=2.5,y=0,xend=2.5,yend=5,colour =  "#D69C4E"),size=0.4, linetype ="twodash")+
  geom_segment(data=NULL,aes(x=2.5235,y=0,xend=2.5235,yend=5,colour =  "#08519C"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c( "#D69C4E","#08519C"),
                      labels = c("Mean of the group","True parameter value"))+
  theme_classic(10) +
  xlim(1.5,3.5)+
  xlab(expression(Psi[beta]))+
  ylab("Denisity")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        # axis.title.y=element_blank(),
        strip.text.x = element_text(),
        strip.text.y = element_blank())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="none") 

p1

sc1=data.frame(inf_hb[,1],inf_hb_sig[,1])

p2<-ggplot(sc1,aes(x=sc1[,2],y=sc1[,1]))+
  geom_point(color="#7570B3")+
  ylim(1.5,3.5)+
  xlim(0,1.5)+
  ylab(expression(Psi[beta]))+
  xlab(expression(sigma[beta]))+
  theme_classic()
p2

g2=data.frame(unif_hb_sig[,1],inf_hb_sig[,1])
colnames(g2)<-c("Less infomative prior","Strong informative prior")
g2<-melt(g2)

p3<-ggplot(g2)+
 # geom_histogram(aes(x=value,y=..density..,fill=as.factor(variable)),binwidth = 0.00915,alpha=0.8,position="identity")+
  geom_density(aes(x=value,y=..density..,fill=as.factor(variable)),alpha=0.8,position="identity")+
  scale_fill_manual(values =c("#66A61E", "#7570B3"))+
  geom_segment(data=NULL,aes(x=0.5,y=0,xend=0.5,yend=5,colour =  "#D69C4E"),size=0.4, linetype ="twodash")+
  geom_segment(data=NULL,aes(x=0.3476,y=0,xend=0.3476,yend=5,colour =  "#08519C"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c( "#D69C4E","#08519C"),
                      labels = c("Mean of the group","True parameter value"))+
  theme_classic(10) +
  xlim(0,1.5)+
  xlab(expression(sigma[beta]))+
  ylab("Denisity")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        # axis.title.y=element_blank(),
        strip.text.x = element_text(),
        strip.text.y = element_blank())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="none") 

p3


#A<-plot_grid(p2,p1,p3,NULL,align = 'v')+ theme(plot.margin = margin(0.35, 0.35, 0.35, 0.35, "cm"))+
  #theme(plot.background = element_rect(color = "black"))
A<-plot_grid(p2,p1,p3,NULL,align = 'v')+ theme(plot.margin = margin(0.35, 0.35, 0.35, 0.35, "cm"))
A

###gamma 
g1=data.frame(unif_hb[,2],inf_hb[,2])
colnames(g1)<-c("Less infomative prior","Strong informative prior")
g1<-melt(g1)


p1<-ggplot(g1)+
  geom_density(aes(x=value,y=..density..,fill=as.factor(variable)),alpha=0.8,position="identity")+
 # geom_histogram(aes(x=value,y=..density..,fill=as.factor(variable)),binwidth = 0.0055,alpha=0.8,position="identity")+
  scale_fill_manual(values =c("#66A61E", "#7570B3"))+
  geom_segment(data=NULL,aes(x=1,y=0,xend=1,yend=15,colour =  "#D69C4E"),size=0.4, linetype ="twodash")+
  geom_segment(data=NULL,aes(x=1.0198,y=0,xend=1.0198,yend=15,colour =  "#08519C"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c( "#D69C4E","#08519C"),
                      labels = c("Mean of the group","True parameter value"))+
  theme_classic(10) +
  xlim(0.7,1.5)+
  xlab(expression(Psi[gamma]))+
  ylab("Denisity")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        # axis.title.y=element_blank(),
        strip.text.x = element_text(),
        strip.text.y = element_blank())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="none")

p1

sc1=data.frame(inf_hb[,2],inf_hb_sig[,2])

p2<-ggplot(sc1,aes(x=sc1[,2],y=sc1[,1]))+
  geom_point(color="#7570B3")+
 ylim(0.7,1.5)+
  xlim(0,0.4)+
  ylab(expression(Psi[gamma]))+
  xlab(expression(sigma[gamma]))+
  theme_classic()
p2

g2=data.frame(unif_hb_sig[,2],inf_hb_sig[,2])
colnames(g2)<-c("Less infomative prior","Strong informative prior")
g2<-melt(g2)

p3<-ggplot(g2)+
  geom_density(aes(x=value,y=..density..,fill=as.factor(variable)),alpha=0.8,position="identity")+
 # geom_histogram(aes(x=value,y=..density..,fill=as.factor(variable)),binwidth = 0.00515,alpha=0.8,position="identity")+
  scale_fill_manual(values =c("#66A61E", "#7570B3"))+
  geom_segment(data=NULL,aes(x=0.05,y=0,xend=0.05,yend=15,colour =  "#D69C4E"),size=0.4, linetype ="twodash")+
  geom_segment(data=NULL,aes(x=0.0498,y=0,xend=0.0498,yend=15,colour =  "#08519C"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c( "#D69C4E","#08519C"),
                      labels = c("Mean of the group","True parameter value"))+
  theme_classic(10) +
  xlim(0,0.4)+
  xlab(expression(sigma[gamma]))+
  ylab("Denisity")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        # axis.title.y=element_blank(),
        strip.text.x = element_text(),
        strip.text.y = element_blank())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="none") 

p3


B<-plot_grid(p2,p1,p3,NULL,align='v')+ theme(plot.margin = margin(0.35, 0.35, 0.35, 0.35, "cm"))
#+
#  theme(plot.background = element_rect(color = "black"))
B

###MU
g1=data.frame(unif_hb[,3],inf_hb[,3])
colnames(g1)<-c("Less infomative prior","Strong informative prior")
g1<-melt(g1)


p1<-ggplot(g1)+
  geom_density(aes(x=value,y=..density..,fill=as.factor(variable)),alpha=0.8,position="identity")+
 # geom_histogram(aes(x=value,y=..density..,fill=as.factor(variable)),binwidth = 0.00095,alpha=0.8,position="identity")+
  scale_fill_manual(values =c("#66A61E", "#7570B3"))+
  geom_segment(data=NULL,aes(x=0.06,y=0,xend=0.06,yend=85,colour =  "#D69C4E"),size=0.4, linetype ="twodash")+
  geom_segment(data=NULL,aes(x=0.0606,y=0,xend=0.0606,yend=85,colour =  "#08519C"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c( "#D69C4E","#08519C"),
                      labels = c("Mean of the group","True parameter value"))+
  theme_classic(10) +
  xlim(0,0.1)+
  xlab(expression(Psi[mu]))+
  ylab("Denisity")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        # axis.title.y=element_blank(),
        strip.text.x = element_text(),
        strip.text.y = element_blank())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="none") 

p1

sc1=data.frame(inf_hb[,3],inf_hb_sig[,3])

p2<-ggplot(sc1,aes(x=sc1[,2],y=sc1[,1]))+
  geom_point(color="#7570B3")+
  ylim(0,0.1)+
  xlim(0,0.15)+
  ylab(expression(Psi[mu]))+
  xlab(expression(sigma[mu]))+
  theme_classic()
p2

g2=data.frame(unif_hb_sig[,3],inf_hb_sig[,3])
colnames(g2)<-c("Less infomative prior","Strong informative prior")
g2<-melt(g2)

p3<-ggplot(g2)+
  geom_density(aes(x=value,y=..density..,fill=as.factor(variable)),alpha=0.8,position="identity")+
 # geom_histogram(aes(x=value,y=..density..,fill=as.factor(variable)),binwidth = 0.000915,alpha=0.8,position="identity")+
  scale_fill_manual(values =c("#66A61E", "#7570B3"))+
  geom_segment(data=NULL,aes(x=0.01,y=0,xend=0.01,yend=75,colour =  "#D69C4E"),size=0.4, linetype ="twodash")+
  geom_segment(data=NULL,aes(x=0.0073,y=0,xend=0.0073,yend=75,colour =  "#08519C"),size=0.4, linetype ="twodash")+
  scale_colour_manual(values=c( "#D69C4E","#08519C"),
                      labels = c("Mean of the group","True parameter value"))+
  theme_classic(10) +
  xlim(0,0.15)+
  xlab(expression(sigma[mu]))+
  ylab("Denisity")+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        # axis.title.y=element_blank(),
        strip.text.x = element_text(),
        strip.text.y = element_blank())+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="none") 

p3


C<-plot_grid(p2,p1,p3,NULL,align = "v")+ theme(plot.margin = margin(0.35, 0.35, 0.35, 0.35, "cm"))
#+
 # theme(plot.background = element_rect(color = "black"))
C

plot_grid(A,B,C,NULL,ncol=2,labels =c( "A","B","C"))

top_r=plot_grid(A,B,ncol=2,labels = c("A","B"))
top_r
bot_r=plot_grid(NULL,C,NULL,ncol = 3, rel_widths=c(0.25,0.5,0.25),labels =c( "","C",""))
bot_r

plot_grid(top_r,bot_r,ncol=1, rel_heights=c(1,1,1))

ggsave('hyper_params.png',width = 8,height=6)
