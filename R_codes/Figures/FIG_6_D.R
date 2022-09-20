#load data:
#incomplete data:

ind_16=readMat('ext_prob_ind_pop16.mat')
ind_16=ind_16$ext.prob

hie_16=readMat('ext_prob_hie_pop16.mat')
hie_16=hie_16$ext.prob

ind_17=readMat('ext_prob_ind_pop17.mat')
ind_17=ind_17$ext.prob

hie_17=readMat('ext_prob_hie_pop17.mat')
hie_17=hie_17$ext.prob

#complete data 
ind_com_16=readMat('ext_prob_ind_pop16_comp.mat')
ind_com_16=ind_com_16$ext.prob

ind_com_17=readMat('ext_prob_ind_pop17_comp.mat')
ind_com_17=ind_com_17$ext.prob

hie_com_16=readMat('ext_prob_hie_pop16_comp.mat')
hie_com_16=hie_com_16$ext.prob

hie_com_17=readMat('ext_prob_hie_pop17_comp.mat')
hie_com_17=hie_com_17$ext.prob

names<-c("Independent with \n complete data","Hierarchical with \n complete data","Independent with \n partial data","Hierarchical with \n partial data")

pop16=data.frame(t(ind_com_16),t(hie_com_16),t(ind_16),t(hie_16))
colnames(pop16)<-names

pop17=data.frame(t(ind_com_17),t(hie_com_17),t(ind_17),t(hie_17))
colnames(pop17)<-names

m1=melt(pop16)
m2=melt(pop17)

p1=ggplot(m1)+
  geom_density(aes(x=value,fill=factor(variable)),alpha=0.7,adjust =1)+
  geom_segment(data=NULL,aes(x=0.300310 ,y=0,xend=0.300310 ,yend=3.5),colour =   "#3F5151",size=0.4, linetype ="twodash")+
  scale_fill_manual(values= wes_palette("Darjeeling2", n = 4))+
  theme_minimal(12)+
  #ylim(0,35)+
  ylab("Density")+
  xlab("Fade-out probability")+
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
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))
  
p1

p2=ggplot(m2)+
  geom_density(aes(x=value,fill=factor(variable)),alpha=0.7,adjust =1)+
  geom_segment(data=NULL,aes(x=0.719888 ,y=0,xend=0.719888 ,yend=6.5),colour =   "#3F5151",size=0.4, linetype ="twodash")+
  theme_minimal(12)+
  scale_fill_manual(values= wes_palette("Darjeeling2", n = 4))+
  #ylim(0,35)+
  ylab("Density")+
  xlab("Fade-out probability")+
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

p2


legend=get_legend(p2)
library(ggpubr)

prob_plots<-ggarrange(p1, p2, ncol=2, common.legend = TRUE, legend="bottom")



prob2<-annotate_figure(prob_plots ,top = text_grob("Epidemic fade-out probabilities", color = "black", face = "bold", size = 14))   

prob3<-prob2+ theme(plot.margin = margin(1, 1, 0.35, 0.35, "cm"))
prob3


#prob_plots<-plot_grid(p1,p2+theme(legend.position="none"),legend,ncol=2 , rel_heights = c(1, .1))
p_all1<-ggarrange(dt2,post3,align="v",nrow=1,labels = c("(A)","(B)"))
p_all2<-ggarrange(post4,prob3,align="v",nrow=1,labels = c("(C)","(D)"))
p_all<-ggarrange(p_all1,p_all2,align="v",nrow=2)

ggsave('new_data_ext_probs_all.png',height = 10,width = 14,last_plot())

