

subs<-sprintf('%d ',16:17)
pop_names=paste("Sub-population ",subs)
#pop_names=factor(subs)


data=readMat('sc2_data.mat')
data=data$data
data=data.frame(1:15,data[1:15,1])
#data=data.frame("Day"=1:15,data)

data2=readMat('sc2_data.mat')
data2=data2$data
data2=data.frame(15:35,data2[15:35,1])


colnames(data)=c("Day","Sub-population 16")

m=melt(data,id="Day")

m=data.frame(m$Day,m$value)
colnames(m)=c("Day","value")
colnames(data2)=c("Day","Sub-population 16")

m2=melt(data2,id="Day")
m2=data.frame(m2$Day,m2$value)
colnames(m2)=c("Day","value")
#m2$Var1=15:35


pd1<-ggplot(m,aes(x=Day,y=value))+
  geom_line(colour = "black")+
  geom_line(m2, mapping=aes(x=Day,y=value), colour =  "#666666",linetype ="twodash")+
  scale_color_manual(values = c("black", "#666666"),labels=c("Observed path","Actual path upto 35 days"))+
  #facet_wrap(~as.factor(Var2),ncol=2)+
  theme_minimal(12) +
  ylab("Prevalance")+
  xlab("Day")+
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
pd1


subs<-sprintf('%d ',16:17)
pop_names=paste("Sub-population ",subs)
#pop_names=factor(subs)


data=readMat('sc2_data.mat')
data=data$data
data=data.frame(1:15,data[1:15,2])
#data=data.frame("Day"=1:15,data)

data2=readMat('sc2_data.mat')
data2=data2$data
data2=data.frame(15:35,data2[15:35,2])


colnames(data)=c("Day","Sub-population 17")

m=melt(data,id="Day")

m=data.frame(m$Day,m$value)
colnames(m)=c("Day","value")
colnames(data2)=c("Day","Sub-population 17")

m2=melt(data2,id="Day")
m2=data.frame(m2$Day,m2$value)
colnames(m2)=c("Day","value")
#m2$Var1=15:35


pd2<-ggplot(m,aes(x=Day,y=value))+
  geom_line(colour = "black")+
  geom_line(m2, mapping=aes(x=Day,y=value), colour =  "#666666",linetype ="twodash")+
  scale_color_manual(values = c("black", "#666666"),labels=c("Observed path","Actual path upto 35 days"))+
  #facet_wrap(~as.factor(Var2),ncol=2)+
  theme_minimal(12) +
  ylab("Prevalance")+
  xlab("Day")+
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
pd2

pd=ggarrange(pd1,pd2)
pd

dt<-annotate_figure(pd ,top = text_grob("Data", color = "black", face = "bold", size = 14))   

dt2<-dt + theme(plot.margin = margin(1,1, 0.35, 0.35, "cm"))
dt2

#ggsave('sc1_ data.png',last_plot())

