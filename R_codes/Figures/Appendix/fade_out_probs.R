
#load data:
subs<-sprintf('%d ',1:15)

ind=readMat("ext_prob_ind_inf.mat")
ind=ind$ex.probs
ind=data.frame(t(ind),rep("Independent",5000))


hie=readMat("ext_prob_hie_inf.mat")
hie=hie$ex.probs
hie=data.frame(t(hie),rep("Hierarchical",5000))

colnames(ind)=colnames(hie)=c(subs,"Method")
d=rbind(hie,ind)
m=melt(d)

p=ggplot(m,aes(x=value,y=factor(variable),fill=factor(Method)))+
  geom_density_ridges(scale=1,alpha=0.6)+
  xlim(0,1)+
 # scale_vline_size_continuous(limits = c(0,4))+
  # geom_segment(data =true_betas,mapping=aes(x = tr_be, xend = tr_be, y = Var2,
  #yend = Var2+0.5))+
  scale_fill_manual(values = c( "#D69C4E", "#046C9A", "#8D8680"))+
  # geom_vline(xintercept=2, linetype="dotted")+
  #facet_wrap(~Pop)+
  theme_ridges()+
  theme_clean() +
  ylab("Sub-population")+
  xlab("Epidemic fade-out probability")+
  #ggtitle(expression(paste("Marginal posteriors of ",beta,"")))+
  theme(plot.background=element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.y=element_text(size = 12),
        axis.title.x=element_text(size = 12),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  theme(legend.text=element_text(size=rel(0.75)))+
  theme(legend.title=element_blank(),legend.background = element_blank()) +
  theme(legend.position="bottom") 
p

ggsave("fade_ous.png",last_plot(),height = 12,width = 6)
