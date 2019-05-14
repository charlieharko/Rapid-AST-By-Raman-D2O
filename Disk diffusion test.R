library(ggplot2)
library(reshape2)
library(plyr)

bb<-read.csv(file.choose(),header = T)
head(bb)
bb<-melt(bb,id.vars = "Strain",variable.name = "treat")
bb<-ddply(bb,.(Strain,treat),
          summarise, mean = round(mean(value), 3),
          sd = round(sd(value), 3))
bb$treat<-factor(bb$treat,levels=c("Amp","Chl","Kan","Mem"))
bb$Strain<-factor(bb$Strain,levels = c("UTI1","UTI2","UTI3","UTI4","UTI5"))
bb.p<-ggplot(bb,aes(x=treat,y=mean,fill=treat))+
  geom_bar(width = 0.5,stat = "identity")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
                width=0.3,size=0.8)+
  labs(x="Treat",y="Inhibition diameter (mm)")+
  scale_fill_manual(values =c("#7cae00","#00bfc4","#c77cff","grey"))+
  scale_y_continuous(expand = c(0,0),limits = c(0,30.5))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        text = element_text(color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, angle = 90),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.position = "none",
        strip.text = element_text(size = 15),
        panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length=unit(0.4,"lines"))+
  facet_grid(.~Strain)
bb.p
#8.5*2.5

