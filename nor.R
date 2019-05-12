library("ggplot2","reshape2","plyr")

y.all<-read.csv(file.choose(),header = TRUE)
head(y.all)
y.all<-melt(y.all,measure.vars = c("DH5a","KanR"),variable.name = "strain",na.rm=TRUE)
y.cd.ch<-data.frame(y.all,cd.ch=y.all$value/(1+y.all$value))
head(y.cd.ch)
write.csv(y.cd.ch,file = "normal.csv",row.names = F)
# y<-read.csv(file.choose(),header = 1)
# head(y)
yy.m<-ddply(y.cd.ch,.(time,strain,treat),
            summarise, mean = round(mean(cd.ch), 3),
            sd = round(sd(cd.ch), 3))
head(yy.m)
write.csv(yy.m,file = "mean.csv",row.names = F)
###

y.nor<-read.csv(file.choose(),header = 1)
y.nor$strain<-factor(y.nor$strain,levels = c("DH5a","AmpR"))
head(y.nor)
p1<-ggplot(y.nor,aes(x=time,y=normalize,group=treat))+
  #geom_jitter(aes(color=treat),width = 0.05)+
  geom_smooth(aes(color=treat,fill=treat),
              method = 'loess',span = 1.5,level = 0.99)+
  scale_colour_manual(name = "Treat",values =c("#485B8E","#C32F30"))+
  scale_fill_manual(name = "Treat",values =c("#485B8E","#C32F30"))+
  labs(x="Time (min)",y="Relative CD/(CD+CH)")+
  scale_x_continuous(breaks = seq(0,120,30),limits = c(0,120))+#"#919DBA","#DB8282"
  scale_y_continuous(breaks = seq(0,1.25,0.25))+
  expand_limits(y = c(0,1.25))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        text = element_text(color="black"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15, angle = 90),
        axis.text.x = element_text(size = 12,angle = 0),
        axis.text.y = element_text(size = 12),
        legend.position = c(0.6,0.25),
        legend.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length=unit(0.4,"lines"))+
  facet_grid(.~strain)
p1
