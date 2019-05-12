x<-read.csv(file = file.choose(),header = TRUE)
x_long<-melt(x,id.vars="strain",na.rm= TRUE)
head(x_long)
x_cd_ratio<-data.frame(x_long,cd_ratio=x_long$value/(1+x_long$value))
write.csv(x_cd_ratio,file = "C:/Users/yk/Desktop/normalize/Isolate new criteria/x_cd_ratio.csv",row.names = F)
x_mean<-ddply(x_cd_ratio,.(strain,variable),
            summarise, mean = round(mean(cd_ratio), 3),
            sd = round(sd(cd_ratio), 3))
write.csv(x_mean,file = "C:/Users/yk/Desktop/normalize/Isolate new criteria/x_mean.csv",row.names = F)

x_cd_ratio_p<-read.csv(file = file.choose(),header = TRUE)
x_cd_ratio_p$strain<-factor(x_cd_ratio_p$strain,levels = c("E. coli25922","Proteus","Salmonella","Shigella"))
x_cd_ratio_p$variable<-factor(x_cd_ratio_p$variable,levels = c("control","Amp","Chl","Kan","Mem"))
x_p<-ggplot(x_cd_ratio_p,aes(x=variable,y=nor,color=variable))+
  geom_boxplot(width = 0.7, outlier.color = NA)+
  geom_jitter(aes(color=variable),width = 0.3,alpha=0.7)+
  geom_hline(yintercept = c(0.76,0.78),color = "red", linetype = 2)+
  stat_summary(fun.y = mean, geom = 'point', fill = 'white', shape = 21, size = 1.5) +
  geom_signif(size = 0.6,textsize = 3,
              test = "wilcox.test",
              comparisons = list(c("control","Amp"),
                                 c("control","Chl"),
                                 c("control","Kan"),
                                 c("control","Mem")),
              map_signif_level=TRUE,
              tip_length = 0,
              y_position = (c(1.2,1.25,1.3,1.35,1.4)),
              vjust= 0.5, color = "black")+
  labs(x="Treat",y="Relative CD/(CD+CH)")+
  scale_x_discrete(breaks=c("control","Amp","Chl","Kan","Mem"),
                   labels=c("Con","Amp","Chl","Kan","Mem"))+
  scale_y_continuous(breaks = seq(0,1.5,0.25))+
  scale_colour_manual(values =c("#f8766d","#7cae00","#00bfc4","#c77cff","black"))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        text = element_text(color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, angle = 90),
        axis.text.x = element_text(color="black",size = 13),
        axis.text.y = element_text(color="black",size = 13),
        legend.position = "none",
        strip.text = element_text(size = 15),
        panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length=unit(0.4,"lines"))+
  facet_grid(.~strain,scales = "free_x",space = "free_x")
x_p

x<-read.table(file.choose())
summary(x)

