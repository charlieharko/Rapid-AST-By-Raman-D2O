library(ggplot2)
library(reshape2)
####data
y<-read.csv(file.choose(),header = 1)
head(y)

y_long<-melt(data=y,
        id.vars=c("time","concentration","strain"),
        measurevar = c("DH","RE"),
        na.rm = TRUE)
head(y_long)
tail(y_long)
p1<-ggplot(data =y_long,aes(x=time,y=value/(1+value)))+
  geom_smooth(data=y_long, 
              aes(group=variable,color=variable,fill= variable),
              method= "loess",
              level=0.95,span=1)+
  scale_x_continuous(breaks = seq(0,120,30),limits = c(0,120))+
  scale_y_continuous(breaks = seq(0,0.2,0.05),limits = c(0,0.2),
                     labels = scales::percent)+
  scale_colour_manual(values =c("#485B8E","#C32F30"))+
  scale_fill_manual(values =c("#919DBA","#DB8282"))+
  labs(x="Time (min)",y="CD/(CD+CH)")+
  theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        legend.title=element_blank(),
        legend.text = element_text(size = 15),
        legend.position = c(0.85,0.955),
        legend.background = element_blank(),
        text = element_text(color="black"),
        axis.title.y = element_text(size = 20, angle = 90),
        axis.text.x = element_text(size = 15,angle = 0),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 15),
        panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.4,"lines"))+
  facet_grid(strain~concentration)
p1
#10*7
