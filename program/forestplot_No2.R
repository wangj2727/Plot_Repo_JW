
### Description: code to create forestplot using ggplot2 and grid packages

library(ggplot2)
library(grid)


### Example Input Dataset:
 #                             Characteristic          OR   LCL    UCL    p.value
 # Location (Provincial ARC vs. Regional ARC)        0.148 0.06   1.99   0.001
 #                                        Age        1.055 1.04   7.98   0.001
 #                      Sex (Male vs. Female)        1.103 0.51   29.18   0.800


ggplot(DataPlot,aes(x=(OR),y=Characteristic))+geom_point()+
  geom_errorbarh(aes(xmin=(LCL),xmax=(UCL)),size=1,height=0.15)+
  theme_bw()+
  scale_x_continuous(trans="log",breaks=c(0.02,0.1,1,10,50),labels = c(0.02,0.1,1,10,50))+
  geom_vline(xintercept=1,linetype="dotted",size=0.75)+geom_hline(yintercept = 49.5)+
  labs(y="",x="Odds Ratio")+
  theme(legend.position = "none",plot.margin = margin(2,3,1,0.5,"cm"),axis.text=element_text(size=11),
        axis.title.x = element_text(size=12,face="bold"))
grid.text(y=unit(.95,"npc"),x=unit(0.55,"npc"),label="Some Labels",gp=gpar(cex=0.8,fontface="bold"))  





