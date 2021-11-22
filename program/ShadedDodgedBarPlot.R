### Description: this is the code to create dodged barplots by group by visit, with shaded lines added to indicate one of the groups


### Example input data
### Category   grp          prop
#    1        Remdesivir    0.2 
#    2        Placebo       0.3 
#    3        Remdesivir    0.05


library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(stringr)
library(grid)
library(pBrackets) 
library(ggpubr)


p<-ggplot(dta, aes(x=Category, y=prop, fill= grp)) +
  geom_bar(colour = "grey70", width = 0.7, stat="identity",position="dodge") +
  xlab("") + ylab("Proportion") + ylim(c(0,0.5))+
  theme_minimal()+
  theme(legend.title = element_blank(),
        text = element_text(size=15),
        plot.margin = unit(c(1, 1, 3, 1), "lines"))+
  scale_fill_manual(values=rev(c("#00A5E3", "#FF6F68")))


### add shading lines
X.start.all <- 4:7
Y.end.all <- dta$prop[dta$grp=="Remdesivir"]-0.01; Y.end.all <- Y.end.all[4:7]
Y.start <- Y.end <- X.start <- X.end <- NULL

for(i in seq_along(X.start.all)){
  Y.start <- c(Y.start,seq(0, Y.end.all[i],0.01))
  X.start <- c(X.start,rep(X.start.all[i], floor(Y.end.all[i]/0.01)+1))
}
Y.end <- Y.start + 0.01
X.end <- X.start +0.35

segment_data<-data.frame(x=X.start,xend=X.end,y=Y.start,yend=Y.end,grp="Remdesivir")

p <- p+ geom_segment(data=segment_data, aes(x=x, y=y, xend=xend, yend=yend),
                     colour = "grey50")
p

### add brakets
bottom_y <- 365
grid.brackets(226, bottom_y,   106, bottom_y, lwd=1.5, col="grey30")
grid.text("Recovery", x = unit(0.235, "npc"), y = unit(0.1, "npc"))
grid.brackets(550, bottom_y,  500, bottom_y, lwd=1.5, col="grey30")
grid.text("Death", x = unit(0.75, "npc"), y = unit(0.1, "npc"))
