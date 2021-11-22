### Description: this is the code to create shifted barplot of 
###              Day 15 outcomes by baseline ordinal scale by treatment arms
###              (plots and legend are created separately, which will needed to be further combined in other software)


library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(readxl)
library(scales)
library(RColorBrewer)

### Example input data
# BaseScore Category Remdesivir Placebo
#  4        1         xx        xx
#  4        2         xx        xx
#  4        3         xx        xx
#  4        4         xx        xx
#  4        5         xx        xx
#  4        6         xx        xx
#  4        7         xx        xx
#  4        8         xx        xx
#  5        1         xx        xx
#  ...
#  7        7         xx        xx
#  7        8         xx        xx


dta_total <- dta0%>%
  group_by(BaseScore)%>%
  summarise(Rem_prop= sum(Remdesivir),
            Placebo_prop = sum(Placebo))
dta <- dta0%>%
  group_by(BaseScore)%>%
  mutate(Rem_prop= Remdesivir/sum(Remdesivir),
         Placebo_prop = Placebo/sum(Placebo))


### set up common parameters
NumCat <- 8   ### number of category
Ybottom <- (-2)
Yupper <- (-4)
MyCol <- brewer.pal(n = NumCat, name = "RdBu")



par(mar=c(3,1,1,0))

#############################
##
## Base Cat=4
##
############################
Score <-4

dtaPlot <- dta%>%
  filter(BaseScore==Score)%>%
  arrange(desc(Category))%>%
  mutate(Rem_cum = cumsum(Rem_prop), Placebo_cum = cumsum(Placebo_prop))

Placebo.X.left <- c(0,dtaPlot$Placebo_cum)
Placebo.X.right <- c(dtaPlot$Placebo_cum,1)

Rem.X.left <- c(0,dtaPlot$Rem_cum)
Rem.X.right <- c(dtaPlot$Rem_cum,1)

Shift <- sum(dtaPlot$Placebo_prop[1:(Score+1)]) - sum(dtaPlot$Rem_prop[1:(Score+1)])

dtaPlot$Placebo_pct <- sprintf("%1.1f%%", 100*dtaPlot$Placebo_prop)
dtaPlot$Placebo_pct <- ifelse(dtaPlot$Placebo_pct=="0.0%","", dtaPlot$Placebo_pct)

dtaPlot$Rem_pct <- sprintf("%1.1f%%", 100*dtaPlot$Rem_prop)
dtaPlot$Rem_pct <- ifelse(dtaPlot$Rem_pct=="0.0%","", dtaPlot$Rem_pct)



plot(NA, xlim=c(-0.1, 1.7), ylim=c(-5, 5), xlab="", ylab="", main="", axes=FALSE)

for(i in 1:NumCat){
  rect(Placebo.X.left[i],Ybottom,Placebo.X.right[i],Yupper, col=MyCol[i])
  rect(Rem.X.left[i] + Shift,abs(Ybottom),Rem.X.right[i] + Shift,abs(Yupper), col=MyCol[i])
  
  if(i==1){
    text(x = Placebo.X.left[i] + (Placebo.X.right[i]-Placebo.X.left[i])/2, 
         y = Yupper, 
         srt = 45, xpd = TRUE,adj = 1.2,
         labels =dtaPlot$Placebo_pct[i] , cex = 0.65)
    
    text(x = Rem.X.left[i] + (Rem.X.right[i]- Rem.X.left[i])/2 +Shift-0.04, 
         y = abs(Yupper), 
         srt = 45, xpd = TRUE,adj = -0.3,
         labels =dtaPlot$Rem_pct[i] , cex = 0.65)
  }else{
    text(x = Placebo.X.left[i] + (Placebo.X.right[i]-Placebo.X.left[i])/2 +0.01, 
         y = Yupper, 
         srt = 45, xpd = TRUE,adj = 1.2,
         labels =dtaPlot$Placebo_pct[i] , cex = 0.65)
    
    text(x = Rem.X.left[i] + (Rem.X.right[i]- Rem.X.left[i])/2 +Shift-0.015, 
         y = abs(Yupper), 
         srt = 45, xpd = TRUE,adj = -0.3,
         labels =dtaPlot$Rem_pct[i] , cex = 0.65)
  }
  
}

text(1.2, -3, "Placebo",  adj = 0.7, pos=4, cex=0.9)
text(1.2, -4, paste0("(N=",dta_total$Placebo_prop[dta_total$BaseScore==Score],")"),  adj = 0.7, pos=4, cex=0.9)
text(1.2, 3, "Remdesivir",  adj = 0.7, pos=4, cex=0.9)
text(1.2, 2, paste0("(N=",dta_total$Rem_prop[dta_total$BaseScore==Score],")"),  adj = 0.7, pos=4, cex=0.9)

segments(Placebo.X.left[Score+2], Ybottom, Placebo.X.left[Score+2], abs(Ybottom), lty=1, col="grey50")
rect(Placebo.X.left[Score+2]-0.09, -1.4, Placebo.X.left[Score+2], 1.4, col=MyCol[8-4+1],border="grey50")
text(Placebo.X.left[Score+2]-0.06, 0, srt=90, labels="Enrollment", cex=0.7)
text(Placebo.X.left[Score+2]-0.03, 0, srt=90, labels="Score", cex=0.7)

### arrows
arrows(x0 = Placebo.X.left[Score+2], y0= (-5.8), x1=Placebo.X.left[Score+2]+0.35, y1=(-5.8),
       length = 0.1, angle = 30, xpd = TRUE,col="#2166AC", cex=1.5, lwd=2)
arrows(x0 = Placebo.X.left[Score+2], y0= (-5.8), x1=Placebo.X.left[Score+2]-0.35, y1=(-5.8),
       length = 0.1, angle = 30, xpd = TRUE,col="#B2182B", cex=1.5, lwd=2)

text(Placebo.X.left[Score+2]+0.13, -6.5, "Improvement", xpd=TRUE, cex=0.7)
text(Placebo.X.left[Score+2]-0.18, -6.5, "Worse/No Improvement", xpd=TRUE, cex=0.7)

mtext(paste0("Score=",Score), side = 3, adj = 0.05, line = 0,cex=0.8)





#############################
##
## Base Cat=5
##
############################
Score <-5

dtaPlot <- dta%>%
  filter(BaseScore==Score)%>%
  arrange(desc(Category))%>%
  mutate(Rem_cum = cumsum(Rem_prop), Placebo_cum = cumsum(Placebo_prop))

Placebo.X.left <- c(0,dtaPlot$Placebo_cum)
Placebo.X.right <- c(dtaPlot$Placebo_cum,1)

Rem.X.left <- c(0,dtaPlot$Rem_cum)
Rem.X.right <- c(dtaPlot$Rem_cum,1)

Shift <- sum(dtaPlot$Placebo_prop[1:(Score-1)]) - sum(dtaPlot$Rem_prop[1:(Score-1)])

dtaPlot$Placebo_pct <- sprintf("%1.1f%%", 100*dtaPlot$Placebo_prop)
dtaPlot$Placebo_pct <- ifelse(dtaPlot$Placebo_pct=="0.0%","", dtaPlot$Placebo_pct)

dtaPlot$Rem_pct <- sprintf("%1.1f%%", 100*dtaPlot$Rem_prop)
dtaPlot$Rem_pct <- ifelse(dtaPlot$Rem_pct=="0.0%","", dtaPlot$Rem_pct)



plot(NA, xlim=c(-0.1, 1.7), ylim=c(-5, 5), xlab="", ylab="", main="", axes=FALSE)

for(i in 1:NumCat){
  rect(Placebo.X.left[i],Ybottom,Placebo.X.right[i],Yupper, col=MyCol[i])
  rect(Rem.X.left[i] + Shift,abs(Ybottom),Rem.X.right[i] + Shift,abs(Yupper), col=MyCol[i])
  
  if(i==1){
    text(x = Placebo.X.left[i] + (Placebo.X.right[i]-Placebo.X.left[i])/2, 
         y = Yupper, 
         srt = 45, xpd = TRUE,adj = 1.2,
         labels =dtaPlot$Placebo_pct[i] , cex = 0.65)
    
    text(x = Rem.X.left[i] + (Rem.X.right[i]- Rem.X.left[i])/2 +Shift-0.04, 
         y = abs(Yupper), 
         srt = 45, xpd = TRUE,adj = -0.3,
         labels =dtaPlot$Rem_pct[i] , cex = 0.65)
  }else{
    text(x = Placebo.X.left[i] + (Placebo.X.right[i]-Placebo.X.left[i])/2 +0.01, 
         y = Yupper, 
         srt = 45, xpd = TRUE,adj = 1.2,
         labels =dtaPlot$Placebo_pct[i] , cex = 0.65)
    
    text(x = Rem.X.left[i] + (Rem.X.right[i]- Rem.X.left[i])/2 +Shift-0.015, 
         y = abs(Yupper), 
         srt = 45, xpd = TRUE,adj = -0.3,
         labels =dtaPlot$Rem_pct[i] , cex = 0.65)
  }
  
}

text(1.2, -3, "Placebo",  adj = 0.7, pos=4, cex=0.9)
text(1.2, -4, paste0("(N=",dta_total$Placebo_prop[dta_total$BaseScore==Score],")"),  adj = 0.7, pos=4, cex=0.9)
text(1.2, 3, "Remdesivir",  adj = 0.7, pos=4, cex=0.9)
text(1.2, 2, paste0("(N=",dta_total$Rem_prop[dta_total$BaseScore==Score],")"),  adj = 0.7, pos=4, cex=0.9)

segments(Placebo.X.left[Score], Ybottom, Placebo.X.left[Score], abs(Ybottom), lty=1, col="grey50")
rect(Placebo.X.left[Score]-0.09, -1.4, Placebo.X.left[Score], 1.4, col=MyCol[8-5+1],border="grey50")
text(Placebo.X.left[Score]-0.06, 0, srt=90, labels="Enrollment", cex=0.7)
text(Placebo.X.left[Score]-0.03, 0, srt=90, labels="Score", cex=0.7)

### arrows
arrows(x0 = Placebo.X.left[Score], y0= (-5.8), x1=Placebo.X.left[Score]+0.35, y1=(-5.8),
       length = 0.1, angle = 30, xpd = TRUE,col="#2166AC", cex=1.5, lwd=2)
arrows(x0 = Placebo.X.left[Score], y0= (-5.8), x1=Placebo.X.left[Score]-0.35, y1=(-5.8),
       length = 0.1, angle = 30, xpd = TRUE,col="#B2182B", cex=1.5, lwd=2)

text(Placebo.X.left[Score]+0.13, -6.5, "Improvement", xpd=TRUE, cex=0.7)
text(Placebo.X.left[Score]-0.18, -6.5, "Worse/No Improvement", xpd=TRUE, cex=0.7)

mtext(paste0("Score=",Score), side = 3, adj = 0.05, line = 0,cex=0.8)




#############################
##
## Base Cat=6
##
############################
Score <-6

dtaPlot <- dta%>%
  filter(BaseScore==Score)%>%
  arrange(desc(Category))%>%
  mutate(Rem_cum = cumsum(Rem_prop), Placebo_cum = cumsum(Placebo_prop))

Placebo.X.left <- c(0,dtaPlot$Placebo_cum)
Placebo.X.right <- c(dtaPlot$Placebo_cum,1)

Rem.X.left <- c(0,dtaPlot$Rem_cum)
Rem.X.right <- c(dtaPlot$Rem_cum,1)

Shift <- sum(dtaPlot$Placebo_prop[1:(Score-3)]) - sum(dtaPlot$Rem_prop[1:(Score-3)])

dtaPlot$Placebo_pct <- sprintf("%1.1f%%", 100*dtaPlot$Placebo_prop)
dtaPlot$Placebo_pct <- ifelse(dtaPlot$Placebo_pct=="0.0%","", dtaPlot$Placebo_pct)

dtaPlot$Rem_pct <- sprintf("%1.1f%%", 100*dtaPlot$Rem_prop)
dtaPlot$Rem_pct <- ifelse(dtaPlot$Rem_pct=="0.0%","", dtaPlot$Rem_pct)



plot(NA, xlim=c(-0.1, 1.7), ylim=c(-5, 5), xlab="", ylab="", main="", axes=FALSE)

for(i in 1:NumCat){
  rect(Placebo.X.left[i],Ybottom,Placebo.X.right[i],Yupper, col=MyCol[i])
  rect(Rem.X.left[i] + Shift,abs(Ybottom),Rem.X.right[i] + Shift,abs(Yupper), col=MyCol[i])
  
  if(i==1){
    text(x = Placebo.X.left[i] + (Placebo.X.right[i]-Placebo.X.left[i])/2, 
         y = Yupper, 
         srt = 45, xpd = TRUE,adj = 1.2,
         labels =dtaPlot$Placebo_pct[i] , cex = 0.65)
    
    text(x = Rem.X.left[i] + (Rem.X.right[i]- Rem.X.left[i])/2 +Shift-0.04, 
         y = abs(Yupper), 
         srt = 45, xpd = TRUE,adj = -0.3,
         labels =dtaPlot$Rem_pct[i] , cex = 0.65)
  }else{
    text(x = Placebo.X.left[i] + (Placebo.X.right[i]-Placebo.X.left[i])/2 +0.01, 
         y = Yupper, 
         srt = 45, xpd = TRUE,adj = 1.2,
         labels =dtaPlot$Placebo_pct[i] , cex = 0.65)
    
    text(x = Rem.X.left[i] + (Rem.X.right[i]- Rem.X.left[i])/2 +Shift-0.015, 
         y = abs(Yupper), 
         srt = 45, xpd = TRUE,adj = -0.3,
         labels =dtaPlot$Rem_pct[i] , cex = 0.65)
  }
  
}

text(1.2, -3, "Placebo",  adj = 0.7, pos=4, cex=0.9)
text(1.2, -4, paste0("(N=",dta_total$Placebo_prop[dta_total$BaseScore==Score],")"),  adj = 0.7, pos=4, cex=0.9)
text(1.2, 3, "Remdesivir",  adj = 0.7, pos=4, cex=0.9)
text(1.2, 2, paste0("(N=",dta_total$Rem_prop[dta_total$BaseScore==Score],")"),  adj = 0.7, pos=4, cex=0.9)

segments(Placebo.X.left[Score-2], Ybottom, Placebo.X.left[Score-2], abs(Ybottom), lty=1, col="grey50")
rect(Placebo.X.left[Score-2]-0.09, -1.4, Placebo.X.left[Score-2], 1.4, col=MyCol[8-6+1],border="grey50")
text(Placebo.X.left[Score-2]-0.06, 0, srt=90, labels="Enrollment", cex=0.7)
text(Placebo.X.left[Score-2]-0.03, 0, srt=90, labels="Score", cex=0.7)

### arrows
arrows(x0 = Placebo.X.left[Score-2], y0= (-5.8), x1=Placebo.X.left[Score-2]+0.35, y1=(-5.8),
       length = 0.1, angle = 30, xpd = TRUE,col="#2166AC", cex=1.5, lwd=2)
arrows(x0 = Placebo.X.left[Score-2], y0= (-5.8), x1=Placebo.X.left[Score-2]-0.35, y1=(-5.8),
       length = 0.1, angle = 30, xpd = TRUE,col="#B2182B", cex=1.5, lwd=2)

text(Placebo.X.left[Score-2]+0.13, -6.5, "Improvement", xpd=TRUE, cex=0.7)
text(Placebo.X.left[Score-2]-0.18, -6.5, "Worse/No Improvement", xpd=TRUE, cex=0.7)

mtext(paste0("Score=",Score), side = 3, adj = 0.05, line = 0,cex=0.8)





#############################
##
## Base Cat=7
##
############################
Score <-7

dtaPlot <- dta%>%
  filter(BaseScore==Score)%>%
  arrange(desc(Category))%>%
  mutate(Rem_cum = cumsum(Rem_prop), Placebo_cum = cumsum(Placebo_prop))

Placebo.X.left <- c(0,dtaPlot$Placebo_cum)
Placebo.X.right <- c(dtaPlot$Placebo_cum,1)

Rem.X.left <- c(0,dtaPlot$Rem_cum)
Rem.X.right <- c(dtaPlot$Rem_cum,1)

Shift <- sum(dtaPlot$Placebo_prop[1:(Score-5)]) - sum(dtaPlot$Rem_prop[1:(Score-5)])

dtaPlot$Placebo_pct <- sprintf("%1.1f%%", 100*dtaPlot$Placebo_prop)
dtaPlot$Placebo_pct <- ifelse(dtaPlot$Placebo_pct=="0.0%","", dtaPlot$Placebo_pct)

dtaPlot$Rem_pct <- sprintf("%1.1f%%", 100*dtaPlot$Rem_prop)
dtaPlot$Rem_pct <- ifelse(dtaPlot$Rem_pct=="0.0%","", dtaPlot$Rem_pct)



plot(NA, xlim=c(-0.1, 1.7), ylim=c(-5, 5), xlab="", ylab="", main="", axes=FALSE)

for(i in 1:NumCat){
  rect(Placebo.X.left[i],Ybottom,Placebo.X.right[i],Yupper, col=MyCol[i])
  rect(Rem.X.left[i] + Shift,abs(Ybottom),Rem.X.right[i] + Shift,abs(Yupper), col=MyCol[i])
  
  if(i==1){
    text(x = Placebo.X.left[i] + (Placebo.X.right[i]-Placebo.X.left[i])/2, 
         y = Yupper, 
         srt = 45, xpd = TRUE,adj = 1.2,
         labels =dtaPlot$Placebo_pct[i] , cex = 0.65)
    
    text(x = Rem.X.left[i] + (Rem.X.right[i]- Rem.X.left[i])/2 +Shift-0.04, 
         y = abs(Yupper), 
         srt = 45, xpd = TRUE,adj = -0.3,
         labels =dtaPlot$Rem_pct[i] , cex = 0.65)
  }else{
    text(x = Placebo.X.left[i] + (Placebo.X.right[i]-Placebo.X.left[i])/2 +0.01, 
         y = Yupper, 
         srt = 45, xpd = TRUE,adj = 1.2,
         labels =dtaPlot$Placebo_pct[i] , cex = 0.65)
    
    text(x = Rem.X.left[i] + (Rem.X.right[i]- Rem.X.left[i])/2 +Shift-0.015, 
         y = abs(Yupper), 
         srt = 45, xpd = TRUE,adj = -0.3,
         labels =dtaPlot$Rem_pct[i] , cex = 0.65)
  }
  
}

text(1.2, -3, "Placebo",  adj = 0.7, pos=4, cex=0.9)
text(1.2, -4, paste0("(N=",dta_total$Placebo_prop[dta_total$BaseScore==Score],")"),  adj = 0.7, pos=4, cex=0.9)
text(1.2, 3, "Remdesivir",  adj = 0.7, pos=4, cex=0.9)
text(1.2, 2, paste0("(N=",dta_total$Rem_prop[dta_total$BaseScore==Score],")"),  adj = 0.7, pos=4, cex=0.9)

segments(Placebo.X.left[Score-4], Ybottom, Placebo.X.left[Score-4], abs(Ybottom), lty=1, col="grey50")
rect(Placebo.X.left[Score-4]-0.09, -1.4, Placebo.X.left[Score-4], 1.4, col=MyCol[8-7+1],border="grey50")
text(Placebo.X.left[Score-4]-0.06, 0, srt=90, labels="Enrollment", cex=0.7)
text(Placebo.X.left[Score-4]-0.03, 0, srt=90, labels="Score", cex=0.7)

### arrows
arrows(x0 = Placebo.X.left[Score-4], y0= (-5.8), x1=Placebo.X.left[Score-4]+0.35, y1=(-5.8),
       length = 0.1, angle = 30, xpd = TRUE,col="#2166AC", cex=1.5, lwd=2)
arrows(x0 = Placebo.X.left[Score-4], y0= (-5.8), x1=Placebo.X.left[Score-4]-0.35, y1=(-5.8),
       length = 0.1, angle = 30, xpd = TRUE,col="#B2182B", cex=1.5, lwd=2)

text(Placebo.X.left[Score-4]+0.13, -6.5, "Improvement", xpd=TRUE, cex=0.7)
text(Placebo.X.left[Score-4]-0.18, -6.5, "Worse/No Improvement", xpd=TRUE, cex=0.7)


mtext(paste0("Score=",Score), side = 3, adj = 0.05, line = 0,cex=0.8)






####################
##
## legend plot
##
####################

y.bottom <- 1:8
y.up <- y.bottom+0.7
x.left <- 0
x.right <-0.5

legend.labels <- c("Death", 7:2, "Not hosp, no limitations ")


plot(NA, xlim=c(0, 10), ylim=c(0,10), xlab="", ylab="", main="", axes=FALSE)

for(i in 1:8){
  rect(x.left, y.bottom[i], x.right, y.up[i], col=MyCol[i])
  text(1, y.bottom[i]+0.35, labels = legend.labels[i], adj = 0)
}








