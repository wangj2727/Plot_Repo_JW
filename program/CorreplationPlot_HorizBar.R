### Project: PANDORA Study of IRIS
### Programmer: Jing Wang

library(tidyverse)
library(corrplot)
library(pBrackets)



### Example input data
# biomarker SUVmax      SUVMean        TGA TotalVolume p.suvmax p.suvmean  p.TGA  p.vol
#   MPO  0.51635150  0.542602892  0.2662959   0.2436040   0.0039    0.0023 0.1546 0.1939
#  MCP-1 -0.13815350 -0.296551724 -0.1897664  -0.1688543   0.4650    0.1116 0.3138 0.3708
#  IL-6r  0.17107898  0.159065628  0.1372636   0.1581758   0.3645    0.3995 0.4679 0.4022


layout(matrix(1:4,ncol=4))

par(mar=c(5,1,5,1))
plot(NA,ylim=c(0,15),xlim=c(-1.2,1),ylab="",xlab="",yaxt="n",frame.plot = F,main="",xaxt="n")
for(i in seq(0,12,by=2)){
  rect(-1.2,i,1,i+1,col="grey85",border=NA)  
}
segments(0,0,0,14,lwd=2)

for(i in 1:14){text(-1.18,i-0.4,plotdta$biomarker[i],adj=0,cex=1.5)}

for(i in 1:14){
  if(plotdta$SUVmax[i]>=0&plotdta$p.suvmax[i]<0.05){
    rect(0,i-1+0.2,plotdta$SUVmax[i],i-0.2,col="firebrick2",border=NA)
    text(plotdta$SUVmax[i]+0.05,i-0.5,paste0('P=',plotdta$p.suvmax[i]),adj=0,cex=1.5)
  } else if(plotdta$SUVmax[i]>=0&plotdta$p.suvmax[i]>=0.05){
    rect(0,i-1+0.2,plotdta$SUVmax[i],i-0.2,col="black")
  } else if(plotdta$SUVmax[i]<0&plotdta$p.suvmax[i]<0.05){
    rect(plotdta$SUVmax[i],i-1+0.2,0,i-0.2,col="royalblue",border=NA)
  } else if(plotdta$SUVmax[i]<0&plotdta$p.suvmax[i]>=0.05){
    rect(plotdta$SUVmax[i],i-1+0.2,0,i-0.2,col="black")
  }
}
segments(-1.2,14,1,14,lwd=2)
segments(-1.2,15,1,15,lwd=2)
text(-1.16,15-0.4,"Biomarkers",font=2,adj=0,cex=1.5)
text(-0.3,15-0.4,"Spearman correlation ranks",font=2,adj=0,cex=1.5)
mtext("SUV Max",side=3,line=0,font=2,cex=1.2,adj=0.55)
axis(side=1,at=seq(-0.6,0.6,by=0.2),labels=seq(-0.6,0.6,by=0.2))





par(mar=c(5,0,5,0))
plot(NA,ylim=c(0,15),xlim=c(-1.2,1),ylab="",xlab="",yaxt="n",frame.plot = F,main="",xaxt="n")
for(i in seq(0,12,by=2)){
  rect(-1.2,i,1,i+1,col="grey85",border=NA)  
}
segments(0,0,0,14,lwd=2)

for(i in 1:14){text(-1.16,i-0.4,plotdta$biomarker[i],adj=0,cex=1.5)}

for(i in 1:14){
  if(plotdta$SUVMean[i]>=0&plotdta$p.suvmean[i]<0.05){
    rect(0,i-1+0.2,plotdta$SUVMean[i],i-0.2,col="firebrick2",border=NA)
    text(plotdta$SUVMean[i]+0.05,i-0.5,paste0('P=',plotdta$p.suvmean[i]),adj=0,cex=1.5)
  } else if(plotdta$SUVMean[i]>=0&plotdta$p.suvmean[i]>=0.05){
    rect(0,i-1+0.2,plotdta$SUVMean[i],i-0.2,col="black")
  } else if(plotdta$SUVMean[i]<0&plotdta$p.suvmean[i]<0.05){
    rect(plotdta$SUVMean[i],i-1+0.2,0,i-0.2,col="royalblue",border=NA)
  } else if(plotdta$SUVMean[i]<0&plotdta$p.suvmean[i]>=0.05){
    rect(plotdta$SUVMean[i],i-1+0.2,0,i-0.2,col="black")
  }
}
segments(-1.2,14,1,14,lwd=2)
segments(-1.2,15,1,15,lwd=2)
text(-1.16,15-0.4,"Biomarkers",font=2,adj=0,cex=1.5)
text(-0.3,15-0.4,"Spearman correlation ranks",font=2,adj=0,cex=1.5)
mtext("SUV Mean",side=3,line=0,font=2,cex=1.2,adj=0.55)
axis(side=1,at=seq(-0.6,0.6,by=0.2),labels=seq(-0.6,0.6,by=0.2))





par(mar=c(5,1,5,1))
plot(NA,ylim=c(0,15),xlim=c(-1.2,1),ylab="",xlab="",yaxt="n",frame.plot = F,main="",xaxt="n")
for(i in seq(0,12,by=2)){
  rect(-1.2,i,1,i+1,col="grey85",border=NA)  
}
segments(0,0,0,14,lwd=2)

for(i in 1:14){text(-1.16,i-0.4,plotdta$biomarker[i],adj=0,cex=1.5)}

for(i in 1:14){
  if(plotdta$TGA[i]>=0&plotdta$p.TGA[i]<0.05){
    rect(0,i-1+0.2,plotdta$TGA[i],i-0.2,col="firebrick2",border=NA)
    text(plotdta$TGA[i]+0.05,i-0.5,paste0('P=',plotdta$p.TGA[i]),adj=0,cex=1.5)
  } else if(plotdta$TGA[i]>=0&plotdta$p.TGA[i]>=0.05){
    rect(0,i-1+0.2,plotdta$TGA[i],i-0.2,col="black")
  } else if(plotdta$TGA[i]<0&plotdta$p.TGA[i]<0.05){
    rect(plotdta$TGA[i],i-1+0.2,0,i-0.2,col="royalblue",border=NA)
  } else if(plotdta$TGA[i]<0&plotdta$p.TGA[i]>=0.05){
    rect(plotdta$TGA[i],i-1+0.2,0,i-0.2,col="black")
  }
}
segments(-1.2,14,1,14,lwd=2)
segments(-1.2,15,1,15,lwd=2)
text(-1.16,15-0.4,"Biomarkers",font=2,adj=0,cex=1.5)
text(-0.3,15-0.4,"Spearman correlation ranks",font=2,adj=0,cex=1.5)
mtext("TGA",side=3,line=0,font=2,cex=1.2,adj=0.55)
axis(side=1,at=seq(-0.6,0.6,by=0.2),labels=seq(-0.6,0.6,by=0.2))





par(mar=c(5,1,5,1))
plot(NA,ylim=c(0,15),xlim=c(-1.2,1),ylab="",xlab="",yaxt="n",frame.plot = F,main="",xaxt="n")
for(i in seq(0,12,by=2)){
  rect(-1.2,i,1,i+1,col="grey85",border=NA)  
}
segments(0,0,0,14,lwd=2)

for(i in 1:14){text(-1.16,i-0.4,plotdta$biomarker[i],adj=0,cex=1.5)}

for(i in 1:14){
  if(plotdta$TotalVolume[i]>=0&plotdta$p.vol[i]<0.05){
    rect(0,i-1+0.2,plotdta$TotalVolume[i],i-0.2,col="firebrick2",border=NA)
    text(plotdta$TotalVolume[i]+0.05,i-0.5,paste0('P=',plotdta$p.vol[i]),adj=0,cex=1.5)
  } else if(plotdta$TotalVolume[i]>=0&plotdta$p.vol[i]>=0.05){
    rect(0,i-1+0.2,plotdta$TotalVolume[i],i-0.2,col="black")
  } else if(plotdta$TotalVolume[i]<0&plotdta$p.vol[i]<0.05){
    rect(plotdta$TotalVolume[i],i-1+0.2,0,i-0.2,col="royalblue",border=NA)
  } else if(plotdta$TotalVolume[i]<0&plotdta$p.vol[i]>=0.05){
    rect(plotdta$TotalVolume[i],i-1+0.2,0,i-0.2,col="black")
  }
}
segments(-1.2,14,1,14,lwd=2)
segments(-1.2,15,1,15,lwd=2)
text(-1.16,15-0.4,"Biomarkers",font=2,adj=0,cex=1.5)
text(-0.3,15-0.4,"Spearman correlation ranks",font=2,adj=0,cex=1.5)
mtext("Total Volume",side=3,line=0,font=2,cex=1.2,adj=0.55)
axis(side=1,at=seq(-0.6,0.6,by=0.2),labels=seq(-0.6,0.6,by=0.2))






