### Project: IRIS multi-country main cohort of 560 participants
### Purpose: create forest plot of Odds Ratio from logistic regression of biomarkers in prediction of the risk of developing IRIS. 
### Programmer: Jing Wang

library(survival)
library(MASS)
library(plyr)
library(gplots)
library(XLConnect)
library(shape)


### Example input data
# Predictor    OddsRatioEst  LowerCL   UpperCL  M_OddsRatioEst M_LowerCL M_UpperCL      Uni.ORCL       Multi.ORCL      Uni.pvalue Multi.pvalue     bio
# sCD14__pg_ml    12.360046 2.870981 53.212037       3.351614 0.6987777 16.075663 12.36(2.87,53.21) 3.35(0.70,16.08)          1            0   sCD14
# TNF____pg_ml     3.691164 1.464735  9.301814       2.465859 0.9604907  6.330580   3.69(1.46,9.30)  2.47(0.96,6.33)          1            0     TNF
# HA__pg_ml_L     3.393021 1.849144  6.225902       2.359726 1.2352424  4.507864   3.39(1.85,6.23)  2.36(1.24,4.51)           1            1      HA

all<-readWorksheet(loadWorkbook("H:\\Rebecca\\IRIS\\output\\table_figure\\OddRatio_for_Forestplot_02AUG16.xlsx"),
                   sheet="Overall506")
all<-all[order(-all$OddsRatioEst),]
all$Uni.ORCL<-paste(sprintf('%.2f',round(all$OddsRatioEst,2)),"(",sprintf('%.2f',round(all$LowerCL,2)),",",sprintf('%.2f',round(all$UpperCL,2)),")",sep="")
all$Multi.ORCL<-paste(sprintf('%.2f',round(all$M_OddsRatioEst,2)),"(",sprintf('%.2f',round(all$M_LowerCL,2)),",",
                      sprintf('%.2f',round(all$M_UpperCL,2)),")",sep="")
all$Uni.pvalue<-ifelse(all$LowerCL>1,1,0)
all$Multi.pvalue<-ifelse(all$M_LowerCL>1,1,0)
all$bio<-sapply(strsplit(all$Predictor,"_"),function(x){if(x[2]=="") {x[1]
} else{paste(x[1],x[2],sep="-")}})

par(xpd=NA)
par(mfrow=c(1,1))
par(mar=c(3,1,3,1))
plot(x=NULL,y=NULL,xlim=c(0.02,2500),ylim=c(0,22),log="x",xaxt="n",yaxt="n",ylab="",xlab="",axes=F,main="")

segments(0.1,-1,0.1,21,lty=3,lwd=1)
segments(10,-1,10,21,lty=3,lwd=1)
segments(1,-1,1,21,lty=1,lwd=0.6)
segments(0.016,21,3500,21,lty=1)


text(200,22.5,"Univariate ORs",cex=1)
text(200,21.5," (95% CI)",cex=1)
text(1500,22.5,"Adjusted* ORs",cex=1)
text(1500,21.5," (95% CI)",cex=1)
text(0.03,22.5,"Biomarkers",cex=1)
text(0.03,21.5," (pg/ml)",cex=1)

points(200,23.2,pch=15,col="royalblue")
points(1500,23.2,pch=15,col="indianred1")
segments(135,23.2,325,23.2,lty=3,lwd=2,col="royalblue")
segments(1000,23.2,2200,23.2,lty=1,lwd=2,col="indianred1")

axis(side=1,at=c(0.1,1,10,100),labels=c(0.1,1,10,100),cex.axis=1)

for(i in 1:19){text(0.02,i-0.2,labels=all$bio[i],cex=1.2,adj=0)
  text(65,i,labels=all$Uni.ORCL[i],cex=1.1,adj=0)
  text(650,i,labels=all$Multi.ORCL[i],cex=1.1,adj=0)
  
  points(all$OddsRatioEst[i],i,pch=15,col="royalblue")
  points(all$M_OddsRatioEst[i],i-0.3,pch=15,col="indianred1")
  
  segments(all$LowerCL[i],i,all$UpperCL[i],i,col="royalblue",lwd=2,lty=3)
  segments(all$M_LowerCL[i],i-0.3,all$M_UpperCL[i],i-0.3,col="indianred1",lwd=2)
}                                              

for(i in 20:20){text(0.02,i-0.2,labels=all$bio[i],cex=1.2,adj=0)
  text(65,i,labels=all$Uni.ORCL[i],cex=1.1,adj=0)
  text(650,i,labels=all$Multi.ORCL[i],cex=1.1,adj=0)
  Arrows(0.09,i,all$M_UpperCL[i],i,arr.type="simple",arr.col="royalblue",code=1,lty=3,lwd=2,col="royalblue")
  Arrows(0.09,i,0.1,i,arr.type="simple",arr.col="royalblue",code=1,lty=1,lwd=2,col="royalblue")
  Arrows(0.09,i-0.3,all$M_UpperCL[i],i-0.3,arr.type="simple",arr.col="indianred1",
         code=1,lty=1,lwd=2,col="indianred1")
  
}
