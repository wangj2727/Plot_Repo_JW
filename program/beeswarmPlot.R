### PALM manuscript
### Purpose: generate boxplot for time to 1st neg PCR by arms
### Date: Sep 27th 2019
### Programer: Jing Wang




library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggbeeswarm)
library(ggsignif)
library(pBrackets)
library(beeswarm)

indir<-"H:\\Dr. Dodd Lori\\DRC Ebola\\data\\palmpaper\\"
outdir<-"H:\\Dr. Dodd Lori\\DRC Ebola\\output\\"


dta0<-read.csv(paste0(indir,"palmpaper.csv"),header=TRUE, stringsAsFactors = FALSE)

check1<-dta0%>%
  filter(death==0 & is.na(t2firstneg))%>%
  select(pid, t2firstneg, t2death, death,death28day, t2discharge)

check2<-dta0%>%
  filter(!is.na(t2firstneg) & t2firstneg>35)%>%
  select(pid, t2firstneg, t2death, death,death28day, t2discharge)%>%
  bind_rows(check1)


check3<-dta0%>%
  filter(!is.na(t2firstneg) & t2firstneg>25)


# Notes: 
# one participant receiving ZMapp did not convert to negative prior to day 28 provided a negative ctNP at day 49;
# This subject was removed from the plot
#
# Open trangles indicate patients who were randomizaed during the first phase of the trial
#
# In this set of data, 10 subjects didn't die and their t2firstneg data had not been entered into the database. They 
# were excluded now. 


dta<-dta0%>%
  select(pid, rgroup, t2firstneg, pcr_stratum,rdate, death)%>%
  mutate(FirstNegPCR=ifelse(is.na(t2firstneg) & death==0,NA,
                            ifelse(is.na(t2firstneg) & death==1, 35,t2firstneg)),
         FirstNegPCR=ifelse(FirstNegPCR>35,NA,FirstNegPCR))%>% 
  mutate(rdate=as.Date(rdate, format="%d%B%Y"),
         ConcGroup=ifelse(rdate>as.Date("2019-02-04",format="%Y-%m-%d"),1, 0),
         siteA=ifelse(str_detect(pid,"A"),"A","other"),
         rgroup2=ifelse(ConcGroup==1,19,2))

CC<-c("hotpink","tomato","royalblue","orange","#9ACD32")


for(i in 1:length(dta$FirstNegPCR)){
  if(!is.na(dta$FirstNegPCR[i])&dta$FirstNegPCR[i]==35) dta$FirstNegPCR2[i]<-rnorm(1,43,1.5)
}

medians <-dta%>%
  group_by(rgroup)%>%
  summarise(median=median(FirstNegPCR, na.rm=TRUE))%>%
  select(median)



pdf(paste0(outdir,"Figure2A_DotBoxplot_t2firstnegPCR__",Sys.Date(),".pdf"),width=10, height=8)

set.seed(5566)
par(mar=c(8,7,3,3))
par(xpd=NA)

beeswarm(FirstNegPCR ~ rgroup, data=dta[dta$FirstNegPCR!=35,],
         xlim=c(0.5,4.5),  
         ylim=c(0,48),xlab="",ylab="Days to first negative CtNP               ",
         xaxt = "n",
         yaxt="n",
         main="",
         cex.main=0.9,cex.lab=1.2, col=c("tomato","royalblue","orange","#9ACD32"),
         pch=19,
         pwpch=dta$rgroup2[dta$FirstNegPCR!=35])




beeswarm(FirstNegPCR2 ~ rgroup, data=dta[dta$FirstNegPCR==35,],add=TRUE,method="center",
         xlim=c(0.5,4.5),  
         ylim=c(0,48),xlab="",ylab="",
         xaxt = "n",
         yaxt="n",
         main="",
         cex.main=0.9,cex.lab=0.8, col=c("tomato","royalblue","orange","#9ACD32"),
         pwpch=dta$rgroup2[dta$FirstNegPCR==35])



brackets(0.35,37,0.35,47,h=0.2,type=4,xpd=NA,lwd=2)
text(-0.1,42.2,"Death*",font=2)

axis(side=1,at=c(1,2,3,4),
     labels=c(expression('ZMapp'^"TM"),"Remdesivir","mAb114","REGN-EB3"))
axis(side=2, at=seq(0,30,by=5), labels=seq(0,30,by=5), las=1)


text(0,-12,adj = 0,
     labels = "Solid bars indicate median days. Solid bars in the 'Death' category indicate median days\n not observed because >50% of patients with CtNP values died before conversion to negative. \n Not displayed: one participant receiving ZMapp did not convert to negative prior to day 28 \n provided a negative CtNP at day 49.                                                        \n Open triangles indicate patients who were randomized during the first phase of the trial.  \n *Death imputed as maximum number of days")

#rect(0.75, 42.7, 1.25, 43.3, lwd=2,density=10)
rect(1.75, 42.7, 2.25, 43.3, lwd=2,density=10)
rect(0.75, medians$median[1]-0.2, 1.25, medians$median[1]+0.2, lwd=2, col="grey50",border="grey50")
rect(2.75, medians$median[3]-0.2, 3.25, medians$median[3]+0.2, lwd=2, col="grey50",border="grey50")
rect(3.75, medians$median[4]-0.2, 4.25, medians$median[4]+0.2, lwd=2, col="grey50",border="grey50")


dev.off()



AB<-round(wilcox.test(FirstNegPCR ~ rgroup, data=dta[dta$rgroup %in% c(1,2),])$p.value,4)
AC<-round(wilcox.test(FirstNegPCR ~ rgroup, data=dta[dta$rgroup %in% c(1,3),])$p.value,4)
AD<-round(wilcox.test(FirstNegPCR ~ rgroup, data=dta[dta$rgroup %in% c(1,4) & dta$ConcGroup==1,])$p.value,4)

median_pvalue<-dta%>%
  group_by(rgroup)%>%
  summarise(median=median(FirstNegPCR, na.rm=TRUE))

Comparison<-c(" ","ZMapp vs Remdesivir", "ZMapp vs mAb114","ZMapp concurrent vs REGN-EB3")
Treatment<-c("ZMapp", "Remdesivir", "mAb114", "REGN-EB3")
Pvalue<-c("", AB, AC, AD)
median_Pvalue2<-cbind(Treatment,Comparison, median_pvalue[,2], Pvalue)

write.csv(median_Pvalue2,paste0(outdir, "Fig2 Dot boxplot-median pvalue.csv"),row.names = FALSE,na="")





########################
#######################
#### Without FootNote
#######################
########################

pdf(paste0(outdir,"Figure2A_DotBoxplot_t2firstnegPCR_NoFootNote_",Sys.Date(),".pdf"),width=10, height=8)

set.seed(5566)
par(mar=c(3,7,3,3))
par(xpd=NA)

beeswarm(FirstNegPCR ~ rgroup, data=dta[dta$FirstNegPCR!=35,],
         xlim=c(0.5,4.5),  
         ylim=c(0,48),xlab="",ylab="Days to first negative CtNP               ",
         xaxt = "n",
         yaxt="n",
         main="",
         cex.main=0.9,cex.lab=1.2, col=c("tomato","royalblue","orange","#9ACD32"),
         pch=19,
         pwpch=dta$rgroup2[dta$FirstNegPCR!=35])




beeswarm(FirstNegPCR2 ~ rgroup, data=dta[dta$FirstNegPCR==35,],add=TRUE,method="center",
         xlim=c(0.5,4.5),  
         ylim=c(0,48),xlab="",ylab="",
         xaxt = "n",
         yaxt="n",
         main="",
         cex.main=0.9,cex.lab=0.8, col=c("tomato","royalblue","orange","#9ACD32"),
         pwpch=dta$rgroup2[dta$FirstNegPCR==35])



brackets(0.35,37,0.35,47,h=0.2,type=4,xpd=NA,lwd=2)
text(-0.1,42.2,"Death*",font=2)

axis(side=1,at=c(1,2,3,4),
     labels=c(expression('ZMapp'^"TM"),"Remdesivir","mAb114","REGN-EB3"))
axis(side=2, at=seq(0,30,by=5), labels=seq(0,30,by=5), las=1)

#rect(0.75, 42.7, 1.25, 43.3, lwd=2,density=10)
rect(1.75, 42.7, 2.25, 43.3, lwd=2,density=10)
rect(0.75, medians$median[1]-0.2, 1.25, medians$median[1]+0.2, lwd=2, col="grey50",border="grey50")
rect(2.75, medians$median[3]-0.2, 3.25, medians$median[3]+0.2, lwd=2, col="grey50",border="grey50")
rect(3.75, medians$median[4]-0.2, 4.25, medians$median[4]+0.2, lwd=2, col="grey50",border="grey50")


dev.off()














