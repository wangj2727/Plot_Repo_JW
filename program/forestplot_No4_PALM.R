### PALM forest plot
### Purpose: generate forest plot
### Date: Oct 7th 2019
### Programer: Jing Wang
# 1.	Change Age grouping from (<5, 5-<18, >=18) to (<=5, 5-<18 exclusive of 5, >=18). 
# 2.	Change mAb114 CT<=22 death/total from (51/74) to (51/73); change mAB114 CT>22 death/total from (10/100) to (10/101);


library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(exact2x2)

indir<-"H:\\Dr. Dodd Lori\\DRC Ebola\\output\\"

exact_CI_function<-function(x0, n0, x1, n1,confylevel=0.95){
  
  estimate<-round((x1/n1-x0/n0)*100,0)
  confy<-uncondExact2x2(x0, n0, x1, n1, parmtype = "difference", 
                        nullparm = NULL, alternative = "two.sided", conf.int = TRUE, conf.level = confylevel, 
                        method = "FisherAdj", tsmethod = "central", midp = FALSE, gamma = 0, 
                        EplusM=FALSE, tiebreak=FALSE, plotprobs = FALSE, control=ucControl(), 
                        Tfunc=NULL)
  outCI<-paste0(estimate," (",round(confy$conf.int[1]*100,0)," to ",round(confy$conf.int[2]*100,0),")")
  outPvalue<-round(confy$p.value,4)
  return(c(outCI, outPvalue))
}



fisher_exact_function<-function(x0, n0, x1, n1){
  
  input_matrix<-matrix(c(x1, n1-x1, x0, n0-x0), byrow=TRUE, 2,2)
  fish.out<-fisher.exact(input_matrix, tsmethod="central")
  #estimate_OR<-round(as.numeric(fish.out$estimate),3) ### Note that the conditional Maximum Likelihood Estimate (MLE) rather than the unconditional MLE (the sample odds ratio) is used
  estimate_OR<-round(as.numeric((x1*(n0-x0))/(x0*(n1-x1))),3)
  P_value<-round(as.numeric(fish.out$p.value),4)
  OR_LCL<-round(as.numeric(fish.out$conf.int)[1],3)
  OR_UCL<-round(as.numeric(fish.out$conf.int)[2],3)
  return(c(estimate_OR, P_value, OR_LCL, OR_UCL))
}

## check results
# fish.out<-fisher.exact(matrix(c(7,12-7,8,19-8), byrow = TRUE,2,2, dimnames=list(c("Zmapp","Remdesivir"), c("death","alive"))),
#                        tsmethod="central")


raw<-read_excel(paste0(indir,"PALM_ForestPlot_MortalityRate_Data_Template.xlsx"),sheet="28DaysFUraw", na=" ")

dta0<-raw%>%
  filter(!is.na(Var))%>%
  rowwise()%>%
  mutate(AB_CI=exact_CI_function(A_death, A_total, B_death, B_total)[1],
         AC_CI=exact_CI_function(A_death, A_total, C_death, C_total)[1],
         AD_CI=exact_CI_function(A_death_conc, A_total_conc, D_death, D_total)[1],
         `Group A`=paste0(A_death,"/",A_total),
         `Group B`=paste0(B_death,"/",B_total),
         `Group C`=paste0(C_death,"/",C_total),
         `Group D`=paste0(D_death,"/",D_total),
         AB_pvalue=exact_CI_function(A_death, A_total, B_death, B_total)[2],
         AC_pvalue=exact_CI_function(A_death, A_total, C_death, C_total)[2],
         AD_conc_pvalue=exact_CI_function(A_death_conc, A_total_conc, D_death, D_total)[2])%>%
  mutate(AB_OR=fisher_exact_function(A_death, A_total, B_death, B_total)[1],
         AC_OR=fisher_exact_function(A_death, A_total, C_death, C_total)[1],
         AD_OR=fisher_exact_function(A_death_conc, A_total_conc, D_death, D_total)[1],
         AB_OR_pvalue=fisher_exact_function(A_death, A_total, B_death, B_total)[2],
         AC_OR_pvalue=fisher_exact_function(A_death, A_total, C_death, C_total)[2],
         AD_OR_conc_pvalue=fisher_exact_function(A_death_conc, A_total_conc, D_death, D_total)[2],
         AB_OR_LCL=fisher_exact_function(A_death, A_total, B_death, B_total)[3],
         AC_OR_LCL=fisher_exact_function(A_death, A_total, C_death, C_total)[3],
         AD_OR_LCL_conc=fisher_exact_function(A_death_conc, A_total_conc, D_death, D_total)[3],
         AB_OR_UCL=fisher_exact_function(A_death, A_total, B_death, B_total)[4],
         AC_OR_UCL=fisher_exact_function(A_death, A_total, C_death, C_total)[4],
         AD_OR_UCL_conc=fisher_exact_function(A_death_conc, A_total_conc, D_death, D_total)[4])



dta<-raw%>%
  select(Var)%>%
  left_join(dta0, by="Var")%>%
  mutate(AB=as.numeric(str_extract(AB_CI,"[:punct:]?\\d+")),
         AB_LCL=as.numeric(gsub(" to","",str_extract(AB_CI,"[:punct:]?\\d+[:space:]to"))),
         AB_UCL=as.numeric(gsub("to ","",str_extract(AB_CI,"to[:space:][:punct:]?\\d+"))),
         AC=as.numeric(str_extract(AC_CI,"[:punct:]?\\d+")),
         AC_LCL=as.numeric(gsub(" to","",str_extract(AC_CI,"[:punct:]?\\d+[:space:]to"))),
         AC_UCL=as.numeric(gsub("to ","",str_extract(AC_CI,"to[:space:][:punct:]?\\d+"))),
         AD=as.numeric(str_extract(AD_CI,"[:punct:]?\\d+")),
         AD_LCL=as.numeric(gsub(" to","",str_extract(AD_CI,"[:punct:]?\\d+[:space:]to"))),
         AD_UCL=as.numeric(gsub("to ","",str_extract(AD_CI,"to[:space:][:punct:]?\\d+"))))


attach(dta)


## object anchor position
varlistAB<-c(37,34,33,30,27,24,23,20,17,16,13,10,9,6,3,0)+1
varlistAC<-varlistAB-1
varlistAD<-varlistAB-2

varlistAB_rect<-c(37,33,30,27,23,20,16,13,9,6,3,0)+1
varlistAC_rect<-varlistAB_rect-1
varlistAD_rect<-varlistAB_rect-2

varlistAB_CI<-varlistAB[!is.na(AB)]
varlistAC_CI<-varlistAC[!is.na(AC)]
varlistAD_CI<-varlistAD[!is.na(AD)]

Var[1]<-NA



####################################################################
##
## Absolute Diff in Mortality and OR plot, No estimate text
##
#####################################################################

pdf(paste0(indir,"PALM_DiffinMortality_And_OR_",Sys.Date(),".pdf"),width=14, height=10)

par(mar=c(7,1,1,1))

plot(x=NULL,
     y=NULL,
     xlim=c(-700, 680),  
     ylim=c(0,40),xlab="",ylab="",
     xaxt = "n",
     yaxt="n",
     main="",
     cex.main=0.9,cex.lab=0.8, frame.plot = FALSE)

axis(side=1, at=c(-50,0,50)*2-50, labels=c(-50,0,50))
segments(-50,-1.5,-50,38.2, lty=2, lwd=2)


### move abs diff to the left by 50

for(i in 1:nrow(dta)){
  
  points(AB[i]*2-50, varlistAB[i], pch=17, col="royalblue", cex=1.5)
  segments(AB_LCL[i]*2-50, varlistAB[i], AB_UCL[i]*2-50, varlistAB[i], col="royalblue", lwd=2)
  
  points(AC[i]*2-50, varlistAC[i], pch=19, col="orange", cex=1.5)
  segments(AC_LCL[i]*2-50, varlistAC[i], AC_UCL[i]*2-50, varlistAC[i], col="orange", lwd=2)
  
  points(AD[i]*2-50, varlistAD[i], pch=15, col="#9ACD32", cex=1.5)
  segments(AD_LCL[i]*2-50, varlistAD[i], AD_UCL[i]*2-50, varlistAD[i], col="#9ACD32", lwd=2)
  
  rect(-600, varlistAD_rect[i]-0.35, 750, varlistAB_rect[i]+0.35, col="#B3B3B330",border=NA)
  
  text(-550, varlistAC[i], `Group A`[i])
  text(-450, varlistAC[i], `Group B`[i])
  text(-350, varlistAC[i], `Group C`[i])
  text(-250, varlistAC[i], `Group D`[i])
  
  text(-700, varlistAC[i], Var[i], adj=0)

}


# for(i in 1:nrow(dta)){
#   points(AB_OR[i]*100+100, varlistAB[i], pch=17, col="royalblue", cex=1.5)
#   segments(AB_OR_LCL[i]*100+100, varlistAB[i], AB_OR_UCL[i]*100+100, varlistAB[i], col="royalblue", lwd=2)
#   
#   points(AC_OR[i]*100+100, varlistAC[i], pch=19, col="orange", cex=1.5)
#   segments(AC_OR_LCL[i]*100+100, varlistAC[i], AC_OR_UCL[i]*100+100, varlistAC[i], col="orange", lwd=2)
#   
#   points(AD_OR[i]*100+100, varlistAD[i], pch=15, col="#9ACD32", cex=1.5)
#   segments(AD_OR_LCL_conc[i]*100+100, varlistAD[i], AD_OR_UCL_conc[i]*100+100, varlistAD[i], col="#9ACD32", lwd=2)
# 
# }


for(i in 1:nrow(dta)){
  points(log(AB_OR[i])*100+500, varlistAB[i], pch=17, col="royalblue", cex=1.5)
  segments(log(AB_OR_LCL[i])*100+500, varlistAB[i], log(AB_OR_UCL[i])*100+500, varlistAB[i], col="royalblue", lwd=2)
  
  points(log(AC_OR[i])*100+500, varlistAC[i], pch=19, col="orange", cex=1.5)
  segments(log(AC_OR_LCL[i])*100+500, varlistAC[i], log(AC_OR_UCL[i])*100+500, varlistAC[i], col="orange", lwd=2)
  
  points(log(AD_OR[i])*100+500, varlistAD[i], pch=15, col="#9ACD32", cex=1.5)
  segments(log(AD_OR_LCL_conc[i])*100+500, varlistAD[i], log(AD_OR_UCL_conc[i])*100+500, varlistAD[i], col="#9ACD32", lwd=2)
  
}

axis(side=1, at=log(c(1/10,1/5,1/2,1,2,5,10))*100+500, labels=as.character(c(1/10,1/5,1/2,1,2,5,10)))
segments(500,-1.5,500,38.2, lty=2, lwd=2)



text(-670, 41, " ", font=2, cex=0.9)
text(-550, 41, "Zmapp", font=2, cex=0.9)
text(-450, 41, "Remdesivir", font=2, cex=0.9)
text(-350, 41, "mAb114", font=2, cex=0.9)
text(-250, 41, "REGN-EB3", font=2, cex=0.9)
text(0, 41, "Absolute Difference in Mortality", font=2, cex=0.9)

text(-400, 39, "no. of patients who died/total no. of patients", cex=0.8)
text(0, 39, "percentage points (95% CI)", cex=0.8)

text(500, 41, "Odds Ratio", font=2, cex=0.9)
text(500, 39, "(95% CI)", cex=0.8)


text(-740,37,"Overall",font=2,cex=1,adj=0)
text(-740,35,"Age",font=2,cex=1,adj=0)
text(-740,25,"Sex",font=2,cex=1,adj=0)
text(-740,18,"CT",font=2,cex=1,adj=0)
text(-740,11,"Site",font=2,cex=1,adj=0)

### legend
par(xpd=TRUE)
legend(-180, -6, legend=c("Remdesivir", "mAb114", "REGN-EB3"),pch=c(17,19,15),
       lty=1, lwd=2, col=c("royalblue","orange","#9ACD32"), horiz=TRUE, bty="n",pt.cex=1.5)

### Add arrows
arrows(-40,-4.5, 20, -4.5, lwd=1, length=0.13, angle=20)
arrows(-60,-4.5, -120, -4.5, lwd=1, length=0.13, angle=20)
text(10, -5.5, "ZMapp Better", cex=0.8)
text(-110, -5.5, "ZMapp Worse", cex=0.8)

arrows(520,-4.5, 650, -4.5, lwd=1, length=0.13, angle=20)
arrows(480,-4.5, 350, -4.5, lwd=1, length=0.13, angle=20)
text(580, -5.5, "ZMapp Better", cex=0.8)
text(420, -5.5, "ZMapp Worse", cex=0.8)

dev.off()


detach(dta)





#
#
### send the following dataset to Lori and Mike
exact_CI_function<-function(x0, n0, x1, n1,confylevel=0.95){
  
  estimate<-round((x1/n1-x0/n0)*100,1)
  confy<-uncondExact2x2(x0, n0, x1, n1, parmtype = "difference", 
                        nullparm = NULL, alternative = "two.sided", conf.int = TRUE, conf.level = confylevel, 
                        method = "FisherAdj", tsmethod = "central", midp = FALSE, gamma = 0, 
                        EplusM=FALSE, tiebreak=FALSE, plotprobs = FALSE, control=ucControl(), 
                        Tfunc=NULL)
  outCI<-paste0(estimate," (",round(confy$conf.int[1]*100,1)," to ",round(confy$conf.int[2]*100,1),")")
  outPvalue<-round(confy$p.value,4)
  return(c(outCI, outPvalue))
}

dta1<-raw%>%
  filter(!is.na(Var))%>%
  rowwise()%>%
  mutate(AB_CI=exact_CI_function(A_death, A_total, B_death, B_total)[1],
         AC_CI=exact_CI_function(A_death, A_total, C_death, C_total)[1],
         AD_CI=exact_CI_function(A_death_conc, A_total_conc, D_death, D_total)[1],
         `Group A`=paste0(A_death,"/",A_total),
         `Group B`=paste0(B_death,"/",B_total),
         `Group C`=paste0(C_death,"/",C_total),
         `Group D`=paste0(D_death,"/",D_total),
         AB_pvalue=exact_CI_function(A_death, A_total, B_death, B_total)[2],
         AC_pvalue=exact_CI_function(A_death, A_total, C_death, C_total)[2],
         AD_conc_pvalue=exact_CI_function(A_death_conc, A_total_conc, D_death, D_total)[2])%>%
  mutate(AB_OR=fisher_exact_function(A_death, A_total, B_death, B_total)[1],
         AC_OR=fisher_exact_function(A_death, A_total, C_death, C_total)[1],
         AD_OR=fisher_exact_function(A_death_conc, A_total_conc, D_death, D_total)[1],
         AB_OR_pvalue=fisher_exact_function(A_death, A_total, B_death, B_total)[2],
         AC_OR_pvalue=fisher_exact_function(A_death, A_total, C_death, C_total)[2],
         AD_OR_conc_pvalue=fisher_exact_function(A_death_conc, A_total_conc, D_death, D_total)[2],
         AB_OR_LCL=fisher_exact_function(A_death, A_total, B_death, B_total)[3],
         AC_OR_LCL=fisher_exact_function(A_death, A_total, C_death, C_total)[3],
         AD_OR_LCL_conc=fisher_exact_function(A_death_conc, A_total_conc, D_death, D_total)[3],
         AB_OR_UCL=fisher_exact_function(A_death, A_total, B_death, B_total)[4],
         AC_OR_UCL=fisher_exact_function(A_death, A_total, C_death, C_total)[4],
         AD_OR_UCL_conc=fisher_exact_function(A_death_conc, A_total_conc, D_death, D_total)[4])

dta2<-dta1%>%
  mutate(AB_OR=paste0(AB_OR, "(", AB_OR_LCL, ",", AB_OR_UCL,")"),
         AC_OR=paste0(AC_OR, "(", AC_OR_LCL, ",", AC_OR_UCL,")"),
         AD_OR=paste0(AD_OR, "(", AD_OR_LCL_conc, ",", AD_OR_UCL_conc,")"))

write.csv(dta2,paste0(indir,"PALM_mortalityRateDiff_OR_CI_Pvalue_",Sys.Date(),".csv"),row.names = FALSE,na="")









