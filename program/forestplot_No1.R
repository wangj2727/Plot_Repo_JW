### Description: this is the code to create forest plot of Recovery Rate Ratios by baseline characteristic groups

library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)

#### input dataset:
# Index   subgroup   RRR `95% CI`       n
# Overall NA        1.29 1.12, 1.49  1062



dta <- dta0%>%
  mutate(LCL = as.numeric(sapply(strsplit(dta0$`95% CI`, ","), function(x) x[1])), 
         UCL = as.numeric(sapply(strsplit(dta0$`95% CI`, ","), function(x) x[2])),
         `95% CI` = paste0("(",`95% CI`,")"),
         `95% CI` = ifelse(`95% CI`=="(NA)", "",`95% CI`))


axes <- c(1/5, 1/4, 1/3, 1/2, 1, 2,3,4,5)
Num_row <- nrow(dta)
loc <- c(log(1/5), log(1/(4.5)), log(1/(2.1)), log(4), log(5.5))


attach(dta)

par(mar=(c(5,1,1,1)))

plot(NA, xlim=c(log(1/6), log(6)), ylim=c(0, nrow(dta0)+2), xlab="", 
     ylab="", main="", axes = FALSE)

for(i in 1:Num_row){
  
  ### bars
  if(i==Num_row){
    points(log(RRR[Num_row+1-i]), i, pch=19, cex=1.5)
    text(log(LCL[Num_row+1-i]), i, "(", font=2)
    text(log(UCL[Num_row+1-i]), i, ")", font=2)
    segments(log(LCL[Num_row+1-i]), i, log(UCL[Num_row+1-i]), i, lwd=2)
  }else{
    points(log(RRR[Num_row+1-i]), i, pch=19, cex=1)
    text(log(LCL[Num_row+1-i]), i, "(")
    text(log(UCL[Num_row+1-i]), i, ")")
    segments(log(LCL[Num_row+1-i]), i, log(UCL[Num_row+1-i]), i)
  }
  
  text(loc[1], i, dta$Index[Num_row+1-i], adj=0, font=2)
  text(loc[2], i, dta$subgroup[Num_row+1-i], adj=0)
  text(loc[3], i, dta$n[Num_row+1-i])
  
  text(loc[4], i, dta$RRR[Num_row+1-i])
  text(loc[5], i, dta$`95% CI`[Num_row+1-i])
  
}



segments(0, -2, 0, Num_row+1, lty=2)
segments(loc[3]-log(0.95), nrow(dta0)+1.3, loc[3]+log(0.95),nrow(dta0)+1.3)
segments(loc[4]-log(0.9), nrow(dta0)+1.3, loc[4]+log(0.9),nrow(dta0)+1.3)
segments(loc[5]-log(0.85), nrow(dta0)+1.3, loc[5]+log(0.85),nrow(dta0)+1.3)

text(loc[3], nrow(dta0)+2, labels = "n", font=2)
text(loc[4], nrow(dta0)+2, labels = "RRR", font=2)
text(loc[5], nrow(dta0)+2, labels = "95% CI", font=2)

axis(1, at=log(axes), labels=c("1/5", "1/4", "1/3", "1/2", 1:5))

### Add arrows
arrows(0,-5.7, log(3), -5.7, lwd=2, length=0.13, angle=20, 
       col="blue", xpd=TRUE)
arrows(0,-5.7, log(1/3), -5.7, lwd=2, length=0.13, angle=20, 
       col="red", xpd=TRUE)
text(log(1.2), -6.5, "Remdesivir better", xpd=TRUE, col="blue", adj=0)
text(log(1/(1.2)), -6.5, "Placebo better", xpd=TRUE, col="red", adj=1)

mtext(side=1, line=2, text="Recovery Rate Ratio")


detach(dta)






