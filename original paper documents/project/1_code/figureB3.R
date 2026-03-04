###################################################################
## figureB3.R
##
###################################################################

rm(list=ls())
library(optmatch)

load("../2_output/0_results/results.minusdropout.RData") # from analysis_excl_dropout.R
load("../2_output/0_results/results.RData") # from analysis.R

pdf(file="../2_output/1_figs/figB3_dropout.pdf", height=4, width=9)
par(mar=c(5,13,1,1))

plot(effect.size.lm[c(2,1,5),1], c(5, 3, 1), 
     xlim=c(min(effect.size.lm.ci95) - 0.1, max(effect.size.lm.ci95) + 0.1), 
     ylim=c(0.6, 5.4), pch=16, col="white", 
     xlab="Estimated ATT", ylab="", yaxt="n")
abline(v=0, lwd=1.8, lty="dashed", col="firebrick")
segments(effect.size.lm.ci95[c(2,1,5),1], c(5, 3, 1), 
         effect.size.lm.ci95[c(2,1,5),2], c(5, 3, 1), 
         lwd=3.1, col="gray78")
segments(effect.size.lm.ci90[c(2,1,5),1], c(5, 3, 1), 
         effect.size.lm.ci90[c(2,1,5),2], c(5, 3, 1), 
         lwd=3.1, col="black")
points(effect.size.lm[c(2,1,5),1], c(5, 3, 1), pch=16, col="dodgerblue2", cex=2.1)

axis(2, at=c(5, 3, 1), 
     labels=c("Num. aspirants from non-core\n ethnic groups\nOriginal: 0.43, p<0.01\nAdjusted: 0.38, p<0.01",
        "Number of female aspirants\nOriginal: 0.17, p<0.01\nAdjusted: 0.15, p<0.01 ",
        "Total number of aspirants\nOriginal: 0.31, p=0.06\nAdjusted: 0.19, p=0.20"), las=2)

segments(result.num_aspirants_total.minusdropout.2016.main$lm.confint.95[,1], 0.8, 
         result.num_aspirants_total.minusdropout.2016.main$lm.confint.95[,2], 0.8, 
         lwd=3.1, col="gray78")
segments(result.num_aspirants_total.minusdropout.2016.main$lm.confint.90[,1], 0.8, 
         result.num_aspirants_total.minusdropout.2016.main$lm.confint.90[,2], 0.8, 
         lwd=3.1, col="black")
points(result.num_aspirants_total.minusdropout.2016.main$lm.est[1], 0.8, pch=16, col="orange", cex=2.1)

segments(result.female.sum.minusdropout.2016.main$lm.confint.95[,1], 2.8, 
         result.female.sum.minusdropout.2016.main$lm.confint.95[,2], 2.8, 
         lwd=3.1, col="gray78")
segments(result.female.sum.minusdropout.2016.main$lm.confint.90[,1], 2.8,
         result.female.sum.minusdropout.2016.main $lm.confint.90[,2], 2.8, 
         lwd=3.1, col="black")
points(result.female.sum.minusdropout.2016.main$lm.est[1], 2.8, pch=16, col="orange", cex=2.1)

segments(result.not_owngroups.sum.minusdropout.2016.main$lm.confint.95[,1], 4.8, 
         result.not_owngroups.sum.minusdropout.2016.main$lm.confint.95[,2], 4.8, 
         lwd=3.1, col="gray78")
segments(result.not_owngroups.sum.minusdropout.2016.main$lm.confint.90[,1], 4.8, 
         result.not_owngroups.sum.minusdropout.2016.main$lm.confint.90[,2], 4.8, 
         lwd=3.1, col="black")
points(result.not_owngroups.sum.minusdropout.2016.main$lm.est[1], 4.8, pch=16, col="orange", cex=2.1)

dev.off()
