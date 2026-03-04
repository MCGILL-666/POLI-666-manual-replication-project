###################################################################
## Figure2.R
##
###################################################################

rm(list=ls())
library(optmatch)
library(ggplot2)

load("../2_output/0_results/results.RData")

effect.size.lm.reorder <- effect.size.lm[c(1,5,6,2,3,7,9,10),]
effect.size.lm.ci95.reorder <- effect.size.lm.ci95[c(1,5,6,2,3,7,9,10),]
effect.size.lm.ci90.reorder <- effect.size.lm.ci90[c(1,5,6,2,3,7,9,10),]

pdf(file="../2_output/1_figs/fig2_results.pdf", height=9, width=9)
par(mar=c(5,15,1,1), cex=1.3)
plot(effect.size.lm.reorder[,1], c(15, 13, 11, 9, 7, 5, 3, 1), 
     xlim=c(min(effect.size.lm.ci95.reorder) - 0.1, max(effect.size.lm.ci95.reorder) + 0.1), 
     pch=16, col="white", xlab="Estimated ATT", ylab="", yaxt="n")
abline(v=0, lwd=1.8, lty="dashed", col="firebrick")
segments(effect.size.lm.ci95.reorder[,1], c(15, 13, 11, 9, 7, 5, 3, 1), 
         effect.size.lm.ci95.reorder[,2], c(15, 13, 11, 9, 7, 5, 3, 1), 
         lwd=3.1, col="gray78")
segments(effect.size.lm.ci90.reorder[,1], c(15, 13, 11, 9, 7, 5, 3, 1), 
         effect.size.lm.ci90.reorder[,2], c(15, 13, 11, 9, 7, 5, 3, 1), 
         lwd=3.1, col="black")
points(effect.size.lm.reorder[,1], c(15, 13, 11, 9, 7, 5, 3, 1), pch=16, col="dodgerblue2", cex=2.1)
#hard to automate here (b/c it needs manual spacing adjustments), so update these by hand from: round(effect.size.lm[,4], digits=2) and round(eff.sample.size, digits=0)
axis(2, at=c(15, 13, 11, 9, 7, 5, 3, 1), 
     labels=c("Number of female aspirants\n(est=0.17, p<0.01)",
              "Total number of aspirants\n(est=0.31, p=0.06)",
              "Nominee is female\n(est=0.08, p<0.01)",
              "Num. aspirants from non-core\n ethnic groups (est=0.43, p<0.01)",
              "Num. aspirants from party's\ncore ethnic group(s)\n(est=-0.21, p=0.09)",
              "Nominee belongs to party's\ncore ethnic group(s)\n(est=-0.25, p<0.01)",
              "Nominee has private sector\n background only\n(est=-0.11, p<0.01)",
              "Nominee is the incumbent\n(est=0.17, p<0.01)"), las=2)
dev.off()

