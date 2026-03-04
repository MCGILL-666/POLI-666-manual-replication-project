###################################################################
## figureC2.R
##
###################################################################

rm(list=ls())

load("../2_output/0_results/results.noexact.RData") # from analysis_without_em.R

effect.size.lm.ci95 <- rbind(result.female.sum.2016.fullopt.noexact$lm.confint.95, 
                             result.num_aspirants_total.2016.fullopt.noexact$lm.confint.95,
                             result.female.nominee.2016.fullopt.noexact$lm.confint.95,
                             result.not_owngroups.sum.2016.fullopt.noexact$lm.confint.95, 
                             result.cand_owngroups.sum.2016.fullopt.noexact$lm.confint.95,  
                             result.owngroup.nominee.2016.fullopt.noexact$lm.confint.95,
                             result.private_sector.ONLY.nominee.2016.fullopt.noexact$lm.confint.95,  
                             result.incumbent.nominee.2016.fullopt.noexact$lm.confint.95) 

effect.size.lm.ci90 <- rbind(result.female.sum.2016.fullopt.noexact$lm.confint.90, 
                             result.num_aspirants_total.2016.fullopt.noexact$lm.confint.90,
                             result.female.nominee.2016.fullopt.noexact$lm.confint.90,
                             result.not_owngroups.sum.2016.fullopt.noexact$lm.confint.90, 
                             result.cand_owngroups.sum.2016.fullopt.noexact$lm.confint.90,  
                             result.owngroup.nominee.2016.fullopt.noexact$lm.confint.90,
                             result.private_sector.ONLY.nominee.2016.fullopt.noexact$lm.confint.90,  
                             result.incumbent.nominee.2016.fullopt.noexact$lm.confint.90) 


outcomes.2016.english <- c("Number of female aspirants",
                           "Total number of aspirants",
                           "Nominee is female",
                           "Num. aspirants from non-core ethnic groups",
                           "Num. aspirants from party's core ethnic groups",
                           "Nominee belongs to party's core ethnic group(s)",
                           "Nominee has private sector background only",
                           "Nominee is the incumbent")

labels.noem<- rep(NA, length(outcomes.2016.english))

for(i in 1:length(outcomes.2016.english)){
  labels.noem[i] <- paste(outcomes.2016.english[i], " \n(est=", round(tab.m1.est[i,1],2), 
                ", p=", round(tab.m1.est[i,4], 2), ")", sep="")
}

pdf(file="../2_output/1_figs/figC2_noexact_results.pdf", height=7, width=9)
par(mar=c(5,20,1,1), cex=1.1)

plot(tab.m1.est[,1], seq(15, 1, -2), 
     xlim=c(min(effect.size.lm.ci95) - 0.1, max(effect.size.lm.ci95) + 0.1), 
     ylim=c(0.5, 15.5), pch=16, col="white", xlab="Estimated ATT", ylab="", yaxt="n")
abline(v=0, lwd=1.8, lty="dashed", col="firebrick")
segments(effect.size.lm.ci95[,1], seq(15, 1, -2), 
         effect.size.lm.ci95[,2], seq(15, 1, -2), lwd=3.1, col="gray78")
segments(effect.size.lm.ci90[,1], seq(15, 1, -2), 
         effect.size.lm.ci90[,2], seq(15, 1, -2), lwd=3.1, col="black")
points(tab.m1.est[,1], seq(15, 1, -2), pch=16, col="dodgerblue2", cex=2.1)
axis(2, 
     at=seq(15, 1, -2), 
     labels=labels.noem,
     las=2)

dev.off()


