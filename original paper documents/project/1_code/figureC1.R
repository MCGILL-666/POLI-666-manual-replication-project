###################################################################
## figureC1.R
##
###################################################################

rm(list=ls())

load("../2_output/0_results/results.caliper.RData") # from analysis_with_calipers.R
load("../2_output/0_results/results.RData") # from analysis.R

outcomes.2016.english <- c("Num.Female Aspirants",
                           "Total Number of Aspirants",
                           "Nominee is Female",
                           "Num.Asp. from Non-Associated Ethnic Groups",
                           "Num.Asp. from Party-Associated Ethnic Groups",
                           "Nominee is a Party-Associated Ethnic Group Member",
                           "Nominee has Private Sector Background", 
                           "Nominee is the Incumbent")

effect.size <- effect.size.lm[c(1,5,6,2,3,7,9,10),]
calipers.effect.size <- calipers.effect.size.lm[c(3,4,6,1,2,5,7,8),]
calipers.effect.size.ci95.low <- calipers.effect.size.lm.ci95.low[c(3,4,6,1,2,5,7,8),]
calipers.effect.size.ci95.high <- calipers.effect.size.lm.ci95.high[c(3,4,6,1,2,5,7,8),]
calipers.effect.size.ci90.low <- calipers.effect.size.lm.ci90.low[c(3,4,6,1,2,5,7,8),]
calipers.effect.size.ci90.high <- calipers.effect.size.lm.ci90.high[c(3,4,6,1,2,5,7,8),]
calipers.prop.score.balance.nt.reordered <- 
  calipers.prop.score.balance.nt[c(3,4,6,1,2,5,7,8),]

rightside_axislabels <- seq(0,1,0.2)*max(calipers.prop.score.balance.nt.reordered)
ylim_bottom <- c(-0.25,-0.25,-0.25,-0.25,-0.8,-0.8,-0.5,-0.5)
ylim_top <- c(1,1,0.5,1,0.5,0.5,0.5,0.5)

pdf(file="../2_output/1_figs/figC1_calipers.pdf", height=12, width=8)
par(mfrow=c(4,2))
for(i in c(1:nrow(effect.size))){
  par(mar=c(4,4,3,5))
  # plot the ATT estimates
  plot(calipers, calipers.effect.size[i,], col="white", 
       ylim=c(ylim_bottom[i], ylim_top[i]), xlim=c(0,1.5), xaxt="n", 
       xlab="caliper restriction (sd of propensity score)", 
       ylab="Estimated ATT", main=outcomes.2016.english[i])
  axis(1, at=c(0, 0.5, 1, 1.5), labels=c(0, 0.5, 1, 1.5))
  abline(h=0)
  abline(h= effect.size[i,1], col="dodgerblue2", lwd=2.1, lty="dashed")
  
  # CIs
  segments(calipers, calipers.effect.size.ci95.low[i,], 
           calipers, calipers.effect.size.ci95.high[i,],  lwd=3.1, col="gray78")
  segments(calipers, calipers.effect.size.ci90.low[i,], 
           calipers, calipers.effect.size.ci90.high[i,],  lwd=3.1, col="black")
  points(calipers, calipers.effect.size[i,], pch=16, col="dodgerblue2", cex=2.1)
  
  # number of treated units in the sample after matching
  axis(4, at=c(ylim_bottom[i], 
               ylim_bottom[i] + 0.2*(ylim_top[i]-ylim_bottom[i]), 
               ylim_bottom[i] + 0.4*(ylim_top[i]-ylim_bottom[i]), 
               ylim_bottom[i] + 0.6*(ylim_top[i]-ylim_bottom[i]), 
               ylim_bottom[i] + 0.8*(ylim_top[i]-ylim_bottom[i]), 
               ylim_top[i]), labels=rightside_axislabels)
  mtext("treated units after matching", side = 4, line = 2.5, cex=0.7)
  points(calipers + 0.01, 
         ylim_bottom[i]+ (calipers.prop.score.balance.nt.reordered[i,] / 
            max(calipers.prop.score.balance.nt.reordered))*(ylim_top[i]-ylim_bottom[i]), 
         pch=18, cex=1.5, col="firebrick1")
  
}
dev.off()
