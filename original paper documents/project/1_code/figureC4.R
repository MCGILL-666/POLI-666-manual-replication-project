###################################################################
## figureC4.R
###################################################################

rm(list=ls())

library(optmatch)

load("../2_output/0_results/results.altmethods.RData")
load("../2_output/0_results/results.RData")

mainlist <- c(".female.sum.2016.altmethods", 
                 ".num_aspirants_total.2016.altmethods",
                 ".female.nominee.2016.altmethods",
                 ".not_owngroups.sum.2016.altmethods", 
                 ".cand_owngroups.sum.2016.altmethods",  
                 ".owngroup.nominee.2016.altmethods", 
                 ".private_sector.ONLY.nominee.2016.altmethods",  
                 ".incumbent.nominee.2016.altmethods") 

outcomes.2016.english <- c("Num. Female Aspirants",
                           "Total Number of Aspirants",
                           "Nominee is Female",
                           "Num.Asp. from \nNon-Associated Ethnic Groups",
                           "Num.Asp. from \nParty-Associated Ethnic Groups",
                           "Nominee is a \nParty-Associated Ethnic Group Member",
                           "Nominee has Private Sector Background",
                           "Nominee is the Incumbent")


effects <- effect.size.lm[c(1,5,6,2,3,7,9,10),]
ci95 <- effect.size.lm.ci95[c(1,5,6,2,3,7,9,10),]
ci90 <- effect.size.lm.ci90[c(1,5,6,2,3,7,9,10),]

qtlist <- c(0.975, 0.95)
collist <- c("gray78", "black")
typelist <- c("ps", "mah", "gen")

pdf(file="../2_output/1_figs/figC4_altmethods_results.pdf", height=10, width=7)
par(mfrow=c(4,2))

for(i in 1:length(mainlist)){
par(mar=c(5,12,4,2))        
xlim_max <- 1.1*max(ci95[i,2], 
                    eval(parse(text=paste("tab", mainlist[i], 
                        '["ps.exact","est"] + (qt(0.975, df=result', mainlist[i], 
                        '$ps.exact$wnobs)*tab', mainlist[i], '["ps.exact","se"])', sep=""))),
                    eval(parse(text=paste("tab", mainlist[i], 
                        '["mah.exact","est"] + (qt(0.975, df=result', mainlist[i], 
                        '$mah.exact$wnobs)*tab', mainlist[i], '["mah.exact","se"])', sep=""))),
                    eval(parse(text=paste("tab", mainlist[i], 
                        '["gen.exact","est"] + (qt(0.975, df=result', mainlist[i], 
                        '$gen.exact$wnobs)*tab', mainlist[i], '["gen.exact","se"])', sep="")))
                    )
xlim_min <- 1.1*min(ci95[i,1], 
                    eval(parse(text=paste("tab", mainlist[i], 
                        '["ps.exact","est"] - (qt(0.975, df=result', mainlist[i], 
                        '$ps.exact$wnobs)*tab', mainlist[i], '["ps.exact","se"])', sep=""))),
                    eval(parse(text=paste("tab", mainlist[i], 
                        '["mah.exact","est"] - (qt(0.975, df=result', mainlist[i], 
                        '$mah.exact$wnobs)*tab', mainlist[i], '["mah.exact","se"])', sep=""))),
                    eval(parse(text=paste("tab", mainlist[i], 
                        '["gen.exact","est"] - (qt(0.975, df=result', mainlist[i], 
                        '$gen.exact$wnobs)*tab', mainlist[i], '["gen.exact","se"])', sep="")))
                    )

                    
plot(effects[i,1], 1, pch=16, col="white", xlab="Estimated ATT", cex.main=0.8, 
     ylab="", yaxt="n", ylim=c(0.6,4.4), xlim=c(xlim_min, xlim_max), 
     main=paste("Outcome: ", outcomes.2016.english[i], sep=""))  

abline(v=0, lwd=1.8, lty="dashed", col="firebrick")
segments(ci95[i,1], 1, ci95[i,2], 1, lwd=3.1, col="gray78")
segments(ci90[i,1], 1, ci90[i,2], 1, lwd=3.1, col="black")
points(effects[i,1], 1, pch=18, col="dodgerblue2", cex=2.1)
axis(2, at=c(1,2,3,4), labels=c("Optimal full matching (main)", "Propensity score", "Mahalanobis distance", "Genetic matching"), las=2, cex=0.8)

for(k in 1:3){
        # plot the 90 and 95% confidence intervals for the alternative methods
        for(j in c(1:2)){ 
        segments(eval(parse(text=paste("tab", mainlist[i], '["', typelist[k], 
                                       '.exact","est"] - (qt(', qtlist[j], 
                                       ', df=result', mainlist[i], "$ps.exact$wnobs)*tab", 
                                       mainlist[i], '["', typelist[k], '.exact","se"])', 
                                       sep=""))), k+1,
                 eval(parse(text=paste('tab', mainlist[i], '["', typelist[k], 
                                       '.exact","est"] + (qt(', qtlist[j], 
                                       ', df=result', mainlist[i], "$ps.exact$wnobs)*tab", 
                                       mainlist[i], '["', typelist[k], '.exact","se"])', 
                                       sep=""))), k+1, 
                 lwd=3.1, col=collist[j])
        }
        # plot the point estimates
        points(eval(parse(text=paste('tab', mainlist[i], '["', typelist[k], 
                                     '.exact","est"]', sep=""))), k+1, pch=16, 
               col="darkseagreen", cex=2.1)
}
}
dev.off()


