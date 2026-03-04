###################################################################
## figureC3.R
###################################################################

rm(list=ls())

library(optmatch)

load("../2_output/0_results/results.altmethods.RData") # from analysis_altmethods.R
load("../2_output/0_results/results.RData")

mainlist <- c("result.female.sum.2016.", 
                 "result.num_aspirants_total.2016.",
                 "result.female.nominee.2016.",
                 "result.not_owngroups.sum.2016.", 
                 "result.cand_owngroups.sum.2016.",  
                 "result.owngroup.nominee.2016.", 
                 "result.private_sector.ONLY.nominee.2016.",  
                 "result.incumbent.nominee.2016.") 

before <- list()
ours <- list()
genetic <- list()
maha <- list()
nearest <- list()

for(i in 1:length(mainlist)){
        before[[i]] <- eval(parse(text=paste(mainlist[i], 
                        "altmethods$tab.balance.sdiff.gen.exact[,1]", sep="")))
        ours[[i]] <-  eval(parse(text=paste(mainlist[i], 
                        "main$balance$results[ , ,2][,1]", sep="")))
        genetic[[i]] <- eval(parse(text=paste(mainlist[i], 
                        "altmethods$tab.balance.sdiff.gen.exact[,2]", sep="")))
        maha[[i]] <- eval(parse(text=paste(mainlist[i], 
                        "altmethods$tab.balance.sdiff.mah.exact[,2]", sep="")))
        nearest[[i]] <- eval(parse(text=paste(mainlist[i], 
                        "altmethods$tab.balance.sdiff.ps.exact[,2]", sep="")))
}

before <- unlist(before)
ours <- c(unlist(ours), rep(0,8)) #put in the 0s for the lagged DV
genetic <- unlist(genetic) 
maha <- unlist(maha)
nearest <- unlist(nearest)




pdf(file="../2_output/1_figs/figC3_alt_matching_balance_pooled.pdf", height=10, width=10)
par(mfrow=c(3,2), mar = c(7, 5, 7, 2) + 0.1, cex.lab=1.5, cex.main=1.9)

hist(abs(before),  col="firebrick", 
     xlab="Abs. value of standardized mean difference (T - C)", 
     main=paste("No matching\n mean=", paste(round(mean(abs(before)), digits=3)), "\n", step=""), 
     xlim=c(0, 0.9), breaks=18, border="white", ylim=c(0,20) )
abline(v=mean(abs(before)), col="black", lty="dashed", lwd=2.2)

hist(abs(nearest),  col="goldenrod", 
     xlab="Abs. value of standardized mean difference (T - C)", 
     main=paste("Propensity score\n mean=", paste(round(mean(abs(nearest)), digits=3)), 
                "\n", step=""), 
     xlim=c(0, 0.9), breaks=18, border="white", ylim=c(0,20))
abline(v=mean(abs(nearest)), col="black", lty="dashed", lwd=2.2)


hist(abs(genetic),  col="gray56", xlab="Abs. value of standardized mean difference (T - C)", 
     main=paste("Genetic matching\n mean=", paste(round(mean(abs(genetic)), digits=3)), 
                "\n", step=""), 
     xlim=c(0, 0.9), breaks=18, border="white", ylim=c(0,20))
abline(v=mean(abs(genetic)), col="black", lty="dashed", lwd=2.2)

hist(abs(maha),  col="darkseagreen", 
     xlab="Abs. value of standardized mean difference (T - C)", 
     main=paste("Mahalanobis distance\n mean=", paste(round(mean(abs(maha)), digits=3)),
                "\n", step=""), 
     xlim=c(0, 0.9), breaks=18, border="white", ylim=c(0,20))
abline(v=mean(abs(maha)), col="black", lty="dashed", lwd=2.2)


hist(abs(ours),  col="dodgerblue2", 
     # xlab="Abs. value of standardized weighted average of\n within-stratum mean differences (T - C)",
     xlab="",
     main=paste("Optimal full matching (main version)\n mean=", 
                paste(round(mean(abs(ours)), digits=3)), "\n", step=""), 
     xlim=c(0, 0.9), breaks=5, border="white", ylim=c(0,29))
abline(v=mean(abs(ours)), col="black", lty="dashed", lwd=2.2)
mtext("Abs. value of weighted average of within-stratum\n standardized mean differences (T - C)", side=1, line=4)
dev.off()

