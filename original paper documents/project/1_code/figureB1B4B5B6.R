###################################################################
## figureB1B4B5B6.R
##
## balance on individual matching variables for all outcomes
## main specification
###################################################################

rm(list=ls())
library(optmatch)

load("../2_output/0_results/results.RData") 

mainlist <- list(result.female.sum.2016.main, 
                 result.not_owngroups.sum.2016.main, 
                 result.cand_owngroups.sum.2016.main,  
                 result.numasp_notowngroup_female.sum.2016.main,
                 result.num_aspirants_total.2016.main,
                 result.female.nominee.2016.main,
                 result.owngroup.nominee.2016.main,
                 result.notowngroup.female.nominee.2016.main,  
                 result.private_sector.ONLY.nominee.2016.main,  
                 result.incumbent.nominee.2016.main) 

mlistname <- c("female_sum",
               "not_owngroups_sum",
               "cand_owngroups_sum",
               "numasp_notowngroup_female_sum",
               "num_aspirants_total",
               "female_nominee",
               "owngroup_nominee",
               "notowngroup_female_nominee",
               "private_sector_ONLY_nominee",
               "incumbent_nominee")

## for figure numbering
figB <- c("4a", "5a", "5b", "1a", "4b", "4c", "5c", "1b", "6a", "6b")

## for variable labels
x1 <- rownames(mainlist[[1]]$balance$results)
for(i in 2:length(mlistname)){
  x <- mainlist[[i]]
  x1 <- c(x1, rownames(x$balance$results))
}
names_mvar <- unique(x1)
labels_mvar <- c("2012 Parl. vote share", "2012 Pres. vote share", 
                 "Ethnic fractionalization (in party)", "Population density",
                 "% Muslim", "% Largest ethnic group",
                 "Segregation (across parties)", "Segregation (within party)",
                 "Num. female aspirants (2012)", "% Party's core groups",
                 "Num. aspirants (2012)", "% Incumbent's ethnic group")


for(i in 1:length(mlistname)){
  x <- mainlist[[i]]
  num_mvar <- dim(x$balance$results)[1]
  ess <- round(eff.sample.size[i],0)
  
  pdf(file=paste0("../2_output/1_figs/figB", figB[i],"_prop_score_balance_", mlistname[i],".pdf", sep=""), height=6, width=10)
  par(mar=c(4.25,18,2.8,1))
  plot(x$balance$results[1:num_mvar], c(num_mvar:1),
       pch=16, col="dimgrey",
       xlab="", yaxt="n", ylab="", cex=1.5,
       xlim=c(min(x$balance$results[1:num_mvar])-.1, max(x$balance$results[1:num_mvar])+.1)
  )
  segments(
    x0=x$balance$results[1:num_mvar],
    y0=c(num_mvar:1), 
    x1=x$balance$results[(3*num_mvar+1):(4*num_mvar)],
    lwd=1
  )
  points(
    x$balance$results[(3*num_mvar+1):(4*num_mvar)], c(num_mvar:1), 
    pch=15, col="red", cex=1.5
  )
  abline(v=0, lty="dashed", col="darkseagreen", lwd=1.5)
  vars_balance <- rownames(x$balance$results)
  lab_balance <- labels_mvar[match(vars_balance, names_mvar)]
  
  axis(2, at=c(num_mvar:1), labels=lab_balance, las=1, cex.axis=1.3)
  
  legend(x="bottomright", legend=c("Before","After", paste("eff. n=", bquote(.(ess)))), 
         pch=c(16,15,15), col=c("dimgrey", "red", "white"), cex=1.3)
  dev.off()
}

