###################################################################
## tableB2.R
##
###################################################################

rm(list=ls())
library(xtable)

load("../2_output/0_results/results.mpopshare.RData") # from analysis_by_mpopshare.R

## number of matched sets and treated units post matching
num.sets <- rbind(
  sum(summary(matchingresult.female.nominee.2016.belowmedian$em)[[2]]),
  sum(summary(matchingresult.female.nominee.2016.abovemedian$em)[[2]])
)

num.treateds <- rbind(
  matchingresult.female.nominee.2016.belowmedian$n.treat.em,
  matchingresult.female.nominee.2016.abovemedian$n.treat.em
)

temp1 <- cbind(effect.size.lm.em[,c(1,2,4)], num.treateds, num.sets)
colnames(temp1) <- c("Estimate", "S.E.", "$p$-value", "n_T", "Sets")


write.csv(round(temp1,2), file=c("../2_output/2_tables/tabB2_femnom_mpop.csv"))

tab <- xtable(temp1,
              caption=c("Estimated Effects on Whether Nominee is Female"), 
              label=c("tab:results.female.nominee.muslim"), 
              align=c("l", "r", "r", "r", "r", "r"), 
              digits=c(0,2,2,2,0,0))

print.xtable(tab, caption.placement=c("top"), 
             file=c("../2_output/2_tables/tabB2_femnom_mpop.tex"))

