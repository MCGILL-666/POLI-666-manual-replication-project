###################################################################
## tableC2.R
##
###################################################################

rm(list=ls())

load("../2_output/0_results/results.RData") # from analysis.R

x <- effect.size.lm.pre[c(1,5,6,2,3,7,9,10),-3]
colnames(x) <- c("Estimate", "Std.Error", "p-value")

write.csv(round(x,2), file=c("../2_output/2_tables/tabC2_ols.csv"))

tab.results.ols <- xtable(x,
                          label=c("tab:results.ols"),
                          align=c("l", "r","r", "r"),
                          digits=c(1,2,2,2))

print.xtable(tab.results.ols, caption.placement=c("top"), 
             file=c("../2_output/2_tables/tabC2_ols.tex"))
             