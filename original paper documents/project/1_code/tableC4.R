###################################################################
## tableC4.R
##
###################################################################

rm(list=ls())

load("../2_output/0_results/results.RData") # from analysis.R

x <- cbind(effect.size.lm.10[c(1,5,6,2,3,7,9,10),-3],
           tab.n.treat.em.10[c(1,5,6,2,3,7,9,10)],
           num.sets.em.10[c(1,5,6,2,3,7,9,10)])
colnames(x) <- c("Estimate", "Std.Error", "p-value", "nT", "Sets")

colnames(x) <- c("Estimate", "Std.Error", "p-value")

write.csv(round(x,2), file=c("../2_output/2_tables/tabC4_restricted.csv"))

tab.results.restricted <- xtable(x,
                          label=c("tab:results.10"),
                          align=c("l", "r","r", "r", "r", "r"),
                          digits=c(1,2,2,2,0,0))

print.xtable(tab.results.restricted, caption.placement=c("top"), 
             file=c("../2_output/2_tables/tabC4_restricted.tex"))
             