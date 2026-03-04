###################################################################
## tableC5.R
##
###################################################################

rm(list=ls())
library(xtable)

load("../2_output/0_results/results.RData") # from analysis.R

x <- cbind(prop.score.balance.pm, tab.n.treat.pm, tab.n.treat.pm)
x <- x[c(1,5,6,2,3,7,9,10),]

write.csv(round(x,2), file=c("../2_output/2_tables/tabC5_balance_pm.csv"))

tab.balance.pm<- xtable(x, label=c("tab:imbalance.pm"),
                        align=c("l", "r", "r","r","r","r"),
                        digits=c(1,2,0,2,0,0))

print.xtable(tab.balance.pm, caption.placement=c("top"), 
             file=c("../2_output/2_tables/tabC5_balance_pm.tex"))
             