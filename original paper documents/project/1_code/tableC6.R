###################################################################
## tableC6.R
##
###################################################################

rm(list=ls())
library(xtable)

load("../2_output/0_results/results.noexact.RData") # from analysis_without_em.R

x <- cbind(tab.balance, n.treat.m1, num.sets.m1)

write.csv(round(x,2), file=c("../2_output/2_tables/tabC6_balance_noexact.csv"))

tab.balance.pm<- xtable(x, label=c("tab:imbalance.pm"),
                        align=c("l", "r", "r","r","r","r"),
                        digits=c(1,2,0,2,0,0))

print.xtable(tab.balance.pm, caption.placement=c("top"), 
             file=c("../2_output/2_tables/tabC6_balance_noexact.tex"))
             