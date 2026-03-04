###################################################################
## tableC3.R
##
###################################################################

rm(list=ls())

load("../2_output/0_results/results.RData") # from analysis.R

# first three columns are before matching, second three columns are after matching
x <- cbind(pre.match.balance, prop.score.balance.10)
x <- x[c(1,5,6,2,3,7,9,10),]

write.csv(round(x,2), file=c("../2_output/2_tables/tabC3_balance10.csv"))

tab.balance.10<- xtable(x, label=c("tab:balance.main"),
                        align=c("l", "r", "r","r","r","r","r"),
                        digits=c(1,2,0,2,2,0,2))

print.xtable(tab.balance.10, caption.placement=c("top"), 
             file=c("../2_output/2_tables/tabC3_balance10.tex"))
             