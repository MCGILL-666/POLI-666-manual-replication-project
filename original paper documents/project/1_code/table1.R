###################################################################
## table1.R
## 
###################################################################

rm(list=ls())

library(optmatch)

load("../2_output/0_results/results.RData")

outcomes.english <-c("Number of female aspirants",
                     "Num. aspirants from non-core ethnic groups",
                     "Num. aspirants from party's core ethnic groups",
                     "Num. non-core female aspirants",
                     "Total number of aspirants",
                     "Nominee is female",
                     "Nominee belongs to party's core ethnic group",
                     "Nominee is female, non-core group member",
                     "Nominee has only private sector background",
                     "Nominee is the incumbent")
row.names(pre.match.balance) <- outcomes.english                                           
row.names(prop.score.balance) <- outcomes.english                                           


x <- cbind(pre.match.balance, tab.n.treat, prop.score.balance, tab.n.treat.em, num.sets.em)
names(x)[3] <- "p-value"
names(x)[4] <- "nT"
names(x)[7] <- "p-value"
names(x)[8] <- "nT"
names(x)[9] <- "Sets"

write.csv(round(x,2), file=c("../2_output/2_tables/tab1_balance.main.csv"))

tab.balance.main <- xtable(x,
                           label=c("tab:balance.main"),
                           align=c("l", "r", "r","r","r","r","r","r","r","r"),
                           digits=c(1,2,0,2, 0,2,0,2,0,0))
print.xtable(tab.balance.main, caption.placement=c("top"), 
             file=c("../2_output/2_tables/tab1_balance.main.tex"))
