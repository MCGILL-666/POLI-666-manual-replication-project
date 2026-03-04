###################################################################
## tableB1.R
##
###################################################################

rm(list=ls())

load("../2_output/0_results/results.RData") # from analysis.R
load("../2_output/0_results/results.comp.RData") # from analysis_by_competitiveness.R

temp1 <- cbind(effect.size.lm[, c(1,2,4)], tab.n.treat.em, num.sets.em)
temp2 <- rbind(temp1[1,], 
               tab.numasp_notowngroup_female.sum.2016.comp.bal[,c(1,2,4:6)],
               temp1[8,], 
               tab.notowngroup.female.nominee.2016.comp.bal[,c(1,2,4:6)])
colnames(temp2) <- c("Estimate", "S.E.", "$p$-value", "n_T", "Sets")
rownames(temp2) <- c(rep(c("Full data", "Noncomp", "Comp", "Strong"), 2))

write.csv(round(temp2, 2), file="../2_output/2_tables/tabB1_intersect.csv")

tab <- xtable(temp2, 
              caption=c("Heterogeneous Effects for Intersectional Outcomes 
                        by Constituency Competitiveness"), 
              label=c("tab:results.HETCOMP.intersect"), align=c("l", "r", "r", "r", "r", "r"),
              digits=c(0,2,2,2,0,0))

print.xtable(tab, caption.placement=c("top"), 
             file="../2_output/2_tables/tabB1_intersect.tex")





