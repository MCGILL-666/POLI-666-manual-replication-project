###################################################################
## tableB3.R
##
###################################################################

rm(list=ls())

load("../2_output/0_results/results.RData")
load("../2_output/0_results/results.comp.RData") # from analysis_by_competitiveness.R

temp1 <- cbind(effect.size.lm[, c(1,2,4)], tab.n.treat.em, num.sets.em)
temp2 <- rbind(temp1[1,], 
               tab.female.sum.2016.comp.bal[,c(1,2,4:6)], 
               temp1[2,], 
               tab.not_owngroups.sum.2016.comp.bal[,c(1,2,4:6)],
               temp1[3,], 
               tab.cand_owngroups.sum.2016.comp.bal[,c(1,2,4:6)], 
               temp1[4,], 
               tab.numasp_notowngroup_female.sum.2016.comp.bal[,c(1,2,4:6)],
               temp1[5,], 
               tab.num_aspirants_total.2016.comp.bal[,c(1,2,4:6)], 
               temp1[6,], 
               tab.female.nominee.2016.comp.bal[,c(1,2,4:6)], 
               temp1[7,], 
               tab.owngroup.nominee.2016.comp.bal[,c(1,2,4:6)],
               temp1[8,], 
               tab.female.nominee.2016.comp.bal[,c(1,2,4:6)],
               temp1[9,], 
               tab.private_sector.ONLY.nominee.2016.comp.bal[,c(1,2,4:6)], 
               temp1[10,], 
               tab.incumbent.nominee.2016.comp.bal[c(2:3),c(1,2,4:6)])
colnames(temp2) <- c("Estimate", "S.E.", "$p$-value", "n_T", "Sets")
rownames(temp2) <- c(rep(c("Full data", "Noncomp", "Comp", "Strong"), 9), "Full data", "Comp", "Strong")

write.csv(round(temp2, 2), file="../2_output/2_tables/tabB3_hetcomp.csv")

tab <- xtable(temp2, 
              caption=c("Heterogeneous Effects by Constituency Competitiveness"), 
              label=c("tab:results.HETCOMP"), align=c("l", "r", "r", "r", "r", "r"),
              digits=c(0,2,2,2,0,0))

print.xtable(tab, caption.placement=c("top"), 
             file="../2_output/2_tables/tabB3_hetcomp.tex")





