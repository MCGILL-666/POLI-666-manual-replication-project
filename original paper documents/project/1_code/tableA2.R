###################################################################
## tableA2.R
## 
###################################################################

rm(list=ls())

library(stargazer)

load("../0_data/prim.dat.Rdata")

dat1 <- prim.dat[, c("num_aspirants_total.2016", "num_aspirants_total.2012", 
                     "female.sum.2012", "not_owngroups.sum.2012", 
                     "female.nominee.2012", "incumbent.nominee.2012", 
                     "private_sector.ONLY.nominee.2012", 
                     "own2012_parl_p", "own2012_pres_p", "ethfrac_ownparty", 
                     "const_muslim_p", "popdens")]
dat1$missing <- is.na(dat1$num_aspirants_total.2016)
x <- dim(dat1)[2]
dat1 <- dat1[,2:x]
tab <- matrix(NA, nrow=(x-2), ncol=2)
for(i in 1:(x-2)){
  tab[i,1] <- t.test(dat1[,i] ~ dat1$missing)[[5]][2] -
    t.test(dat1[,i] ~ dat1$missing)[[5]][1]
  tab[i,2] <- t.test(dat1[,i] ~ dat1$missing)[[3]]
}

tab <- round(tab, 2)
colnames(tab) <- c("Differences in means (missing -- non-missing)", "p-value")
rownames(tab) <- c("Total number of aspirants, 2012", "Number of female aspirants, 2012", 
                   "Num. aspirants from non-core groups, 2012", "Female nominee, 2012", 
                   "Incumbent was nominee, 2012", 
                   "Private-sector only nominee, 2012", 
                   "2012 parliamentary vote share", "2012 presidential vote share", 
                   "Ethnic fractionalization of core groups", 
                   "Muslim % (constituency)", "Population density (constituency)")
tab2 <- xtable(tab, caption=c("Comparing missing to non-missing party-constituencies"), 
               label=c("tab:missing"), 
               align=c("l", "r", "r"), 
               digits=c(0, 2,2))

write.csv(round(tab,2), file=c("../2_output/2_tables/tabA2_missing.csv"))

print.xtable(tab2, caption.placement=c("top"), 
             file=c("../2_output/2_tables/tabA2_missing.tex"))
