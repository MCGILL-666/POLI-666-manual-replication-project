###################################################################
## tableA1.R
## 
###################################################################

rm(list=ls())

library(stargazer)

load("../0_data/prim.dat.Rdata")

vars.2016 <- c("num_aspirants_total.2016",
               "female.sum.2016",
               "numasp_notowngroup_female.sum.2016", 
               "cand_owngroups.sum.2016",
               "not_owngroups.sum.2016",
               "female.nominee.2016", ## change order
               "notowngroup.female.nominee.2016", 
               "private_sector.nominee.2016", 
               "incumbent.nominee.2016",
               "owngroup.nominee.2016",
               "own2012_parl_p", 
               "own2012_pres_p", 
               "ethfrac_ownparty",
               "seg_acrossparty", 
               "seg_ownparty",
               "popdens",
               "const_largest_eth_group_p",
               "const_muslim_p")
vars.2012 <- gsub(".2016", ".2012", vars.2016[c(1:10)])


vars.2016.english <- c("Total number of 2016 aspirants",
                       "Total number of 2016 female aspirants",
                       "Number of 2016 non-core female aspirants",
                       "Num.~2016 aspirants from party's core ethnic groups",
                       "Num.~2016 aspirants from non-core ethnic groups",
                       "2016 nominee is female",
                       "2016 nominee is non-core and female",
                       "2016 nominee has only private sector background",
                       "2016 nominee is the incumbent",
                       "2016 nominee belongs to party's core ethnic group",
                       "Vote share in 2012 parliamentary election",
                       "Vote share in 2012 presidential election",
                       "Fractionalization of party's core ethnic groups", 
                       "Segregation of party's core groups from other groups",
                       "Segregation among party's core groups", 
                       "Population density of constituency (log(1000s per sq km))",
                       "Population share of largest ethnic group in constituency",
                       "Muslim population share in constituency"
)


vars.2012.english <- c("Total number of 2012 aspirants",
                       "Total number of 2012 female aspirants",
                       "Total number of 2012 non-core female aspirants",
                       "Num.~2012 aspirants from party's core ethnic groups",
                       "Num.~2012 aspirants from non-core ethnic groups",
                       "2012 nominee is female",
                       "2012 nominee is non-core and female",
                       "2012 nominee has only private sector background",
                       "2012 nominee is the incumbent",
                       "2012 nominee belongs to party's core ethnic group"
)

## NDC only
stargazer(subset(prim.dat[c(vars.2016[1:10], vars.2012, vars.2016[11:15])], 
                 prim.dat$party=="NDC"), 
          covariate.labels =c(vars.2016.english[1:10], vars.2012.english, 
                              vars.2016.english[11:15]),
          header=FALSE, column.sep.width="3pt", digits=2, 
          summary.stat=c("n", "mean", "sd", "min", "max"),
          out="../2_output/2_tables/tabA1_NDCsumstat.tex")
stargazer(subset(prim.dat[c("ethnicity.p.inc.2016")], 
                 prim.dat$party=="NDC"&prim.dat$holds_seat.2016==1),
          covariate.labels=c("2016 Incumbent's ethnic group share"),
          header=FALSE, column.sep.width="3pt", digits=2, 
          summary.stat=c("n", "mean", "sd", "min", "max"),
          out="../2_output/2_tables/tabA1_addtoNDCsumstat1.tex")

## NPP only + constituency-level variables
stargazer(subset(prim.dat[c(vars.2016[1:10], vars.2012, vars.2016[11:18])], 
                 prim.dat$party=="NPP"), 
          covariate.labels =c(vars.2016.english[1:10], vars.2012.english, 
                              vars.2016.english[11:18]),
          header=FALSE, column.sep.width="3pt", digits=2, 
          summary.stat=c("n", "mean", "sd", "min", "max"),
          out="../2_output/2_tables/tabA1_NPPsumstat.tex")
stargazer(subset(prim.dat[c("ethnicity.p.inc.2016")], 
                 prim.dat$party=="NPP"&prim.dat$holds_seat.2016==1),
          covariate.labels=c("2016 Incumbent's ethnic group share"),
          header=FALSE, column.sep.width="3pt", digits=2, 
          summary.stat=c("n", "mean", "sd", "min", "max"),
          out="../2_output/2_tables/tabA1_addtoNPPsumstat1.tex")

## combine these manually to produce summary statistics table
