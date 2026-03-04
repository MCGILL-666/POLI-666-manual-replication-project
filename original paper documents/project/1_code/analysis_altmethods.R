###################################################################
## analysis_altmethods.R
##
## this contains: results for each outcome variable (for each method; 
## standardized differences in matching variables pre and post
## matching) + table of results by outcome various methods)
##
## the methods are all 1:1
## for 3-6: with and without caliper (see below for more specifics 
## on caliper)
##
## 1 - genetic matching with exact match on 2012 outcome
## 2 - genetic matching without exact match on 2012 outcome
## 3 - mahalanobis distance matching with exact match on 2012 outcome
## 4 - mahalanobis distance matching without exact match on 2012 outcome
## 5 - propensity score matching with exact match on 2012 outcome
## 6 - propensity score  matching without exact match on 2012 outcome
#####################################################################


rm(list=ls())
library(Matching)
library(rgenoud)

load("../0_data/prim.dat.Rdata")

outcomes.2016 <- c("not_owngroups.sum.2016",  
                   "cand_owngroups.sum.2016",
                   "female.sum.2016",
                   "num_aspirants_total.2016",
                   "owngroup.nominee.2016",
                   "female.nominee.2016",
                   "private_sector.ONLY.nominee.2016", 
                   "incumbent.nominee.2016")

outcomes.2016.english <- c("Num.Asp. from Non-Associated Ethnic Groups",
                           "Num.Asp. from Party-Associated Ethnic Groups",
                           "Num. Female Aspirants",
                           "Total Number of Aspirants",
                           "Nominee is a Party-Associated Ethnic Group Member",
                           "Nominee is Female",
                           "Nominee has Private Sector Background",
                           "Nominee is the Incumbent")

outcomes.2012 <- gsub(".2016", ".2012", outcomes.2016)



#####################################################################
## alternative matching methods, all 1:1
## 1 - genetic matching with exact match on 2012 outcome
## 2 - genetic matching without exact match on 2012 outcome
## 3 - mahalanobis distance matching with exact match on 2012 outcome
## 4 - mahalanobis distance matching without exact match on 2012 outcome
## 5 - propensity score matching with exact match on 2012 outcome
## 6 - propensity score matching without exact match on 2012 outcome
#####################################################################

## different sets of matching variables for different outcomes
mv1 <- c("constparty.id", "own2012_parl_p", "own2012_pres_p", "ethfrac_ownparty", "popdens")
mv2 <- c(mv1, "const_muslim_p")
mv3 <- c(mv1, "const_largest_eth_group_p_NAMELEVELS", "seg_acrossparty", "seg_ownparty")
mv4 <- c(mv1, "ethnicity.p.inc.2016")
mv5 <- c(mv1, "const_muslim_p", "female.sum.2012")
mv6 <- c(mv1, "const_owngroups_p", "seg_acrossparty", "num_aspirants_total.2012")

matchvarlist <- c("mv3", "mv3", "mv2", "mv1",   
                  "mv6", "mv5",  "mv1", "mv4")

for(i in 1:length(outcomes.2016)){
  # subset data to complete cases for each outcome
  if(outcomes.2016[i]=="incumbent.nominee.2016"){
    dat1 <- prim.dat[prim.dat$holds_seat.2016==1,c("ndc", outcomes.2016[i], outcomes.2012[i], get(matchvarlist[i]))]
    dat1 <- dat1[complete.cases(dat1),]
  } else{
    dat1 <- prim.dat[,c("ndc", outcomes.2016[i], outcomes.2012[i], get(matchvarlist[i]))]
    dat1 <- dat1[complete.cases(dat1),]
  }
  
  set.seed(201906+2*i)
  
  ## genetic matching 1:1 with exact match on 2012 outcome -----
  genout1 <- GenMatch(Tr=dat1[,1], X=dat1[,-c(1,2,4)], BalanceMatrix=dat1[,-c(1,2,4)], 
                     estimand="ATT", M=1, exact=c(1,rep(0,ncol(dat1)-4)), 
                     pop.size=100, max.generations=100, wait.generations=10)
  mout1 <- Match(Y=dat1[,2], Tr=dat1[,1], X=dat1[,-c(1,2,4)], Z=dat1[,-c(1,2,4)], 
                 estimand="ATT", BiasAdjust = TRUE, Weight.matrix=genout1)
  mb1 <- MatchBalance(as.formula(paste(names(dat1)[1], "~", paste(names(dat1[-c(1,2,4)]), collapse="+"))), match.out=mout1, nboots=1000, data=dat1)

  ## genetic matching 1:1 without exact match on 2012 outcome -----
  genout2 <- GenMatch(Tr=dat1[,1], X=dat1[,-c(1,2,4)], BalanceMatrix=dat1[,-c(1,2,4)], 
                     estimand="ATT", M=1,  
                     pop.size=100, max.generations=100, wait.generations=10)
  mout2 <- Match(Y=dat1[,2], Tr=dat1[,1], X=dat1[,-c(1,2,4)], Z=dat1[,-c(1,2,4)],
                 estimand="ATT", BiasAdjust = TRUE, Weight.matrix=genout2)
  mb2 <- MatchBalance(as.formula(paste(names(dat1)[1], "~", paste(names(dat1[-c(1,2,4)]), collapse="+"))), match.out=mout2, nboots=1000, data=dat1)
                      
  ## Mahalanobis 1:1 matching with exact match on 2012 outcome-----
  mout3 <- Match(Y=dat1[,2], Tr=dat1[,1], X=dat1[,-c(1,2,4)], Z=dat1[,-c(1,2,4)], 
                 estimand="ATT", M=1, BiasAdjust = TRUE, replace=TRUE, sample=FALSE,
                 exact=c(1,rep(0,ncol(dat1)-4)), ties=TRUE) 
  ## caliper: all matches not equal to or within 1 sd of each covariate dropped
  mout3.caliper <- Match(Y=dat1[,2], Tr=dat1[,1], X=dat1[,-c(1,2,4)], Z=dat1[,-c(1,2,4)], 
                         estimand="ATT", M=1, BiasAdjust = TRUE, replace=TRUE, sample=FALSE,
                         exact=c(1,rep(0,ncol(dat1)-4)), ties=TRUE, caliper=1) 
  mb3 <- MatchBalance(as.formula(paste(names(dat1)[1], "~", paste(names(dat1[-c(1,2,4)]), collapse="+"))), match.out=mout3, nboots=1000, data=dat1)  
  mb3.caliper <- MatchBalance(as.formula(paste(names(dat1)[1], "~", paste(names(dat1[-c(1,2,4)]), collapse="+"))), match.out=mout3.caliper, nboots=1000, data=dat1)  
  
  ## Mahalanobis 1:1 matching without exact match on 2012 outcome-----
  mout4 <- Match(Y=dat1[,2], Tr=dat1[,1], X=dat1[,-c(1,2,4)], Z=dat1[,-c(1,2,4)], 
                 estimand="ATT", M=1, BiasAdjust = TRUE, replace=TRUE, sample=FALSE,
                 ties=TRUE)  
  ## caliper: all matches not equal to or within 1 sd of each covariate dropped
  mout4.caliper <- Match(Y=dat1[,2], Tr=dat1[,1], X=dat1[,-c(1,2,4)], 
                         estimand="ATT", M=1, BiasAdjust = TRUE, replace=TRUE, sample=FALSE,
                         caliper=1, ties=TRUE)  
  mb4 <- MatchBalance(as.formula(paste(names(dat1)[1], "~", paste(names(dat1[-c(1,2,4)]), collapse="+"))), match.out=mout4, nboots=1000, data=dat1)  
  mb4.caliper <- MatchBalance(as.formula(paste(names(dat1)[1], "~", paste(names(dat1[-c(1,2,4)]), collapse="+"))), match.out=mout4.caliper, nboots=1000, data=dat1)   
  
  ## propensity score matching 1:1 with exact match on 2012 outcome ----
  pscore5 <- glm(as.formula(paste(names(dat1)[1], "~", paste(names(dat1)[-c(1,2,3,4)], collapse="+"))), 
                                 family=binomial(logit), data=dat1)
  mout5 <- Match(Y=dat1[,2], Tr=dat1[,1], X=cbind(fitted(pscore5), dat1[,3]), 
                 estimand="ATT", exact = c(0,1), M=1, BiasAdjust = TRUE, 
                 replace=TRUE, sample=FALSE, ties=TRUE) 
  mout5.caliper <- Match(Y=dat1[,2], Tr=dat1[,1], X=cbind(fitted(pscore5), dat1[,3]), 
                 estimand="ATT", exact = c(0,1), M=1, caliper=.25, 
                 replace=TRUE, sample=FALSE, ties=TRUE) 
  mb5 <- MatchBalance(as.formula(paste(names(dat1)[1], "~", paste(names(dat1[-c(1,2,4)]), collapse="+"))), match.out=mout5, nboots=1000, data=dat1)  
  mb5.caliper <- MatchBalance(as.formula(paste(names(dat1)[1], "~", paste(names(dat1[-c(1,2,4)]), collapse="+"))), match.out=mout5.caliper, nboots=1000, data=dat1)  
  
  ## propensity score matching 1:1 without exact match on 2012 outcome ----
  pscore6 <- glm(as.formula(paste(names(dat1)[1], "~", paste(names(dat1)[-c(1,2,4)], collapse="+"))), 
                family=binomial(logit), data=dat1)
  mout6 <- Match(Y=dat1[,2], Tr=dat1[,1], X=fitted(pscore6), 
                 estimand="ATT", M=1, BiasAdjust = TRUE, 
                 replace=TRUE, sample=FALSE, ties=TRUE) 
  mout6.caliper <- Match(Y=dat1[,2], Tr=dat1[,1], X=fitted(pscore6), 
                          estimand="ATT", M=1, caliper=.25,
                          replace=TRUE, sample=FALSE, ties=TRUE) 
  mb6 <- MatchBalance(as.formula(paste(names(dat1)[1], "~", paste(names(dat1[-c(1,2,4)]), collapse="+"))), match.out=mout6, nboots=1000, data=dat1)  
  mb6.caliper <- MatchBalance(as.formula(paste(names(dat1)[1], "~", paste(names(dat1[-c(1,2,4)]), collapse="+"))), match.out=mout6.caliper, nboots=1000, data=dat1)  
  
  ## create balance tables -- before matching and after matching standardized differences
  mblist <- c("mb1", "mb2", "mb3", "mb3.caliper", "mb4", "mb4.caliper", 
              "mb5", "mb5.caliper", "mb6", "mb6.caliper")
  mblist.names <- c("gen.exact", "gen.noexact", 
                    "mah.exact", "mah.exact.caliper", 
                    "mah.noexact", "mah.noexact.caliper",
                    "ps.exact", "ps.exact.caliper", 
                    "ps.noexact", "ps.noexact.caliper")
  for(k in 1:length(mblist)){
    x <- matrix(NA, nrow=(dim(dat1)[2]-3), ncol=2)
    for(j in 1:(dim(dat1)[2]-3)){
      x[j,1] <- eval(parse(text=paste(mblist[k],"$BeforeMatching[", j, "][[1]]$sdiff.pooled", sep="")))/100
      x[j,2] <- eval(parse(text=paste(mblist[k],"$AfterMatching[", j, "][[1]]$sdiff.pooled", sep="")))/100
    }
    rownames(x) <- names(dat1[-c(1,2,4)])
    colnames(x) <- c("before", "after")
    assign(paste("tab.balance.sdiff", mblist.names[k], sep="."), x)  
  }
  
  ## save outputs in a list
  holder <- list()
  
  holder$gen.exact.genout <- genout1
  holder$gen.exact <- mout1
  holder$gen.exact.balance <- mb1
  
  holder$gen.noexact.genout <- genout2
  holder$gen.noexact <- mout2
  holder$gen.noexact.balance <- mb2
  
  holder$mah.exact <- mout3
  holder$mah.exact.balance <- mb3
  holder$mah.exact.caliper <- mout3.caliper
  holder$mah.exact.caliper.balance <- mb3.caliper
  
  holder$mah.noexact <- mout4
  holder$mah.noexact.balance <- mb4
  holder$mah.noexact.caliper <- mout4.caliper
  holder$mah.noexact.caliper.balance <- mb4.caliper
  
  holder$ps.exact.scores <- pscore5
  holder$ps.exact <- mout5
  holder$ps.exact.balance <- mb5
  holder$ps.exact.caliper <- mout5.caliper
  holder$ps.exact.caliper.balance <- mb5.caliper
  
  holder$ps.noexact.scores <- pscore6
  holder$ps.noexact <- mout6
  holder$ps.noexact.balance <- mb6
  holder$ps.noexact.caliper <- mout6.caliper
  holder$ps.noexact.caliper.balance <- mb6.caliper

  holder$tab.balance.sdiff.gen.exact <- tab.balance.sdiff.gen.exact
  holder$tab.balance.sdiff.gen.noexact <- tab.balance.sdiff.gen.noexact
  holder$tab.balance.sdiff.mah.exact <- tab.balance.sdiff.mah.exact
  holder$tab.balance.sdiff.mah.exact.caliper <- tab.balance.sdiff.mah.exact.caliper
  holder$tab.balance.sdiff.mah.noexact <- tab.balance.sdiff.mah.noexact
  holder$tab.balance.sdiff.mah.noexact.caliper <- tab.balance.sdiff.mah.noexact.caliper
  holder$tab.balance.sdiff.ps.exact <- tab.balance.sdiff.ps.exact
  holder$tab.balance.sdiff.ps.exact.caliper <- tab.balance.sdiff.ps.exact.caliper
  holder$tab.balance.sdiff.ps.noexact <- tab.balance.sdiff.ps.noexact
  holder$tab.balance.sdiff.ps.noexact.caliper <- tab.balance.sdiff.ps.noexact.caliper
  
  ## name the holder
  assign(paste("result", outcomes.2016[i], "altmethods", sep="."), holder)
}  

## organize results

mainlist <- list(result.female.sum.2016.altmethods, 
                 result.num_aspirants_total.2016.altmethods,
                 result.female.nominee.2016.altmethods,
                 result.not_owngroups.sum.2016.altmethods, 
                 result.cand_owngroups.sum.2016.altmethods,  
                 result.owngroup.nominee.2016.altmethods, 
                 result.private_sector.ONLY.nominee.2016.altmethods,  
                 result.incumbent.nominee.2016.altmethods) 

outcomes.2016.english <- c("Num.Female Aspirants",
                           "Total Number of Aspirants",
                           "Nominee is Female",
                           "Num.Asp. from Non-Associated Ethnic Groups",
                           "Num.Asp. from Party-Associated Ethnic Groups",
                           "Nominee is a Party-Associated Ethnic Group Member",
                           "Nominee has Private Sector Background",
                           "Nominee is the Incumbent")

outcomes.2016 <- c("female.sum.2016",
                   "num_aspirants_total.2016",
                   "female.nominee.2016",
                   "not_owngroups.sum.2016",
                   "cand_owngroups.sum.2016",
                   "owngroup.nominee.2016",
                   "private_sector.ONLY.nominee.2016",
                   "incumbent.nominee.2016")

methods <- c("gen.exact", "gen.noexact", 
            "mah.exact", "mah.noexact", 
            "mah.exact.caliper", "mah.noexact.caliper", 
            "ps.exact", "ps.noexact", 
            "ps.exact.caliper", "ps.noexact.caliper")

ends <- c("est", "se", "orig.treated.nobs", "ndrops.matches")

for(i in 1:length(outcomes.2016)){
  x<- matrix(NA, nrow=length(methods), ncol=(length(ends)+1))
  for(j in 1:length(methods)){
    for(k in 1:length(ends)){
      x[j,k] <- eval(parse(text=paste("mainlist[[i]]$",methods[j],"$",ends[k], sep="")))
    }
    
  }
  rownames(x) <- methods
  x[,5] <- x[,3] - x[,4]
  x <- x[,c(1,2,3,5)]
  colnames(x) <- c(ends[1:3], "final.treated.nobs")
  assign(paste("tab", outcomes.2016[i], "altmethods", sep="."), x)
}


## save
save(result.female.sum.2016.altmethods, 
     result.not_owngroups.sum.2016.altmethods, 
     result.cand_owngroups.sum.2016.altmethods,  
     result.num_aspirants_total.2016.altmethods,
     result.female.nominee.2016.altmethods,
     result.owngroup.nominee.2016.altmethods,
     result.private_sector.ONLY.nominee.2016.altmethods,  
     result.incumbent.nominee.2016.altmethods,
     tab.not_owngroups.sum.2016.altmethods,
     tab.cand_owngroups.sum.2016.altmethods, 
     tab.num_aspirants_total.2016.altmethods,
     tab.female.sum.2016.altmethods,
     tab.owngroup.nominee.2016.altmethods,
     tab.female.nominee.2016.altmethods, 
     tab.incumbent.nominee.2016.altmethods,
     tab.private_sector.ONLY.nominee.2016.altmethods,
     file="../2_output/0_results/results.altmethods.RData")


