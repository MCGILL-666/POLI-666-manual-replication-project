###################################################################
## analysis_without_em.R
##
## analysis without exact matching on the 2012 outcome
## output (results.noexact.RData) used to produce Table C6
###################################################################

rm(list=ls())
library(optmatch)
library(RItools)

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
## optimal full matching (same set up as main analysis), but 
## 1) including previous outcome in building the propensity score
## 2) excluding previous outcome from the model entirely
#####################################################################

## different sets of matching variables for different outcomes
mv1 <- c("constparty.id", "own2012_parl_p", "own2012_pres_p", "ethfrac_ownparty", "popdens")
mv2 <- c(mv1, "const_muslim_p")
mv3 <- c(mv1, "const_largest_eth_group_p_NAMELEVELS", "seg_acrossparty", "seg_ownparty")
mv4 <- c(mv1, "ethnicity.p.inc.2016")
mv5 <- c(mv1, "const_muslim_p", "female.sum.2012")
mv6 <- c(mv1, "const_owngroups_p", "seg_acrossparty", "num_aspirants_total.2012")
mv7 <- c(mv1, "const_muslim_p", "const_largest_eth_group_p_NAMELEVELS", "seg_acrossparty", "seg_ownparty")
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
  
  glm1 <- glm(as.formula(paste(names(dat1)[1], "~", paste(names(dat1)[-c(1,2,4)], 
                    collapse=" + "))), family=binomial(), data=dat1)
  ps.dist1 <- match_on(glm1, data=dat1)
  
  m1 <- fullmatch(ps.dist1, data=dat1) 
  balance1 <- xBalance(as.formula(paste(names(dat1)[1], "~", 
                      paste(names(dat1)[-c(1,2,4)], collapse="+"), "+ strata(m1)")), 
                      data=dat1, 
                      report = c("chisquare.test", "std.diffs", "z.scores", "p.values"))
  
  ## pull together data for effect estimates
  dat1a <- cbind(dat1[,1:2], m1, fitted(glm1))
  names(dat1a) <- c("ndc", "y", "m1", "propscore")
  
  ## number of treated units before and after optimal full matching procedures
  n.treat <- table(dat1a$ndc)[2]
  n.treat.m1 <- table(dat1a$ndc[1-is.na(dat1a$m1)==1])[2]
  
  
  ## estimate for full matching 
  ## (within-block differences in means, with ETT weights)
  dat <- cbind(aggregate(y ~ m1, dat=dat1a[dat1a$ndc==1,], FUN=mean), 
                  aggregate(y ~ m1, dat=dat1a[dat1a$ndc==0,], FUN=mean)[,2])
  names(dat) <- c("m1", "meanYT", "meanYC")
  dat$diff <- dat[,2]-dat[,3]
  dat$weights <- table(m1, ifelse(dat1a$ndc, "treated", "control"))[,2]/
    sum(table(m1, ifelse(dat1a$ndc, "treated", "control"))[,2])
  estimate <- sum(dat$diff*dat$weights) 
  
  ## calculate weights for full matching
  x <- as.matrix(table(dat1a$m1, dat1a$ndc))
  colnames(x) <- c("npp", "ndc")
  x <- cbind(x, rowSums(x))
  colnames(x) <- c("npp", "ndc", "blocksize")
  x <- as.data.frame(x)
  x$m1 <- row.names(x)
  x$ndc.weight <- 1
  x$npp.weight <- x$ndc/x$npp
  x.ndc <- x[,4:5]
  x.ndc$ndc <- 1
  names(x.ndc) <- c("m1", "weight", "ndc")
  x.npp <- x[,c(4,6)]
  x.npp$ndc <- 0
  names(x.npp) <- names(x.ndc)
  x <- rbind(x.ndc, x.npp)
  dat.weights <- merge(dat1a, x, by=c("m1", "ndc"))
  
  ## estimates and s.e. from models
  lm.m1 <- lm(y ~ ndc + m1, data=dat.weights, weights=weight)
  
  ## save outputs in a list
  holder <- list()
  
  holder$prop.scores <- glm1
  holder$prop.scores.dist <- ps.dist1
  
  holder$m1 <- m1
  holder$balance <- balance1
  holder$estimate <- estimate
  holder$lm <- lm.m1
  holder$lm.est <- summary(lm.m1)$coeff["ndc",]
  holder$lm.confint.95 <- confint(lm.m1, 2, level=.95)
  holder$lm.confint.90 <- confint(lm.m1, 2, level=.90)
  
  holder$m1.nobs.treated <- sum(dat1a$ndc)
  
  holder$dat.weights <- dat.weights
  
  holder$n.treat <- n.treat
  holder$n.treat.m1 <- n.treat.m1
  
  ## name the holder
  assign(paste("result", outcomes.2016[i], "fullopt.noexact", sep="."), holder)
}  



mainlist <- list(result.female.sum.2016.fullopt.noexact, 
                 result.num_aspirants_total.2016.fullopt.noexact,
                 result.female.nominee.2016.fullopt.noexact,
                 result.not_owngroups.sum.2016.fullopt.noexact, 
                 result.cand_owngroups.sum.2016.fullopt.noexact,  
                 result.owngroup.nominee.2016.fullopt.noexact,
                 result.private_sector.ONLY.nominee.2016.fullopt.noexact,  
                 result.incumbent.nominee.2016.fullopt.noexact) 


outcomes.2016.english <- c("Num.Female Aspirants",
                           "Total Number of Aspirants",
                           "Nominee is Female",
                           "Num.Asp. from Non-Associated Ethnic Groups",
                           "Num.Asp. from Party-Associated Ethnic Groups",
                           "Nominee is a Party-Associated Ethnic Group Member",
                           "Nominee has Private Sector Background",
                           "Nominee is the Incumbent")

## balance table
tab.balance<- list()
for(i in 1:length(outcomes.2016)){
  tab.balance[[i]] <- eval(parse(text=paste("mainlist[[i]]$", "balance$overall[2,]", sep="")))
}
tab.balance <- do.call(rbind,tab.balance)
rownames(tab.balance) <- outcomes.2016.english

## results table, with number of treated units remaining, and effective pairs 
tab.m1.est <- list()
for(i in 1:length(outcomes.2016)){
  tab.m1.est[[i]] <- eval(parse(text=paste("mainlist[[i]]$", "lm.est", sep="")))
}
tab.m1.est <- do.call(rbind,tab.m1.est)
rownames(tab.m1.est) <- outcomes.2016.english

## nobs treated
nobs.tr <- rep(NA, length(outcomes.2016))
for(i in 1:length(outcomes.2016)){
  nobs.tr[i] <- eval(parse(text="mainlist[[i]]$m1.nobs.treated"))
}

tab.m1.est <- cbind(tab.m1.est, nobs.tr)

## num.sets.m1
num.sets.m1 <- c(
  sum(summary(result.female.sum.2016.fullopt.noexact$m1)[[2]]), 
  sum(summary(result.num_aspirants_total.2016.fullopt.noexact$m1)[[2]]),
  sum(summary(result.female.nominee.2016.fullopt.noexact$m1)[[2]]),
  sum(summary(result.not_owngroups.sum.2016.fullopt.noexact$m1)[[2]]), 
  sum(summary(result.cand_owngroups.sum.2016.fullopt.noexact$m1)[[2]]),  
  sum(summary(result.owngroup.nominee.2016.fullopt.noexact$m1)[[2]]),
  sum(summary(result.private_sector.ONLY.nominee.2016.fullopt.noexact$m1)[[2]]),  
  sum(summary(result.incumbent.nominee.2016.fullopt.noexact$m1)[[2]])) 

## n.treat.m1
n.treat.m1 <- rep(NA, length(outcomes.2016))
for(i in 1:length(outcomes.2016)){
  n.treat.m1[i] <- eval(parse(text="mainlist[[i]]$n.treat.m1"))
}

## save results

save(result.female.sum.2016.fullopt.noexact, 
     result.not_owngroups.sum.2016.fullopt.noexact, 
     result.cand_owngroups.sum.2016.fullopt.noexact,  
     result.num_aspirants_total.2016.fullopt.noexact,
     result.female.nominee.2016.fullopt.noexact,
     result.owngroup.nominee.2016.fullopt.noexact,
     result.private_sector.ONLY.nominee.2016.fullopt.noexact,  
     result.incumbent.nominee.2016.fullopt.noexact,
     tab.m1.est,
     tab.balance,
     num.sets.m1,
     n.treat.m1,
     mainlist,
     file="../2_output/0_results/results.noexact.RData")





