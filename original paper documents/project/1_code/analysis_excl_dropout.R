###################################################################
## analysis_excl_dropout.R
## 
## output used to produce figure B2
###################################################################

rm(list=ls())

library(optmatch)
library(RItools)

load("../0_data/prim.dat.Rdata")

outcomes.2016 <- c("not_owngroups.sum.minusdropout.2016", 
                   "female.sum.minusdropout.2016",
                   "num_aspirants_total.minusdropout.2016")

outcomes.2016.english <- c("Num.Asp. from Non-Associated Ethnic Groups (excl. dropouts)",
                           "Num.Female Aspirants (excl. dropouts)",
                           "Total Number of Aspirants (excl. dropouts")
                           
outcomes.2012 <- c("not_owngroups.sum.2012",  
                   "female.sum.2012",
                   "num_aspirants_total.2012")

#####################################################################
## main specification: 
## 1) before matching
## 2) optimal full matching with exact matches on previous outcome 
## and propensity score from other matching vars
#####################################################################

## different sets of matching variables for different outcomes
mv1 <- c("constparty.id", "own2012_parl_p", "own2012_pres_p", "ethfrac_ownparty", "popdens")
mv2 <- c(mv1, "const_muslim_p")
mv3 <- c(mv1, "const_largest_eth_group_p_NAMELEVELS", "seg_acrossparty", "seg_ownparty")
matchvarlist <- c("mv3", "mv2", "mv1")

for(i in c(1:3)){ 
  dat1 <- prim.dat[,c("ndc", outcomes.2016[i], outcomes.2012[i], get(matchvarlist[i]))]
  dat1 <- dat1[complete.cases(dat1),]
  
  ## linear model (before matching)
  lm1 <- lm(as.formula(paste(names(dat1)[2], "~", paste(names(dat1)[-c(2,4)], 
                                                        collapse=" + "))), data=dat1)
  ## pre-matching balance
  balance.pre <- xBalance(
    as.formula(paste(names(dat1)[1], "~", paste(names(dat1)[-c(1,2,4)], collapse=" + "))), 
    data = dat1, report=c("chisquare.test", "std.diffs", "z.scores", "p.values"))
  
  ## propensity scores
  glm1 <- glm(as.formula(paste(names(dat1)[1], "~", paste(names(dat1)[-c(1,2,4)], 
                                                          collapse=" + "))), family=binomial(), data=dat1)
  ## distance
  ps.dist1 <- match_on(glm1, data=dat1)
  
  ## full matching with exact matches on lagged outcome
  em <- fullmatch(ps.dist1 + exactMatch(dat1[,1] ~ dat1[,3], data=dat1), data=dat1) 
  balance <- xBalance(as.formula(paste(names(dat1)[1], "~", 
                                       paste(names(dat1)[-c(1,2,3,4)], collapse="+"), "+ strata(em)")), 
                      data=dat1, 
                      report = c("chisquare.test", "std.diffs", "z.scores", "p.values"))
  
  ## pull together data for effect estimates
  dat1a <- cbind(dat1[,1:2], em, fitted(glm1))
  names(dat1a) <- c("ndc", "y", "em", "propscore")
  
  ## estimate for full matching with exact matches on lagged outcome
  ## (within-block differences in means, with ETT weights)
  dat <- cbind(aggregate(y ~ em, dat=dat1a[dat1a$ndc==1,], FUN=mean), 
               aggregate(y ~ em, dat=dat1a[dat1a$ndc==0,], FUN=mean)[,2])
  names(dat) <- c("em", "meanYT", "meanYC")
  dat$diff <- dat[,2]-dat[,3]
  dat$weights <- table(em, ifelse(dat1a$ndc, "treated", "control"))[,2]/
    sum(table(em, ifelse(dat1a$ndc, "treated", "control"))[,2])
  estimate <- sum(dat$diff*dat$weights) 
  
  ## calculate weights for full matching
  x <- as.matrix(table(dat1a$em, dat1a$ndc))
  colnames(x) <- c("npp", "ndc")
  x <- cbind(x, rowSums(x))
  colnames(x) <- c("npp", "ndc", "blocksize")
  x <- as.data.frame(x)
  x$em <- row.names(x)
  x$ndc.weight <- 1
  x$npp.weight <- x$ndc/x$npp
  x.ndc <- x[,4:5]
  x.ndc$ndc <- 1
  names(x.ndc) <- c("em", "weight", "ndc")
  x.npp <- x[,c(4,6)]
  x.npp$ndc <- 0
  names(x.npp) <- names(x.ndc)
  x <- rbind(x.ndc, x.npp)
  dat.weights <- merge(dat1a, x, by=c("em", "ndc"))
  
  ## estimates and s.e. from model
  lm.main <- lm(y ~ ndc + em, data=dat.weights, weights=weight)
  
  ## save outputs in a list
  holder <- list()
  
  holder$prop.scores <- glm1
  holder$prop.scores.dist <- ps.dist1
  
  holder$balance.pre <- balance.pre
  holder$lm.pre <- lm1
  holder$lm.est.pre <- summary(lm1)$coeff[paste(names(dat1)[1]),]
  holder$lm.confint.pre.95 <- confint(lm1, 2, level=.95) 
  holder$lm.confint.pre.90 <- confint(lm1, 2, level=.90) 
  
  holder$em <- em
  holder$balance <- balance
  holder$estimate <- estimate
  holder$lm <- lm.main
  holder$lm.est <- summary(lm.main)$coeff["ndc",]
  holder$lm.confint.95 <- confint(lm.main, 2, level=.95)
  holder$lm.confint.90 <- confint(lm.main, 2, level=.90)
  
  holder$dat.weights <- dat.weights
  
  ## name the holder
  assign(paste("result", outcomes.2016[i], "main", sep="."), holder)
}  

## save
save(result.num_aspirants_total.minusdropout.2016.main,
     result.female.sum.minusdropout.2016.main,
     result.not_owngroups.sum.minusdropout.2016.main,
     file="../2_output/0_results/results.minusdropout.RData")
