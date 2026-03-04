###################################################################
## analysis_by_mpopshare.R
##
## estimated ATT on whether nominee is female by low/high
## Muslim population share
###################################################################

rm(list=ls())
library(optmatch)
library(RItools)

load("../0_data/prim.dat.Rdata")

outcomes.2016 <- c("female.nominee.2016", "female.nominee.2016")

outcomes.2016.english <- c("Nominee is Female (Low Muslim)", "Nominee is Female (High Muslim)")

outcomes.2012 <- gsub(".2016", ".2012", outcomes.2016)

median(na.omit(prim.dat$const_muslim_p))

vers <- c("belowmedian", "abovemedian")

for(i in 1:length(outcomes.2016)){
  if(outcomes.2016.english[i]=="Nominee is Female (Low Muslim)"){
    prim.dat2 <- prim.dat[prim.dat$const_muslim_p < median(na.omit(prim.dat$const_muslim_p)), ]
  }else{  
    ## subset the data
    prim.dat2 <- prim.dat[prim.dat$const_muslim_p >= median(na.omit(prim.dat$const_muslim_p)), ]
  }
  dat1 <- prim.dat2[,c("ndc", outcomes.2016[i], outcomes.2012[i], 
                       "constparty.id", "own2012_parl_p", "own2012_pres_p", 
                       "ethfrac_ownparty", "popdens", "const_muslim_p", "female.sum.2012")]
  dat1 <- dat1[complete.cases(dat1),]
  
  ## linear model (before matching)
  lm1 <- lm(as.formula(paste(names(dat1)[2], "~", paste(names(dat1)[-c(2,4)], collapse=" + "))), data=dat1)
  ## pre-matching balance
  balance.pre1 <- xBalance(
    as.formula(paste(names(dat1)[1], "~", paste(names(dat1)[-c(1,2,4)], collapse=" + "))), 
    data = dat1, report=c("chisquare.test", "std.diffs", "z.scores", "p.values"))
  
  ## propensity scores
  glm1 <- glm(as.formula(paste(names(dat1)[1], "~", paste(names(dat1)[-c(1,2,4)], collapse=" + "))), family=binomial(), data=dat1)
  ## distance
  ps.dist1 <- match_on(glm1, data=dat1)
  
  ## full matching with exact matches on lagged outcome
  em <- fullmatch(ps.dist1 + exactMatch(dat1[,1] ~ dat1[,3], data=dat1), data=dat1)
  balance.em <- xBalance(as.formula(paste(names(dat1)[1], "~", 
                  paste(names(dat1)[-c(1,2,3,4)], collapse="+"), "+ strata(em)")), 
                  data=dat1, report = c("chisquare.test", "std.diffs", "z.scores", "p.values"))
  
  ## estimate for full matching with exact matches on lagged outcome
  ## (within-block differences in means, with ETT weights)
  dat1a <- cbind(dat1[,1:2], em)
  names(dat1a) <- c("ndc", "y", "em")
  
  ## number of treated units before and after optimal full matching procedures
  n.treat <- table(dat1a$ndc)[2]
  n.treat.em <- table(dat1a$ndc[1-is.na(dat1a$em)==1])[2]
  
  dat.em <- cbind(aggregate(y ~ em, dat=dat1a[dat1a$ndc==1,], FUN=mean), 
                  aggregate(y ~ em, dat=dat1a[dat1a$ndc==0,], FUN=mean)[,2])
  names(dat.em) <- c("em", "meanYT", "meanYC")
  dat.em$diff <- dat.em[,2]-dat.em[,3]
  dat.em$weights <- table(em, ifelse(dat1a$ndc, "treated", "control"))[,2]/
    sum(table(em, ifelse(dat1a$ndc, "treated", "control"))[,2])
  estimate.em <- sum(dat.em$diff*dat.em$weights)
  
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
  names(x.ndc) <- c("em", "weight.em", "ndc")
  x.npp <- x[,c(4,6)]
  x.npp$ndc <- 0
  names(x.npp) <- names(x.ndc)
  x <- rbind(x.ndc, x.npp)
  dat.weights.em <- merge(dat1a, x, by=c("em", "ndc"))
  
  ## estimates and s.e. from models
  lm.em <- lm(y ~ ndc + em, data=dat.weights.em, weights=weight.em)
  
  ## save outputs in a list
  holder <- list()
  
  holder$lm1 <- lm1
  holder$lm.est <- summary(lm1)$coeff[paste(names(dat1)[1]),]
  holder$lm.confint <- confint(lm1, 2, level=.95) 
  holder$balance.pre <- balance.pre1
  holder$prop.scores <- glm1
  holder$prop.scores.dist <- ps.dist1
  
  holder$em <- em
  holder$balance.em <- balance.em
  holder$estimate.em <- estimate.em
  holder$lm.em <- lm.em
  holder$lm.est.em <- summary(lm.em)$coeff["ndc",]
  holder$lm.confint.em.95 <- confint(lm.em, 2, level=.95)
  holder$lm.confint.em.90 <- confint(lm.em, 2, level=.90)
  
  holder$n.treat <- n.treat 
  holder$n.treat.em <- n.treat.em 
  
  holder$dat.weights.em <- dat.weights.em
  
  ## name the holder
  assign(paste("matchingresult", outcomes.2016[i], vers[i], sep="."), holder)
}  


## Pre-Matching Balance
pre.match.balance <- rbind(
  matchingresult.female.nominee.2016.belowmedian$balance.pre$overall[1,],             
  matchingresult.female.nominee.2016.abovemedian$balance.pre$overall[1,])           

row.names(pre.match.balance) <- outcomes.2016.english


## Balance after Exact Matching with Prior Value of Outcome
prop.score.balance.em <- rbind(
  matchingresult.female.nominee.2016.belowmedian$balance.em$overall[2,],             
  matchingresult.female.nominee.2016.abovemedian$balance.em$overall[2,])             

row.names(prop.score.balance.em) <- outcomes.2016.english


## Effect Size Estimates 
effect.size.lm.em <- rbind(
  matchingresult.female.nominee.2016.belowmedian$lm.est.em,             
  matchingresult.female.nominee.2016.abovemedian$lm.est.em)            

row.names(effect.size.lm.em) <- outcomes.2016.english


## 95% CI on Effect Size Estimates 
effect.size.lm.em.ci95 <- rbind(
  matchingresult.female.nominee.2016.belowmedian$lm.confint.em.95,             
  matchingresult.female.nominee.2016.abovemedian$lm.confint.em.95)  
row.names(effect.size.lm.em.ci95) <- outcomes.2016.english


## save results
save(matchingresult.female.nominee.2016.belowmedian,
     matchingresult.female.nominee.2016.abovemedian,
     pre.match.balance, 
     prop.score.balance.em, 
     effect.size.lm.em, 
     effect.size.lm.em.ci95, 
     file="../2_output/0_results/results.mpopshare.RData")

