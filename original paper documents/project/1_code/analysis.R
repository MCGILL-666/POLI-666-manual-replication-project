###################################################################
## analysis.R
##
## main analysis file 
###################################################################

rm(list=ls())
library(optmatch)
library(RItools)

load("../0_data/prim.dat.Rdata")

outcomes.2016 <- c("not_owngroups.sum.2016",  
                   "cand_owngroups.sum.2016",
                   "female.sum.2016",
                   "num_aspirants_total.2016",
                   "numasp_notowngroup_female.sum.2016",
                   "owngroup.nominee.2016",
                   "female.nominee.2016",
                   "private_sector.ONLY.nominee.2016", 
                   "incumbent.nominee.2016",
                   "notowngroup.female.nominee.2016")

outcomes.2016.english <- c("Num.Asp. from Non-Associated Ethnic Groups",
                           "Num.Asp. from Party-Associated Ethnic Groups",
                           "Num. Female Aspirants",
                           "Total Number of Aspirants",
                           "Num. Female, Non-Core Aspirants",
                           "Nominee is a Party-Associated Ethnic Group Member",
                           "Nominee is Female",
                           "Nominee has Private Sector Background",
                           "Nominee is the Incumbent",
                           "Nominee is Female and Non-Core Group Member")

outcomes.2012 <- gsub(".2016", ".2012", outcomes.2016)


#####################################################################
## main specification: 
## 1) before matching
## 2) optimal full matching with exact matches on previous outcome 
## and propensity score from other matching vars
## 3) optimal full matching with exact matches on previous outcome 
## and propensity score from other matching vars, but restricted 
## to 10:1-1:10 (for the appendix)
## 4) pair matching (for the appendix)
#####################################################################

## different sets of matching variables for different outcomes
mv1 <- c("constparty.id", "own2012_parl_p", "own2012_pres_p", "ethfrac_ownparty", "popdens")
mv2 <- c(mv1, "const_muslim_p")
mv3 <- c(mv1, "const_largest_eth_group_p_NAMELEVELS", "seg_acrossparty", "seg_ownparty")
mv4 <- c(mv1, "ethnicity.p.inc.2016")
mv5 <- c(mv1, "const_muslim_p", "female.sum.2012")
mv6 <- c(mv1, "const_owngroups_p", "seg_acrossparty", "num_aspirants_total.2012")
mv7 <- c(mv1, "const_muslim_p", "const_largest_eth_group_p_NAMELEVELS", "seg_acrossparty", "seg_ownparty")
mv8 <- c(mv1, "const_muslim_p", "female.sum.2012", "const_owngroups_p", "seg_acrossparty", "num_aspirants_total.2012")
matchvarlist <- c("mv3", "mv3", "mv2", "mv1", "mv7",  
                  "mv6", "mv5",  "mv1", "mv4", "mv8" )

for(i in 1:length(outcomes.2016)){
  # subset data to complete cases for each outcome
  if(outcomes.2016[i]=="incumbent.nominee.2016"){
    dat1 <- prim.dat[prim.dat$holds_seat.2016==1,c("ndc", outcomes.2016[i], outcomes.2012[i], get(matchvarlist[i]))]
    dat1 <- dat1[complete.cases(dat1),]
  } else{
    dat1 <- prim.dat[,c("ndc", outcomes.2016[i], outcomes.2012[i], get(matchvarlist[i]))]
    dat1 <- dat1[complete.cases(dat1),]
  }
  
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
  
  ## optimal full matching with exact matches on lagged outcome, restricted to 1:10-10:1
  em.10 <- fullmatch(ps.dist1 + exactMatch(dat1[,1] ~ dat1[,3], data=dat1), data=dat1,
                     min=0.1, max.controls=10)
  balance.10 <- xBalance(as.formula(paste(names(dat1)[1], "~", 
                     paste(names(dat1)[-c(1:4)], collapse="+"), "+ strata(em.10)")), 
                     data=dat1, 
                     report = c("chisquare.test", "std.diffs", "z.scores", "p.values"))  

  ## optimal pair matching 
  pm <- pairmatch(ps.dist1 + exactMatch(dat1[,1] ~ dat1[,3], data=dat1), data=dat1, remove.unmatchables = TRUE) 
  balance.pm <- xBalance(as.formula(paste(names(dat1)[1], "~", 
                 paste(names(dat1)[-c(1,2,4)], collapse="+"), "+ strata(pm)")), 
                data=dat1, report = c("chisquare.test", "std.diffs", "z.scores", "p.values")) 
  
  ## pull together data for effect estimates
  dat1a <- cbind(dat1[,1:2], em, em.10, pm, fitted(glm1))
  names(dat1a) <- c("ndc", "y", "em", "em.10", "pm", "propscore")
  
  ## number of treated units before and after optimal full matching procedures
  n.treat <- table(dat1a$ndc)[2]
  n.treat.em <- table(dat1a$ndc[1-is.na(dat1a$em)==1])[2]
  n.treat.em.10 <- table(dat1a$ndc[1-is.na(dat1a$em.10)==1])[2]
  n.treat.pm <- table(dat1a$ndc[1-is.na(dat1a$pm)==1])[2]
  
  ## estimate for full matching with exact matches on lagged outcome
  ## (within-block differences in means, with ETT weights)
  dat <- cbind(aggregate(y ~ em, dat=dat1a[dat1a$ndc==1,], FUN=mean), 
                  aggregate(y ~ em, dat=dat1a[dat1a$ndc==0,], FUN=mean)[,2])
  names(dat) <- c("em", "meanYT", "meanYC")
  dat$diff <- dat[,2]-dat[,3]
  dat$weights <- table(em, ifelse(dat1a$ndc, "treated", "control"))[,2]/
    sum(table(em, ifelse(dat1a$ndc, "treated", "control"))[,2])
  estimate <- sum(dat$diff*dat$weights) 
  
  ## estimate for optimal full matching, restricted to 1:10-10:1
  ## (within-block differences in means, with ETT weights)
  dat.10 <- cbind(aggregate(y ~ em.10, dat=dat1a[dat1a$ndc==1,], FUN=mean), 
                     aggregate(y ~ em.10, dat=dat1a[dat1a$ndc==0,], FUN=mean)[,2])
  names(dat.10) <- c("em.10", "meanYT", "meanYC")
  dat.10$diff <- dat.10[,2]-dat.10[,3]
  dat.10$weights <- table(em.10, ifelse(dat1a$ndc, "treated", "control"))[,2]/
    sum(table(em.10, ifelse(dat1a$ndc, "treated", "control"))[,2])
  estimate.10 <- sum(dat.10$diff*dat.10$weights)
  
  ## estimate for pair matching
  dat.pm <- cbind(aggregate(y ~ pm, dat=dat1a[dat1a$ndc==1,], FUN=mean), 
                  aggregate(y ~ pm, dat=dat1a[dat1a$ndc==0,], FUN=mean)[,2])
  names(dat.pm) <- c("pm", "meanYT", "meanYC")
  dat.pm$diff <- dat.pm[,2]-dat.pm[,3]
  dat.pm$weights <- table(pm, ifelse(dat1a$ndc, "treated", "control"))[,2]/
    sum(table(pm, ifelse(dat1a$ndc, "treated", "control"))[,2])
  estimate.pm <- sum(dat.pm$diff*dat.pm$weights)
  
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

  ## calculate weights for optimal full matching with the 1:10-10:1 restriction
  x <- as.matrix(table(dat1a$em.10, dat1a$ndc))
  x <- cbind(x, rowSums(x))
  colnames(x) <- c("npp", "ndc", "blocksize")
  x <- as.data.frame(x)
  x$em.10 <- row.names(x)
  x$ndc.weight <- 1
  x$npp.weight <- x$ndc/x$npp
  x.ndc <- x[,4:5]
  x.ndc$ndc <- 1
  names(x.ndc) <- c("em.10", "weight.10", "ndc")
  x.npp <- x[,c(4,6)]
  x.npp$ndc <- 0
  names(x.npp) <- c("em.10", "weight.10", "ndc")
  x <- rbind(x.ndc, x.npp)
  dat.weights <- merge(dat.weights, x, by=c("em.10", "ndc"))
  
  ## calculate weights for pair matching
  x <- as.matrix(table(dat1a$pm, dat1a$ndc))
  x <- cbind(x, rowSums(x))
  colnames(x) <- c("npp", "ndc", "blocksize")
  x <- as.data.frame(x)
  x$pm <- row.names(x)
  x$ndc.weight <- 1
  x$npp.weight <- x$ndc/x$npp
  x.ndc <- x[,4:5]
  x.ndc$ndc <- 1
  names(x.ndc) <- c("pm", "weight.pm", "ndc")
  x.npp <- x[,c(4,6)]
  x.npp$ndc <- 0
  names(x.npp) <- c("pm", "weight.pm", "ndc")
  x <- rbind(x.ndc, x.npp)
  dat.weights <- merge(dat.weights, x, by=c("pm", "ndc"), all.x=TRUE) 
  
  ## for estimates and s.e. from models
  lm.main <- lm(y ~ ndc + em, data=dat.weights, weights=weight)
  lm.10 <- lm(y ~ ndc + em.10, data=dat.weights, weights=weight.10)
  lm.pm <- lm(y ~ ndc + pm, data=dat.weights, weights=weight.pm)
  
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
  
  holder$em.10 <- em.10
  holder$balance.10 <- balance.10
  holder$estimate.10 <- estimate.10
  holder$lm.10 <- lm.10  
  holder$lm.est.10 <- summary(lm.10)$coeff["ndc",]  
  holder$lm.confint.10.95 <- confint(lm.10, 2, level=.95)
  holder$lm.confint.10.90 <- confint(lm.10, 2, level=.90)
  
  holder$pm <- pm
  holder$balance.pm <- balance.pm
  holder$estimate.pm <- estimate.pm
  holder$lm.pm <- lm.pm
  holder$lm.est.pm <- summary(lm.pm)$coeff["ndc",]  
  holder$lm.confint.pm.95 <- confint(lm.pm, 2, level=.95)
  holder$lm.confint.pm.90 <- confint(lm.pm, 2, level=.90)
  
  holder$dat.weights <- dat.weights
  
  holder$n.treat <- n.treat 
  holder$n.treat.em <- n.treat.em 
  holder$n.treat.em.10 <- n.treat.em.10
  holder$n.treat.pm <- n.treat.pm
  
  ## name the holder
  assign(paste("result", outcomes.2016[i], "main", sep="."), holder)
}  




####################################################################
## Create a list of these outputs in the order we want to present them
####################################################################

mainlist <- list(result.female.sum.2016.main, 
                 result.not_owngroups.sum.2016.main, 
                 result.cand_owngroups.sum.2016.main,  
                 result.numasp_notowngroup_female.sum.2016.main,
                 result.num_aspirants_total.2016.main,
                 result.female.nominee.2016.main,
                 result.owngroup.nominee.2016.main,
                 result.notowngroup.female.nominee.2016.main,  
                 result.private_sector.ONLY.nominee.2016.main,  
                 result.incumbent.nominee.2016.main) 


outcomes.2016.english <- c("Num.Female Aspirants",
                           "Num.Asp. from Non-Associated Ethnic Groups",
                           "Num.Asp. from Party-Associated Ethnic Groups",
                           "Num.Non-Core Female Aspirants",
                           "Total Number of Aspirants",
                           "Nominee is Female",
                           "Nominee is a Party-Associated Ethnic Group Member",
                           "Nominee is Female Non-Core Group Member",
                           "Nominee has Private Sector Background",
                           "Nominee is the Incumbent")


####################################################################
## Tables of Results
####################################################################
## set 1: balance, lm est, confint
####################################################################
tables <- c("balance.pre", "balance", "balance.10", "balance.pm",
            "lm.est.pre", "lm.est", "lm.est.10", "lm.est.pm",
            "lm.confint.pre.95", "lm.confint.pre.90", 
            "lm.confint.95", "lm.confint.90", 
            "lm.confint.10.95", "lm.confint.10.90",
            "lm.confint.pm.95", "lm.confint.pm.90")
ends <- c("$overall[1,]", "$overall[2,]", "$overall[2,]", "$overall[2,]",
          "", "", "", "", "", "", "", "", "", "", "", "", "")
tables.names <- c("pre.match.balance", "prop.score.balance", "prop.score.balance.10", "prop.score.balance.pm",
                  "effect.size.lm.pre", "effect.size.lm", "effect.size.lm.10", "effect.size.lm.pm",
                  "effect.size.lm.pre.ci95", "effect.size.lm.pre.ci90", "effect.size.lm.ci95", 
                  "effect.size.lm.ci90", "effect.size.lm.10.ci95", "effect.size.lm.10.ci90", 
                  "effect.size.lm.pm.ci95", "effect.size.lm.pm.ci90")

for(j in 1:length(tables)){
  x<- list()
  for(i in 1:length(outcomes.2016)){
    x[[i]] <- eval(parse(text=paste("mainlist[[i]]$",tables[j],ends[j], sep="")))
  }
  hold <- do.call(rbind,x)
  rownames(hold) <- outcomes.2016.english
  assign(tables.names[j], hold)
}

####################################################################
## set 2: sample size tables
####################################################################
# note that effective sample size = equivalent number of matched pairs

tables <- c("em", "em.10", "pm")
tables.names <- c("eff.sample.size", "eff.sample.size.10", "eff.sample.size.pm")

for(j in 1:length(tables)){ 
  x<- list()
  for(i in 1:length(outcomes.2016)){
    x[[i]] <- eval(parse(text=paste("summary(mainlist[[i]]$",tables[j],")$eff", sep="")))
  }
  hold <- do.call(rbind,x)
  rownames(hold) <- outcomes.2016.english
  assign(tables.names[j], hold)
}

####################################################################
## set 3: number of matched sets
####################################################################

num.sets.em <- rep(NA, 10)
for(i in c(2,5:10)){
  num.sets.em[i] <- sum(summary(mainlist[[i]]$em)[[2]])
}
num.sets.em[1] <- sum(summary(mainlist[[1]]$em)[[2]])-summary(mainlist[[1]]$em)[[2]][1]
num.sets.em[3] <- sum(summary(mainlist[[3]]$em)[[2]])-
  summary(mainlist[[3]]$em)[[2]][1]-summary(mainlist[[3]]$em)[[2]][length(summary(mainlist[[3]]$em)[[2]])]
num.sets.em[4] <- sum(summary(mainlist[[4]]$em)[[2]])-summary(mainlist[[4]]$em)[[2]][1]

num.sets.em.10 <- rep(NA, 10)
for(i in c(2,5:10)){
  num.sets.em.10[i] <- sum(summary(mainlist[[i]]$em.10)[[2]])
}
num.sets.em.10[1] <- sum(summary(mainlist[[1]]$em.10)[[2]])-summary(mainlist[[1]]$em.10)[[2]][1]
num.sets.em.10[3] <- sum(summary(mainlist[[3]]$em.10)[[2]])-
  summary(mainlist[[3]]$em.10)[[2]][1]-summary(mainlist[[3]]$em.10)[[2]][length(summary(mainlist[[3]]$em.10)[[2]])]
num.sets.em.10[4] <- sum(summary(mainlist[[4]]$em.10)[[2]])-summary(mainlist[[4]]$em.10)[[2]][1]

####################################################################
## set 4: number of treated units
####################################################################

tables <- c("n.treat", "n.treat.em", "n.treat.em.10", "n.treat.pm")
tables.names <- paste("tab", tables, sep=".")

for(j in 1:length(tables)){ 
  x<- list()
  for(i in 1:length(outcomes.2016)){
    x[[i]] <- eval(parse(text=paste("mainlist[[i]]$",tables[j],sep="")))
  }
  hold <- do.call(rbind,x)
  rownames(hold) <- outcomes.2016.english
  assign(tables.names[j], hold)
}


####################################################################
## save these matching results
####################################################################

save(result.num_aspirants_total.2016.main,
     result.female.sum.2016.main,
     result.cand_owngroups.sum.2016.main,
     result.not_owngroups.sum.2016.main,
     result.numasp_notowngroup_female.sum.2016.main,
     result.female.nominee.2016.main,
     result.private_sector.ONLY.nominee.2016.main,
     result.incumbent.nominee.2016.main,
     result.owngroup.nominee.2016.main,
     result.notowngroup.female.nominee.2016.main,
     pre.match.balance, 
     prop.score.balance, 
     prop.score.balance.10, 
     prop.score.balance.pm, 
     effect.size.lm.pre, 
     effect.size.lm, 
     effect.size.lm.10, 
     effect.size.lm.pm, 
     effect.size.lm.pre.ci95, 
     effect.size.lm.pre.ci90, 
     effect.size.lm.ci95,      
     effect.size.lm.ci90, 
     effect.size.lm.10.ci95,
     effect.size.lm.10.ci90,
     effect.size.lm.pm.ci95,      
     effect.size.lm.pm.ci90,
     eff.sample.size, 
     eff.sample.size.10, 
     eff.sample.size.pm,
     num.sets.em,
     num.sets.em.10,
     tab.n.treat,
     tab.n.treat.em,
     tab.n.treat.em.10,
     tab.n.treat.pm,
     mainlist,
     file="../2_output/0_results/results.RData")
     


####################################################################
## Tables of Effect Sizes
####################################################################

## effect sizes, before matching with other variables including previous Y
round(effect.size.lm.pre, 3)

## effect sizes after full matching in main specification
round(effect.size.lm, 3)

## effect sizes after full matching in main specification
## but limited to 1:10 - 10:1 balance between treatment and control
round(effect.size.lm.10, 3)

## effect sizes after pair matching
round(effect.size.lm.pm, 3)

### effective sample size
round(eff.sample.size, 0)

### balance before matching 
round(pre.match.balance, 2)

### balance after matching
round(prop.score.balance, 2)



