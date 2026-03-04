###################################################################
## analysis_with_calipers.R
##
## produces output used to produce Figure C1
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


#the calipers to permute over
calipers <- seq(0.1, 1.5, by=0.1)

#the holders for the output of each run through the loop of the calipers
calipers.effect.size.lm <- matrix(NA, nrow=length(outcomes.2016), ncol=length(calipers))
rownames(calipers.effect.size.lm) <- outcomes.2016
colnames(calipers.effect.size.lm) <- paste("cal.", as.character(calipers), sep="")

calipers.effect.size.lm.p <- calipers.effect.size.lm
calipers.effect.size.lm.ci95.low <- calipers.effect.size.lm
calipers.effect.size.lm.ci95.high <- calipers.effect.size.lm
calipers.effect.size.lm.ci90.low <- calipers.effect.size.lm
calipers.effect.size.lm.ci90.high <- calipers.effect.size.lm
calipers.prop.score.balance.p<- calipers.effect.size.lm
calipers.prop.score.balance.nt <- calipers.effect.size.lm

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
mv4 <- c(mv1, "ethnicity.p.inc.2016")
mv5 <- c(mv1, "const_muslim_p", "female.sum.2012")
mv6 <- c(mv1, "const_owngroups_p", "seg_acrossparty", "num_aspirants_total.2012")
matchvarlist <- c("mv3", "mv3", "mv2", "mv1",  
                  "mv6", "mv5",  "mv1", "mv4")

for(k in 1:length(calipers)){

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
  ps.dist0 <- match_on(glm1, data=dat1)
  ## caliper =.15 pooled SD of the propensity score -- this is where we're looping in the different calipers
  ps.dist1 <- caliper(ps.dist0, width=calipers[k])
  
  ## full matching with exact matches on lagged outcome
  em <- fullmatch(ps.dist1 + exactMatch(dat1[,1] ~ dat1[,3], data=dat1), data=dat1)
  balance <- xBalance(as.formula(paste(names(dat1)[1], "~", 
                   paste(names(dat1)[-c(1,2,3,4)], collapse="+"), "+ strata(em)")), 
                   data=dat1, 
                   report = c("chisquare.test", "std.diffs", "z.scores", "p.values"))
  
  ## pull together data for effect estimates
  dat1a <- cbind(dat1[,1:2], em, fitted(glm1))
  names(dat1a) <- c("ndc", "y", "em", "propscore")
  
  ## number of NDC units remaining after matching
  num.ndc.em <- sum(1-is.na(dat1a$em[dat1a$ndc==1]))
  
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

  
  ## estimates and s.e. from models
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
  
  holder$num.ndc.em <- num.ndc.em
  
  assign(paste("result", outcomes.2016[i], "caliper", sep="."), holder)
}   



## Create a list of these outputs in the order we want to present them

mainlist <- list(result.not_owngroups.sum.2016.caliper, 
                  result.cand_owngroups.sum.2016.caliper, 
                  result.female.sum.2016.caliper, 
                   result.num_aspirants_total.2016.caliper,
                   result.owngroup.nominee.2016.caliper,
                   result.female.nominee.2016.caliper,
                   result.private_sector.ONLY.nominee.2016.caliper,  
                   result.incumbent.nominee.2016.caliper) 
  

outcomes.2016.english <- c("Num.Asp. from Non-Associated Ethnic Groups",
                           "Num.Asp. from Party-Associated Ethnic Groups",
                           "Num. Female Aspirants",
                           "Total Number of Aspirants",
                           "Nominee is a Party-Associated Ethnic Group Member",
                           "Nominee is Female",
                           "Nominee has Private Sector Background",
                           "Nominee is the Incumbent")
  

## balance, lm est, confint
tables <- c("balance.pre", "balance", 
            "lm.est.pre", "lm.est", 
            "lm.confint.pre.95", "lm.confint.pre.90", 
            "lm.confint.95", "lm.confint.90")
ends <- c("$overall[1,]", "$overall[2,]",
          "", "", "", "", "", "")
tables.names <- c("pre.match.balance", "prop.score.balance", 
                  "effect.size.lm.pre", "effect.size.lm", 
                  "effect.size.lm.pre.ci95", "effect.size.lm.pre.ci90", 
                  "effect.size.lm.ci95", "effect.size.lm.ci90")

for(j in 1:length(tables)){
  x<- list()
  for(i in 1:length(outcomes.2016)){
    x[[i]] <- eval(parse(text=paste("mainlist[[i]]$",tables[j],ends[j], sep="")))
  }
  hold <- do.call(rbind,x)
  rownames(hold) <- outcomes.2016.english
  assign(tables.names[j], hold)
}

## number of treated units in sample after matching
x<- list()
for(i in 1:length(outcomes.2016)){
  x[[i]] <- eval(parse(text="mainlist[[i]]$num.ndc.em"))
  }
num.ndc.em <- do.call(rbind,x)
rownames(num.ndc.em) <- outcomes.2016.english
  
calipers.effect.size.lm[,k] <- effect.size.lm[,1]
calipers.effect.size.lm.p[,k] <- effect.size.lm[,4]
calipers.effect.size.lm.ci95.low[,k] <- effect.size.lm.ci95[,1]
calipers.effect.size.lm.ci95.high[,k] <- effect.size.lm.ci95[,2]
calipers.effect.size.lm.ci90.low[,k] <- effect.size.lm.ci90[,1]
calipers.effect.size.lm.ci90.high[,k] <- effect.size.lm.ci90[,2]
calipers.prop.score.balance.p[,k] <- prop.score.balance[,3]
calipers.prop.score.balance.nt[,k] <- num.ndc.em[,1]

} #closes the k calipers loop 

## save results
 save(calipers.effect.size.lm,
 	calipers.effect.size.lm.p,
 	calipers.effect.size.lm.ci95.low,
 	calipers.effect.size.lm.ci95.high, 
 	calipers.effect.size.lm.ci90.low, 
 	calipers.effect.size.lm.ci90.high, 
 	calipers.prop.score.balance.p, 
 	calipers.prop.score.balance.nt, 
 	calipers,
 	file="../2_output/0_results/results.caliper.RData")




