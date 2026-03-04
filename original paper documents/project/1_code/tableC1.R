###################################################################
## tableC1.R
##
###################################################################

rm(list=ls())

load("../0_data/prim.dat.RData")

library(optmatch)
library(xtable)

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


## different sets of matching variables for different outcomes
mv1 <- c("constparty.id", "own2012_parl_p", "own2012_pres_p", "ethfrac_ownparty", "popdens")
mv2 <- c(mv1, "const_muslim_p")
mv3 <- c(mv1, "const_largest_eth_group_p_NAMELEVELS", "seg_acrossparty", "seg_ownparty")
mv4 <- c(mv1, "ethnicity.p.inc.2016")
mv5 <- c(mv1, "const_muslim_p", "female.sum.2012")
mv6 <- c(mv1, "const_owngroups_p", "seg_acrossparty", "num_aspirants_total.2012")
mv7 <- c(mv1, "const_muslim_p", "const_largest_eth_group_p_NAMELEVELS", "seg_acrossparty", "seg_ownparty")
mv8 <- c(mv1, "const_muslim_p", "female.sum.2012", "const_owngroups_p", "seg_acrossparty", "num_aspirants_total.2012")
matchvarlist <- c("mv3", "mv3", "mv2", "mv1",  
                  "mv6", "mv5",  "mv1", "mv4" )


#####################################################################
for(i in 1:length(outcomes.2016)){
  # subset data to complete cases for each outcome
  if(outcomes.2016[i]=="incumbent.nominee.2016"){
    dat2 <- prim.dat[prim.dat$holds_seat.2016==1,]
  }else{
    dat2 <- prim.dat
  }
  dat2a <- dat2[,c("ndc", outcomes.2016[i], get(matchvarlist[i]))]
  names(dat2a)[2] <- substr(names(dat2a)[2], 1, nchar(names(dat2a)[2])-5)
  dat2a$year2016 <- 1
  dat2b <- dat2[,c("ndc", outcomes.2012[i], get(matchvarlist[i]))]
  names(dat2b)[2] <- substr(names(dat2b)[2], 1, nchar(names(dat2b)[2])-5)
  dat2b$year2016 <- 0
  dat2 <- rbind(dat2a, dat2b)
  dat1 <- dat2[complete.cases(dat2),]
  
  #####################################################################
  ## diff in diff with covariates
  dd1 <- lm(as.formula(paste(names(dat1)[2], "~", "ndc*year2016 +", 
                             paste(names(dat1)[-c(1:3,length(names(dat1)))], 
                                   collapse=" + "))), data=dat1)
  
  ## diff in diff without covariates
  dd2 <- lm(as.formula(paste(names(dat1)[2], "~", "ndc*year2016")), data=dat1)
  
  ## save outputs in a list
  holder <- list()
  
  holder$dd.cov <- dd1
  holder$dd.cov.est <- summary(dd1)$coeff["ndc:year2016",]

  holder$dd.nocov <- dd2
  holder$dd.nocov.est <- summary(dd2)$coeff["ndc:year2016",]

  holder$dd.samplesize <- dim(dat1)[1]
  
  ## name the holder
  assign(paste("result", outcomes.2016[i], "dd", sep="."), holder)
}  




## Create a list of these outputs in the order we want to present them

mainlist <- list(result.female.sum.2016.dd, 
                 result.num_aspirants_total.2016.dd,
                 result.female.nominee.2016.dd,
                 result.not_owngroups.sum.2016.dd, 
                 result.cand_owngroups.sum.2016.dd,  
                 result.owngroup.nominee.2016.dd,
                 result.private_sector.ONLY.nominee.2016.dd,  
                 result.incumbent.nominee.2016.dd) 

outcomes.2016.english <- c("Num. Female Aspirants",
                           "Total Number of Aspirants",
                           "Nominee is Female",
                           "Num.Asp. from Non-Associated Ethnic Groups",
                           "Num.Asp. from Party-Associated Ethnic Groups",
                           "Nominee is a Party-Associated Ethnic Group Member",
                           "Nominee has Private Sector Background",
                           "Nominee is the Incumbent")

x<- list()
for(i in 1:length(outcomes.2016.english)){
  x[[i]] <- eval(parse(text=paste("mainlist[[i]]$","dd.cov.est", sep="")))
}
dd.cov.est <- do.call(rbind,x)
rownames(dd.cov.est) <- outcomes.2016.english

x <- dd.cov.est[,-3]

colnames(x) <- c("Estimate", "S.E.", "p-value")

write.csv(round(x,2), file=c("../2_output/2_tables/tabC1_dd.csv"))

tab.results.dd.cov <- xtable(x,
                             label=c("tab:results.dd.cov"),
                             align=c("l", "r","r", "r"),
                             digits=c(1,2,2,2))

print.xtable(tab.results.dd.cov, caption.placement=c("top"), 
             file=c("../2_output/2_tables/tabC1_dd.tex"))
