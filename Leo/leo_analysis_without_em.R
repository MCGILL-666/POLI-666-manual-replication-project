# This script is similar to the original analysis.R, but without the exact matching. 
# Lagged outcome is folded into the propensity-score model, then effects are estimated with full matching only. 
# Produces results for appendix table C6. 

##### STANDARDIZED OBJECT NAMES #####
# df_ = data frame
# vec_ = vector
# lst_ = list
# cfg_ = configurable objects (mappings, parameters, etc)
# fn_ = helper functions
# ps_ = propensity score stuff

# df_prim = prim.dat.Rdata dataset
# cfg_outcomes = mapping of outcomes (2016) to lagged outcomes (2012)
# cfg_matchvars = mapping of outcomes to covariate sets
# lst_specs = outcome specifications (for each outcome, including name, lag, covariates)
# dat1 = working dataset for each outcome group (to add in main loop)
# lst_results = stores result for each of the 10+ outcomes
# nrows_... = any int storing the number of rows in a dataset
#######################################

# script structure: 
  # 1. load packages & data
  # 2. define outcomes
  # 3. define covariates
  # 4. run the analysis
    # a. build dataset
    # b. pre-matching regression (removed in the original but kept here)
    # c. pre-matching balance diagnostics (removed in the original but kept here)
    # d. estimate propensity score
    # e. compute propensity score distance
    # f. full matching 
      # i. main (em)
    # g. post-matching balance check
    # h. construct matching datasets
    # i. count treated units
    # j. compute ATT manually (treatment effect estimation)
      # i. ATT_em
    # k. compute matching weights
    # l. regression model (treatment effect estimation with inference)
      # i. lm(y ~ ndc + em) / weights = weight
    # m. store results
  # 5. compile results
  # 6. clean results for appendix tables
  # 7. save & print results




# 1. clear environment, load libraries, load dataset
rm(list=ls())
library(optmatch)
library(RItools)

# change which line is commented based on working directory
load("../0_data/prim.dat.Rdata")
#load("../original paper documents/project/0_data/prim.dat.Rdata")
df_prim <- prim.dat
rm(prim.dat)


# 2. define outcomes (2016) & lagged outcomes (2012)
cfg_outcomes <- list(
  outcomes_2016 = c(
    "not_owngroups.sum.2016",  
    "cand_owngroups.sum.2016",
    "female.sum.2016",
    "num_aspirants_total.2016",
    "owngroup.nominee.2016",
    "female.nominee.2016",
    "private_sector.ONLY.nominee.2016", 
    "incumbent.nominee.2016",
  ),
  outcomes_2012 = c(
    "not_owngroups.sum.2012",  
    "cand_owngroups.sum.2012",
    "female.sum.2012",
    "num_aspirants_total.2012",
    "owngroup.nominee.2012",
    "female.nominee.2012",
    "private_sector.ONLY.nominee.2012", 
    "incumbent.nominee.2012",
  )
)
# make the two sublists the same length
#stopifnot(length(cfg_outcomes$outcomes_2016) == length(cfg_outcomes$outcomes_2012))

# 3. define covariates

# (pasted from original analysis.R)
mv1 <- c("constparty.id", "own2012_parl_p", "own2012_pres_p", "ethfrac_ownparty", "popdens")
mv2 <- c(mv1, "const_muslim_p")
mv3 <- c(mv1, "const_largest_eth_group_p_NAMELEVELS", "seg_acrossparty", "seg_ownparty")
mv4 <- c(mv1, "ethnicity.p.inc.2016")
mv5 <- c(mv1, "const_muslim_p", "female.sum.2012")
mv6 <- c(mv1, "const_owngroups_p", "seg_acrossparty", "num_aspirants_total.2012")
mv7 <- c(mv1, "const_muslim_p", "const_largest_eth_group_p_NAMELEVELS", "seg_acrossparty", "seg_ownparty")
matchvarlist <- c("mv3", "mv3", "mv2", "mv1", "mv7",  
                  "mv6", "mv5",  "mv1", "mv4")

# define cdf_matchvars (mapping of outcomes to covariate sets)
cfg_matchvars <- list(
  sets = list(mv1 = mv1, mv2 = mv2, mv3 = mv3, mv4 = mv4, mv5 = mv5, mv6 = mv6, mv7 = mv7), 
  sets_for_outcome = matchvarlist # which set is used for which outcome
)
rm(mv1, mv2, mv3, mv4, mv5, mv6, mv7, matchvarlist)

######################## GOOD UP TO HERE... STILL NEED TO CHANGE/EDIT/REMOVE CODE FROM NEXT PARTS

# 4. run the analysis
lst_results = list()
for (i in 1:length(cfg_outcomes$outcomes_2016)){
  # a. build the dataset
  outcome_2016 = cfg_outcomes$outcomes_2016[i]
  outcome_2012 = cfg_outcomes$outcomes_2012[i]
  vec_covars = cfg_matchvars$sets[[cfg_matchvars$sets_for_outcome[i]]]
  
  # special restriction for incumbent nomination in 2016, only include cases where the nominee is actually an incumbent 
  if (outcome_2016 == "incumbent.nominee.2016"){ 
    dat1 = df_prim[df_prim$holds_seat.2016 == 1, c("ndc", outcome_2016, outcome_2012, vec_covars)]
  } else {
    dat1 = df_prim[, c("ndc", outcome_2016, outcome_2012, vec_covars)] # treatment, outcome, lagged outcome, and covariates
  }
  dat1 <- dat1[complete.cases(dat1),]
  
  # b. pre-matching regression
  lm1 <- lm(as.formula(paste(names(dat1)[2], "~",paste(names(dat1)[-c(2, 4)], collapse = " + "))), data = dat1)
  
  # c. pre-matching balance diagnostics (same as original)
  balance.pre <- xBalance(
    as.formula(paste(names(dat1)[1], "~", paste(names(dat1)[-c(1,2,4)], collapse=" + "))), 
    data = dat1, report=c("chisquare.test", "std.diffs", "z.scores", "p.values"))
  
  # d. estimate propensity score (same as original)
  ## propensity scores
  glm1 <- glm(as.formula(paste(names(dat1)[1], "~", paste(names(dat1)[-c(1,2,4)], 
                                                          collapse=" + "))), family=binomial(), data=dat1)
  ## distance
  ps.dist1 <- match_on(glm1, data=dat1)
  
  # e. exact matching
  ex1 = exactMatch(dat1[, 1] ~ dat1[, 3], data = dat1)
  
  # f. full matching 
  # i. main (em): optimal full matching + exact on lagged outcome
  em = fullmatch(ps.dist1 + ex1, data = dat1)
  # ii. restricted ratio (em.10): restricted ratio: 1:10–10:1 (appendix)
  em.10 = fullmatch(ps.dist1 + ex1, data = dat1, min = 0.1, max.controls = 10)
  # iii. pair matching (pm): pair matching (appendix)
  pm = pairmatch(ps.dist1 + ex1, data = dat1, remove.unmatchables = TRUE)
  
  # g. post-matching balance check (same as original)
  balance.em <- xBalance(as.formula(paste(names(dat1)[1], "~", 
                                          paste(names(dat1)[-c(1,2,3,4)], collapse="+"), "+ strata(em)")), 
                         data=dat1, report = c("chisquare.test", "std.diffs", "z.scores", "p.values"))
  balance.em10 <- xBalance(as.formula(paste(names(dat1)[1], "~", 
                                            paste(names(dat1)[-c(1:4)], collapse="+"), "+ strata(em.10)")), 
                           data=dat1, report = c("chisquare.test", "std.diffs", "z.scores", "p.values"))  
  balance.pm <- xBalance(as.formula(paste(names(dat1)[1], "~", 
                                          paste(names(dat1)[-c(1,2,4)], collapse="+"), "+ strata(pm)")), 
                         data=dat1, report = c("chisquare.test", "std.diffs", "z.scores", "p.values")) 
  
  # h. construct matching datasets
  dat1a <- data.frame(
    ndc = dat1[[1]],
    y = dat1[[2]],
    em = em,
    em.10 = em.10,
    pm = pm,
    propscore = fitted(glm1)
  )
  
  # i. count treated units (how many treated units exist overall & how many are matcheable)
  n.treat = sum(dat1a$ndc == 1)
  n.treat.em = sum(dat1a$ndc == 1 & !is.na(dat1a$em))
  n.treat.em10 = sum(dat1a$ndc == 1 & !is.na(dat1a$em.10))
  n.treat.pm = sum(dat1a$ndc == 1 & !is.na(dat1a$pm))
  
  # j. compute ATT manually (treatment effect estimation)
  # helper function to compute ATT from matched strata
  fn_compute_att <- function(df, strata_col, treat_col="ndc", y_col="y"){ 
    # drop units with NA strata (unmatched)
    temp_df = df[!is.na(df[[strata_col]]), ]
    if (nrow(temp_df) == 0) return(NA_real_)
    # compute treated & control within strata
    treated_means = aggregate(temp_df[[y_col]] ~ temp_df[[strata_col]], data=temp_df[temp_df[[treat_col]] == 1, ], FUN=mean) # treated
    control_means = aggregate(temp_df[[y_col]] ~ temp_df[[strata_col]], data=temp_df[temp_df[[treat_col]] == 0, ], FUN=mean) # control
    
    # merge by strata; strata must contain both treated and control to contribute
    temp = merge(treated_means, control_means, by="temp_df[[strata_col]]", all=FALSE)
    if (nrow(temp) == 0) return(NA_real_)
    names(temp) = c("strata", "meanYT", "meanYC")
    temp$diff = temp$meanYT - temp$meanYC
    
    tab = table(temp_df[[strata_col]], temp_df[[treat_col]])
    # ensure we grab treated counts robustly even if table column names vary
    treated_counts = tab[, colnames(tab) %in% c("1", 1), drop = TRUE]
    treated_counts = treated_counts[match(temp$strata, names(treated_counts))]
    temp$w = treated_counts / sum(treated_counts, na.rm=TRUE)
    
    sum(temp$diff * temp$w, na.rm=TRUE)
  }
  # i. ATT_em
  ATT_em = fn_compute_att(dat1a, "em")
  # ii. ATT_em10
  ATT_em10 = fn_compute_att(dat1a, "em.10")
  # iii. ATT_pm
  ATT_pm = fn_compute_att(dat1a, "pm")
  
  # k. compute matching weights
  # helper function to compute matching weights for ATT regression
  fn_compute_weights <- function(df, strata_col, treat_col="ndc"){
    temp_df = df
    # default weights NA; only defined for matched units
    w = rep(NA_real_, nrow(temp_df))
    
    ok = !is.na(temp_df[[strata_col]])
    if (!any(ok)) {
      temp_df[[paste0("w_", strata_col)]] <- w
      return(temp_df)
    }
    
    tab = table(temp_df[[strata_col]][ok], temp_df[[treat_col]][ok])
    # extract treated and control counts, except missing cols
    treated_counts = if ("1" %in% colnames(tab)) tab[,"1"] else tab[, which(colnames(tab)==1)]
    control_counts = if ("0" %in% colnames(tab)) tab[,"0"] else tab[, which(colnames(tab)==0)]
    
    ratio = treated_counts / control_counts
    
    # finally assign weights
    strata_vals = as.character(temp_df[[strata_col]])
    w[ok & temp_df[[treat_col]]==1] = 1
    w[ok & temp_df[[treat_col]]==0] = ratio[strata_vals[ok & temp_df[[treat_col]] == 0]]
    
    temp_df[[paste0("w_", strata_col)]] = w
    temp_df
    
  }
  # actually compute weights
  dat.weights = dat1a
  dat.weights = fn_compute_weights(dat.weights, "em")
  dat.weights = fn_compute_weights(dat.weights, "em.10")
  dat.weights = fn_compute_weights(dat.weights, "pm")
  # regression weight columns to be used in the next part
  dat.weights$weight = dat.weights$w_em
  dat.weights$weight.10 = dat.weights$w_em.10
  dat.weights$weight.pm = dat.weights$w_pm
  
  # l. regression models (treatment effect estimation with inference)
  # i. em model: lm(y ~ ndc + em) / weights = weight
  df_em = dat.weights[!is.na(dat.weights$em) & !is.na(dat.weights$weight), ]
  lm.em = lm(y ~ ndc + em, data=df_em, weights=weight)
  # ii. em.10 model: lm(y ~ ndc + em.10) / weights = weight.10
  df_em10 = dat.weights[!is.na(dat.weights$em.10) & !is.na(dat.weights$weight.10), ]
  lm.em10 = lm(y ~ ndc + em.10, data=df_em10, weights=weight.10)
  # iii. pm model: lm(y ~ ndc + pm) / weights = weight.pm
  df_pm = dat.weights[!is.na(dat.weights$pm) & !is.na(dat.weights$weight.pm), ]
  lm.pm = lm(y ~ ndc + pm, data=df_pm, weights=weight.pm)
  
  # m. store results
  lst_results[[outcome_2016]] <- list(
    # identifiers
    outcome_2016 = outcome_2016,
    outcome_2012 = outcome_2012,
    covariates   = vec_covars,
    # models and matching objects
    lm.pre = lm1,
    balance.pre = balance.pre,
    ps.model = glm1,
    ps.dist  = ps.dist1,
    exact.match = ex1,
    em = em,
    em.10 = em.10,
    pm = pm,
    # post-match balance
    balance.em = balance.em,
    balance.em10 = balance.em10,
    balance.pm = balance.pm,
    # treated counts
    n.treat = n.treat,
    n.treat.em = n.treat.em,
    n.treat.em10 = n.treat.em10,
    n.treat.pm = n.treat.pm,
    # manual ATT
    ATT_em = ATT_em,
    ATT_em10 = ATT_em10,
    ATT_pm = ATT_pm,
    # regression models
    lm.em = lm.em,
    lm.em10 = lm.em10,
    lm.pm = lm.pm
  )
}


# 5. compile results
mainlist <- list(
  lst_results[["female.sum.2016"]],
  lst_results[["not_owngroups.sum.2016"]],
  lst_results[["cand_owngroups.sum.2016"]],
  lst_results[["numasp_notowngroup_female.sum.2016"]],
  lst_results[["num_aspirants_total.2016"]],
  lst_results[["female.nominee.2016"]],
  lst_results[["owngroup.nominee.2016"]],
  lst_results[["notowngroup.female.nominee.2016"]],
  lst_results[["private_sector.ONLY.nominee.2016"]],
  lst_results[["incumbent.nominee.2016"]]
)

outcomes.2016.english <- c(
  "Num. Female Aspirants",
  "Num. Asp. from Non-Associated Ethnic Groups",
  "Num. Asp. from Party-Associated Ethnic Groups",
  "Num. Non-Core Female Aspirants",
  "Total Number of Aspirants",
  "Nominee is Female",
  "Nominee is a Party-Associated Ethnic Group Member",
  "Nominee is Female and Non-Core Group Member",
  "Nominee has Private Sector Background",
  "Nominee is the Incumbent"
)


# 6. clean results for tables
# treatment effect estimates
effect.size.lm <- sapply(mainlist, function(x){
  coef(x$lm.em)["ndc"]
})

effect.size.lm = matrix(effect.size.lm, ncol=1)
rownames(effect.size.lm) = outcomes.2016.english
colnames(effect.size.lm) = "Effect"

# confidence intervals
effect.size.lm.ci95 <- t(
  sapply(mainlist, function(x){
    confint(x$lm.em, "ndc", level =0.95)
  }))

effect.size.lm.ci90 <- t(
  sapply(mainlist, function(x){
    confint(x$lm.em, "ndc", level=0.90)
  }))
rownames(effect.size.lm.ci95) = outcomes.2016.english
rownames(effect.size.lm.ci90) = outcomes.2016.english

# ps balance diagnostics

# prop.score.balance <- t(
#   sapply(mainlist, function(x){
#     x$balance.em$overall[2,]
#   }))
prop.score.balance <- t(
  sapply(mainlist, function(x){
    vals = x$balance.em$overall[2,]
    vals = suppressWarnings(as.numeric(vals))
    vals
  }))

# fn_extract_balance_after <- function(xbal){
#   ov = xbal$overall
#   
#   # If it's a data frame, coerce columns to numeric where possible
#   if (is.data.frame(ov)) {
#     # keep only numeric-convertible columns
#     ov_num = as.data.frame(lapply(ov, function(col) suppressWarnings(as.numeric(col))))
#     rownames(ov_num) = rownames(ov)
#     ov = as.matrix(ov_num)
#   } else {
#     ov = as.matrix(ov)
#   }
#   
#   # find explicit "After" row if it exists
#   rn = rownames(ov)
#   if (!is.null(rn)) {
#     after_idx = which(tolower(rn) %in% c("after", "post", "matched"))
#     if (length(after_idx) >= 1) return(ov[after_idx[1], , drop = TRUE])
#   }
#   
#   if (nrow(ov) >= 2) return(ov[2, , drop = TRUE])
#   
#   rep(NA_real_, ncol(ov)) # return all NAs if nothing works
# }
# 
# prop.score.balance <- t(
#   sapply(mainlist, function(x){
#     fn_extract_balance_after(x$balance.em)
#   })
# )

rownames(prop.score.balance) = outcomes.2016.english

# effective post-matching sample size 
fn_eff_sample_size <- function(model){
  w = model$weights
  if (is.null(w)) return(NA_real_)
  (sum(w)^2) / sum(w^2)
}

eff.sample.size <- sapply(mainlist, function(x){
  fn_eff_sample_size(x$lm.em)
})

eff.sample.size = matrix(eff.sample.size, ncol = 1)
rownames(eff.sample.size) = outcomes.2016.english
colnames(eff.sample.size) = "Eff.Sample.Size"

# sample size diagnostics
treated.diagnostics <- t(
  sapply(mainlist, function(x){
    c(
      Treated_Total = x$n.treat,
      Treated_em = x$n.treat.em,
      Treated_em10 = x$n.treat.em10,
      Treated_pm = x$n.treat.pm
    )}
  ))
rownames(treated.diagnostics) = outcomes.2016.english


# PRINT MAIN RESULTS
cat("MAIN TREATMENT EFFECT ESTIMATES\n\n")
cat("Effect of expanding NDC primary electorate\n\n")
print(round(effect.size.lm, 3))
cat("95% CONFIDENCE INTERVALS\n\n")
print(round(effect.size.lm.ci95, 3))
cat("EFFECTIVE SAMPLE SIZE AFTER MATCHING\n")
print(round(eff.sample.size, 0))
#cat("PROPENSITY SCORE BALANCE DIAGNOSTICS\n\n")
#print(round(prop.score.balance, 2))
cat("TREATED UNIT DIAGNOSTICS\n\n")
print(treated.diagnostics)