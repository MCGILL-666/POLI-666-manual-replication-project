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
    "incumbent.nominee.2016"
  ),
  outcomes_2012 = c(
    "not_owngroups.sum.2012",  
    "cand_owngroups.sum.2012",
    "female.sum.2012",
    "num_aspirants_total.2012",
    "owngroup.nominee.2012",
    "female.nominee.2012",
    "private_sector.ONLY.nominee.2012", 
    "incumbent.nominee.2012"
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
  
  # e. no exact matching in this version
  
  # f. full matching 
  m1 = fullmatch(ps.dist1, data=dat1)
  
  # g. post-matching balance check (same as original)
  balance.m1 = xBalance(as.formula(paste(names(dat1)[1], "~", paste(names(dat1)[-c(1,2,4)], collapse="+"), "+ strata(m1)")),
                        data = dat1,report = c("chisquare.test", "std.diffs", "z.scores", "p.values"))  
  # h. construct matching datasets
  dat1a <- data.frame(
    ndc = dat1[[1]],
    y = dat1[[2]],
    m1 = m1,
    propscore = fitted(glm1)
  )
  
  # i. count treated units (how many treated units exist overall & how many are matcheable)
  n.treat = sum(dat1a$ndc == 1)
  n.treat.m1 = sum(dat1a$ndc == 1 & !is.na(dat1a$m1))
  
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
  # i. ATT_m1
  ATT_m1 = fn_compute_att(dat1a, "m1")
  
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
  # compute weights
  dat.weights = dat1a
  dat.weights = fn_compute_weights(dat.weights, "m1")
  # regression weight columns to be used in the next part
  dat.weights$weight = dat.weights$w_m1
  
  # l. regression models (treatment effect estimation with inference)
  # i. m1 model
  df_m1 = dat.weights[!is.na(dat.weights$m1) & !is.na(dat.weights$weight), ]
  lm.m1 = lm(y ~ ndc + m1, data=df_m1, weights=weight)
  
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
    m1=m1,
    # post-match balance
    balance.m1=balance.m1,
    # treated counts
    n.treat = n.treat,
    n.treat.m1 = n.treat.m1,
    # manual ATT
    ATT_m1=ATT_m1,
    # regression models
    lm.m1=lm.m1
  )
}


# 5. compile results
mainlist <- list(
  lst_results[["female.sum.2016"]],
  lst_results[["not_owngroups.sum.2016"]],
  lst_results[["cand_owngroups.sum.2016"]],
  lst_results[["num_aspirants_total.2016"]],
  lst_results[["female.nominee.2016"]],
  lst_results[["owngroup.nominee.2016"]],
  lst_results[["private_sector.ONLY.nominee.2016"]],
  lst_results[["incumbent.nominee.2016"]]
)

outcomes.2016.english <- c(
  "Num. Female Aspirants",
  "Num. Asp. from Non-Associated Ethnic Groups",
  "Num. Asp. from Party-Associated Ethnic Groups",
  "Total Number of Aspirants",
  "Nominee is Female",
  "Nominee is a Party-Associated Ethnic Group Member",
  "Nominee has Private Sector Background",
  "Nominee is the Incumbent"
)


# 6. clean results for tables
# a. balance diagnostics (tab.balance)
tab.balance <- t( # transpose matrix
  sapply(mainlist, function(x){
    vals = x$balance.m1$overall[2,] # get post matching balance 
    # as.numeric(vals)
    suppressWarnings(as.numeric(vals))
  }))
rownames(tab.balance) = outcomes.2016.english

# b. treatment effect estimates (tab.m1.est)

tab.m1.est <- matrix(sapply(mainlist, function(x){
  coef(x$lm.m1)["ndc"]
  }), ncol = 1)
nrow(tab.m1.est)

rownames(tab.m1.est) = outcomes.2016.english
colnames(tab.m1.est) = "Effect"

# c. number of treated units (num.sets.m1)
n.treat.m1 <- matrix(sapply(mainlist, function(x){
  x$n.treat.m1
  }),
  ncol = 1)
rownames(n.treat.m1) = outcomes.2016.english
colnames(n.treat.m1) = "Treated"

# d. number of matched sets (n.treat.m1)
num.sets.m1 = sapply(mainlist, function(x){
  sum(summary(x$m1)[[2]])
  })

# 7. save results
save(
  mainlist,
  tab.balance,
  tab.m1.est,
  num.sets.m1,
  n.treat.m1,
  file = "../2_output/0_results/results.noexact.RData"
)

# PRINT MAIN RESULTS
cat("MAIN TREATMENT EFFECT ESTIMATES (NO EXACT MATCHING)\n\n")
cat("Effect of expanding NDC primary electorate\n\n")
print(round(tab.m1.est, 3))
cat("POST-MATCHING BALANCE DIAGNOSTICS\n\n")
print(round(tab.balance, 2))
cat("NUMBER OF MATCHED SETS\n")
print(num.sets.m1)
cat("TREATED UNIT DIAGNOSTICS\n\n")
print(n.treat.m1)


