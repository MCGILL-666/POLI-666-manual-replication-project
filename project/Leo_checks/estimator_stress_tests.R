# this file runs 4 different tests to stress test the estimator 
# run "REPLICATION_Elections, Protest and Trust_JOP main.R" first

if (!dir.exists("outputs_html")) dir.create("outputs_html")

df <- df %>%
  mutate(
    # keep only observations up to Dec 15 for a narrower post-protest window
    keep_to_dec15 = if_else(
      (MONTH == 11 & DATA >= 25) |
        (MONTH == 12 & DATA <= 15),
      1, 0
    ),
    
    # placebo post indicators
    placebo_post_dec8 = if_else(MONTH == 12 & DATA > 8 & DATA < 26, 1, 0),
    placebo_day_dec8 = if_else(MONTH == 12 & DATA == 8, 1, 0),
    
    placebo_post_dec9 = if_else(MONTH == 12 & DATA > 9 & DATA < 26, 1, 0),
    placebo_day_dec9 = if_else(MONTH == 12 & DATA == 9, 1, 0)
  )

# outcomes
stress_outcomes <- table2_outcomes

# A. baseline models
mods_stress_baseline <- mods_table2

# B. no controls
fit_stress_nocontrols <- function(outcome) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ period3 + period2min + protestday + electday"
    )
  )
  lm(fml, data = df)
}

mods_stress_nocontrols <- setNames(
  map(stress_outcomes, fit_stress_nocontrols),
  stress_outcomes
)

# C. extra controls
fit_stress_morecontrols <- function(outcome) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ period3 + period2min + protestday + electday + ",
      "wealth + education + age + male + permres + nationality + ",
      "unemp + statesector + factor(okrug)"
    )
  )
  lm(fml, data = df)
}

mods_stress_morecontrols <- setNames(
  map(stress_outcomes, fit_stress_morecontrols),
  stress_outcomes
)

# D1. restricted sample: drop event days
fit_stress_noeventdays <- function(outcome) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ period3 + period2min + wealth + education + factor(okrug)"
    )
  )
  
  model_df <- df %>%
    filter(electday != 1, protestday != 1)
  
  lm(fml, data = model_df)
}

mods_stress_noeventdays <- setNames(
  map(stress_outcomes, fit_stress_noeventdays),
  stress_outcomes
)

# D2. restricted sample: narrower window through Dec 15
fit_stress_dec15 <- function(outcome) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ period3 + period2min + protestday + electday + ",
      "wealth + education + factor(okrug)"
    )
  )
  
  model_df <- df %>%
    filter(keep_to_dec15 == 1)
  
  lm(fml, data = model_df)
}

mods_stress_dec15 <- setNames(
  map(stress_outcomes, fit_stress_dec15),
  stress_outcomes
)


# E1. placebo timing: fake post starts Dec 8
fit_stress_placebo_dec8 <- function(outcome) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ placebo_post_dec8 + period2min + placebo_day_dec8 + electday + ",
      "wealth + education + factor(okrug)"
    )
  )
  lm(fml, data = df)
}

mods_stress_placebo_dec8 <- setNames(
  map(stress_outcomes, fit_stress_placebo_dec8),
  stress_outcomes
)

# E2. placebo timing: fake post starts Dec 9
fit_stress_placebo_dec9 <- function(outcome) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ placebo_post_dec9 + period2min + placebo_day_dec9 + electday + ",
      "wealth + education + factor(okrug)"
    )
  )
  lm(fml, data = df)
}

mods_stress_placebo_dec9 <- setNames(
  map(stress_outcomes, fit_stress_placebo_dec9),
  stress_outcomes
)

mods_stress_compare <- list(
  "Baseline" = mods_stress_baseline$trustgovind,
  "No Controls" = mods_stress_nocontrols$trustgovind,
  "More Controls" = mods_stress_morecontrols$trustgovind,
  "No Event Days" = mods_stress_noeventdays$trustgovind,
  "Window Through Dec15" = mods_stress_dec15$trustgovind,
  "Placebo Dec8" = mods_stress_placebo_dec8$trustgovind,
  "Placebo Dec9" = mods_stress_placebo_dec9$trustgovind
)

# save as html table
modelsummary(
  mods_stress_compare,
  vcov = function(x) sandwich::vcovHC(x, type = "HC1"),
  coef_map = c(
    "period3" = "Post-Protest",
    "period2min" = "Post-Election",
    "protestday" = "Protest Day",
    "electday" = "Election Day",
    "placebo_post_dec8" = "Placebo Post-Dec8",
    "placebo_day_dec8" = "Placebo Day-Dec8",
    "placebo_post_dec9" = "Placebo Post-Dec9",
    "placebo_day_dec9" = "Placebo Day-Dec9"
  ),
  coef_omit = "wealth|education|age|male|permres|nationality|unemp|statesector|factor\\(okrug\\)",
  gof_map = c("nobs", "r.squared"),
  stars = c("*" = .05, "**" = .01, "***" = .001),
  title = "Estimator Stress Tests for Trust in Government Index",
  output = file.path("outputs_html", "stress_test_estimator.html")
)
