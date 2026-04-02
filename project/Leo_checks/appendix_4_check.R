# appendix table 4 test: non response sensistivity analysis
# run "REPLICATION_Elections, Protest and Trust_JOP main.R" first

# overall indicator for whether respondent had any nonresponse
# on the trust items used in the main outcomes
df <- df %>%
  mutate(
    any_trust_nr = if_else(
      trustarmynr == 1 |
        trustpolicenr == 1 |
        trustfsbnr == 1 |
        trustcourtnr == 1 |
        trustmungovnr == 1 |
        trustfedgovnr == 1 |
        trustdumanr == 1 |
        trustprocuracynr == 1 |
        trustunnr == 1,
      1, 0
    )
  )

# same outcome list as table 2
nr_outcomes <- table2_outcomes

# baseline models already estimated in mods_table2
mods_nr_baseline <- mods_table2

# complete-case sample: drop anyone with any trust-item nonresponse
fit_nr_complete <- function(outcome) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ period3 + period2min + protestday + electday + ",
      "wealth + education + factor(okrug)"
    )
  )
  
  model_df <- df %>%
    filter(any_trust_nr == 0) %>%
    select(all_of(c(
      outcome, "period3", "period2min", "protestday", "electday",
      "wealth", "education", "okrug"
    ))) %>%
    drop_na()
  
  lm(fml, data = model_df)
}

mods_nr_complete <- setNames(map(nr_outcomes, fit_nr_complete), nr_outcomes)

# full sample + control for any trust nonresponse
fit_nr_control <- function(outcome) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ period3 + period2min + protestday + electday + ",
      "any_trust_nr + wealth + education + factor(okrug)"
    )
  )
  
  lm(fml, data = df)
}

mods_nr_control <- setNames(map(nr_outcomes, fit_nr_control), nr_outcomes)

# combine models for one simple comparison table
mods_nr_compare <- c(
  setNames(mods_nr_baseline, paste0(names(mods_nr_baseline), "_baseline")),
  setNames(mods_nr_complete, paste0(names(mods_nr_complete), "_completecase")),
  setNames(mods_nr_control, paste0(names(mods_nr_control), "_nrcontrol"))
)

# save one html table
if (!dir.exists("outputs_html")) dir.create("outputs_html")

modelsummary(
  mods_nr_compare,
  vcov = function(x) sandwich::vcovHC(x, type = "HC1"),
  coef_map = c(
    "period3" = "Post-Protest",
    "period2min" = "Post-Election",
    "protestday" = "Protest Day",
    "electday" = "Election Day",
    "any_trust_nr" = "Any Trust Nonresponse"
  ),
  coef_omit = "wealth|education|factor\\(okrug\\)",
  gof_map = c("nobs", "r.squared"),
  stars = c("*" = .05, "**" = .01, "***" = .001),
  title = "Nonresponse Sensitivity Test: Baseline vs Complete-Case vs Nonresponse-Control",
  output = file.path("outputs_html", "nonresponse_sensitivity.html")
)

# # coefficient-only data frame for trustgovind
# nr_trustgovind_compare <- bind_rows(
#   broom::tidy(lmtest::coeftest(mods_nr_baseline$trustgovind, vcov. = sandwich::vcovHC(mods_nr_baseline$trustgovind, type = "HC1"))) %>%
#     mutate(model = "baseline"),
#   broom::tidy(lmtest::coeftest(mods_nr_complete$trustgovind, vcov. = sandwich::vcovHC(mods_nr_complete$trustgovind, type = "HC1"))) %>%
#     mutate(model = "complete_case"),
#   broom::tidy(lmtest::coeftest(mods_nr_control$trustgovind, vcov. = sandwich::vcovHC(mods_nr_control$trustgovind, type = "HC1"))) %>%
#     mutate(model = "nr_control")
# ) %>%
#   filter(term %in% c("period3", "period2min", "protestday", "electday", "any_trust_nr"))
# 
# nr_trustgovind_compare# 