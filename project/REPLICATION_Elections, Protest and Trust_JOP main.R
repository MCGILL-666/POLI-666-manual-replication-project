# This file is split into the following sections:
# 1. Helper functions 
# 2. data construction
# 3. t-tests (table 1)
# 4. main regressions (table 2)
# 5. trust index by party and period (figure 1)
# 6. heterogeneity by partisanship (table 3)
# 7. coefficient plot from the interaction model (figure 2)
# 8. appendix balance tests (appendices 1-4)
# 9. daily dummies response model (appendix 5)


library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(purrr)
library(broom)
library(stringr)
library(sandwich)
library(lmtest)
library(modelsummary)

# import data
data <- read_dta("moscowpopsurvey.dta")

# HELPER FUNCTIONS
# recode trust variables
# 99 -> NA, 1 -> 5, 2 -> 4, 3 -> 3, 4 -> 2, 5 -> 1
recode_trust <- function(x) {
  case_when(
    x == 99 ~ NA_real_,
    x == 1  ~ 5,
    x == 2  ~ 4,
    x == 3  ~ 3,
    x == 4  ~ 2,
    x == 5  ~ 1,
    TRUE    ~ as.numeric(x)
  )
}

# Welch t test output
run_ttest <- function(df, outcome, group, filter_expr = TRUE) {
  tmp <- df %>%
    filter({{ filter_expr }}) %>%
    select(all_of(c(outcome, group))) %>%
    filter(!is.na(.data[[outcome]]), !is.na(.data[[group]]))
  
  tt <- t.test(tmp[[outcome]] ~ tmp[[group]])
  
  tibble(
    outcome = outcome,
    group_var = group,
    mean_group0 = tt$estimate[[1]],
    mean_group1 = tt$estimate[[2]],
    diff = unname(tt$estimate[[2]] - tt$estimate[[1]]),
    p_value = tt$p.value,
    conf_low = tt$conf.int[1],
    conf_high = tt$conf.int[2]
  )
}

# coefficient table for lm objects
tidy_robust <- function(model) {
  broom::tidy(
    lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC1"))
  )
}


# CONSTRUCT VARIABLES
df <- data %>%
  mutate(
    # three periods 
    period = if_else(DATA > 24 | DATA < 5, 1, 0),
    dec25 = if_else(MONTH == 12 & DATA == 25, 1, 0),
    period1 = period - dec25,
    period2 = if_else(DATA > 4 & DATA < 11, 1, 0),
    electday = if_else(MONTH == 12 & DATA == 4, 1, 0),
    protestday = if_else(MONTH == 12 & DATA == 10, 1, 0),
    period1min = period1 - electday,
    period2min = period2 - protestday,
    period3 = if_else(DATA > 10 & DATA < 26 & MONTH == 12, 1, 0),
    
    # trust variables 
    trustarmy       = recode_trust(q9a),
    trustpolice     = recode_trust(q9b),
    trustfsb        = recode_trust(q9c),
    trustcourt      = recode_trust(q9d),
    trustmungov     = recode_trust(q9e),
    trustfedgov     = recode_trust(q9f),
    trustduma       = recode_trust(q9g),
    trustprocuracy  = recode_trust(q9h),
    trustun         = recode_trust(q9j),
    
    trustgovind = rowMeans(
      cbind(
        trustarmy, trustpolice, trustfsb, trustcourt,
        trustmungov, trustfedgov, trustduma, trustprocuracy
      ),
      na.rm = FALSE
    ),
    
    #  controls 
    age = as.numeric(q1),
    male = case_when(
      q2 == 2 ~ 0,
      q2 == 1 ~ 1,
      TRUE ~ as.numeric(q2)
    ),
    wealth = if_else(q3 == 99, NA_real_, as.numeric(q3)),
    education = if_else(q5 == 99, NA_real_, as.numeric(q5)),
    permres = case_when(
      q59 == 99 ~ NA_real_,
      q59 %in% c(2, 3) ~ 0,
      TRUE ~ as.numeric(q59)
    ),
    nationality = case_when(
      q67 == 1 ~ 1, # russian ethnicity
      q67 %in% c(2:11, 99) ~ 0,
      TRUE ~ as.numeric(q67)
    ),
    unemp = if_else(q6 %in% c(11, 12, 13), 1, 0),
    statesector = if_else(q7 > 9 & q7 < 13, 1, 0),
    
    okrug = factor(okrug),
    
    # partisanship
    urplan = if_else(as.numeric(q62) == 6, 1, 0, missing = 0),
    urturn = if_else(as.numeric(q64) == 6, 1, 0, missing = 0),
    ur = urplan + urturn,
    
    complan = if_else(as.numeric(q62) == 3, 1, 0, missing = 0),
    compturn = if_else(as.numeric(q64) == 3, 1, 0, missing = 0),
    comp = complan + compturn,
    
    ldprplan = if_else(as.numeric(q62) == 4, 1, 0, missing = 0),
    ldprturn = if_else(as.numeric(q64) == 4, 1, 0, missing = 0),
    ldpr = ldprplan + ldprturn,
    
    yabplan = if_else(as.numeric(q62) == 1, 1, 0, missing = 0),
    yabturn = if_else(as.numeric(q64) == 1, 1, 0, missing = 0),
    yab = yabplan + yabturn,
    
    patplan = if_else(as.numeric(q62) == 2, 1, 0, missing = 0),
    patturn = if_else(as.numeric(q64) == 2, 1, 0, missing = 0),
    pat = patplan + patturn,
    
    srplan = if_else(as.numeric(q62) == 5, 1, 0, missing = 0),
    srturn = if_else(as.numeric(q64) == 5, 1, 0, missing = 0),
    sr = srplan + srturn,
    
    rcplan = if_else(as.numeric(q62) == 7, 1, 0, missing = 0),
    rcturn = if_else(as.numeric(q64) == 7, 1, 0, missing = 0),
    rc = rcplan + rcturn,
    
    dkplan = if_else(as.numeric(q62) == 99, 1, 0, missing = 0),
    dkturn = if_else(as.numeric(q64) == 99, 1, 0, missing = 0),
    dk = dkplan + dkturn,
    
    nopart = if_else(
      coalesce(as.numeric(q61) == 2, FALSE) |
        coalesce(as.numeric(q63) == 2, FALSE) |
        coalesce(as.numeric(q62) == 0, FALSE),
      1, 0
    ),
    
    opposition = yab + pat + sr + rc,
    
    # nonresponse variables 
    trustgovindnr = if_else(is.na(trustgovind), 1, 0),
    
    trustarmynr = if_else(q9a == 99, 1, 0),
    trustpolicenr = if_else(q9b == 99, 1, 0),
    trustfsbnr = if_else(q9c == 99, 1, 0),
    trustcourtnr = if_else(q9d == 99, 1, 0),
    trustmungovnr = if_else(q9e == 99, 1, 0),
    trustfedgovnr= if_else(q9f == 99, 1, 0),
    trustdumanr = if_else(q9g == 99, 1, 0),
    trustprocuracynr = if_else(q9h == 99, 1, 0),
    trustunnr = if_else(q9j == 99, 1, 0),
    
    # figure variables 
    period123 = case_when(
      period1min == 1 ~ 1,
      period2min == 1 ~ 2,
      period3 == 1    ~ 3,
      TRUE ~ NA_real_
    ),
    
    party = case_when(
      ur == 1 ~ 1,
      comp == 1 ~ 2,
      ldpr == 1 ~ 3,
      opposition == 1 ~ 4,
      nopart == 1 ~ 5,
      dk == 1 ~ 6,
      TRUE ~ 0
    ),
    
    # interactions for Table 3 
    urafterprotest = ur * period3,
    urpostelection = ur * period2min,
    apolitical = nopart + dk,
    apolafterprotest = apolitical * period3,
    apolpostelection = apolitical * period2min
  )


# TABLE 1 T-TESTS
trust_vars <- c(
  "trustarmy", "trustpolice", "trustfsb", "trustcourt",
  "trustmungov", "trustfedgov", "trustduma",
  "trustprocuracy", "trustun", "trustgovind"
)

# Period 1 vs Period 2
ttest_p1_p2 <- map_dfr(
  trust_vars,
  ~run_ttest(
    df,
    outcome = .x,
    group = "period1min",
    filter_expr = period3 != 1 & electday != 1 & protestday != 1
  )
) %>%
  mutate(comparison = "Period 1 vs Period 2")

# Period 1 vs Period 3
ttest_p1_p3 <- map_dfr(
  trust_vars,
  ~run_ttest(
    df,
    outcome = .x,
    group = "period1min",
    filter_expr = period2min != 1 & electday != 1 & protestday != 1
  )
) %>%
  mutate(comparison = "Period 1 vs Period 3")

# Period 2 vs Period 3
ttest_p2_p3 <- map_dfr(
  trust_vars,
  ~run_ttest(
    df,
    outcome = .x,
    group = "period2min",
    filter_expr = period1min != 1 & electday != 1 & protestday != 1
  )
) %>%
  mutate(comparison = "Period 2 vs Period 3")

table1_tests <- bind_rows(ttest_p1_p2, ttest_p1_p3, ttest_p2_p3)

table1_tests


# TABLE 2 REGRESSIONS
table2_outcomes <- c(
  "trustgovind", "trustarmy", "trustpolice", "trustfsb",
  "trustcourt", "trustmungov", "trustfedgov",
  "trustduma", "trustprocuracy", "trustun"
)

fit_table2 <- function(outcome) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ period3 + period2min + protestday + electday + ",
      "wealth + education + factor(okrug)"
    )
  )
  lm(fml, data = df)
}

mods_table2 <- setNames(map(table2_outcomes, fit_table2), table2_outcomes)


# FIGURE 1
plot_df1 <- df %>%
  filter(period123 %in% c(1, 2, 3), party %in% 1:6) %>%
  mutate(
    period123 = factor(period123,
                       levels = c(1, 2, 3),
                       labels = c("Pre-election", "Post-election", "Post-protest")),
    party = factor(
      party,
      levels = 1:6,
      labels = c("UR", "CPRF", "LDPR", "Opposition", "Non-voters", "Undecided")
    )
  ) %>%
  group_by(party, period123) %>%
  summarise(mean_trustgovind = mean(trustgovind, na.rm = TRUE), .groups = "drop")

ggplot(plot_df1, aes(x = period123, y = mean_trustgovind, fill = period123)) +
  geom_col(position = "dodge") +
  facet_wrap(~ party) +
  labs(
    x = NULL,
    y = "Trust in Government Index",
    title = "Mean Trust in Government by Party and Period"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
# TABLE 3
fit_table3 <- function(outcome) {
  vars_needed <- c(
    outcome,
    "period3", "period2min", "protestday", "electday",
    "ur", "urafterprotest", "urpostelection",
    "apolitical", "apolafterprotest", "apolpostelection",
    "wealth", "education", "okrug"
  )
  
  model_df <- df %>%
    dplyr::select(all_of(vars_needed)) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(okrug = droplevels(factor(okrug)))
  
  # if no usable observations remain, return NULL (instead of crashing)
  if (nrow(model_df) == 0) {
    message("Skipping ", outcome, ": 0 complete cases.")
    return(NULL)
  }
  
  rhs <- c(
    "period3", "period2min", "protestday", "electday",
    "ur", "urafterprotest", "urpostelection",
    "apolitical", "apolafterprotest", "apolpostelection",
    "wealth", "education"
  )
  
  # only include okrug if it has variation in the estimation sample
  if (nlevels(model_df$okrug) >= 2) {
    rhs <- c(rhs, "okrug")
  }
  
  fml <- as.formula(
    paste(outcome, "~", paste(rhs, collapse = " + "))
  )
  
  lm(fml, data = model_df)
}

mods_table3 <- purrr::map(table2_outcomes, fit_table3)
names(mods_table3) <- table2_outcomes

# keep only models that actually estimated
mods_table3 <- mods_table3[!purrr::map_lgl(mods_table3, is.null)]

modelsummary(
  mods_table3,
  vcov = function(x) sandwich::vcovHC(x, type = "HC1"),
  coef_map = c(
    "period3" = "Post-Protest",
    "period2min" = "Post-Election",
    "protestday" = "Protest Day",
    "electday" = "Election Day",
    "ur" = "United Russia",
    "urafterprotest" = "United Russia x Post-Protest",
    "urpostelection" = "United Russia x Post-Election",
    "apolitical" = "Apolitical",
    "apolafterprotest" = "Apolitical x Post-Protest",
    "apolpostelection" = "Apolitical x Post-Election"
  ),
  coef_omit = "wealth|education|okrug",
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Adj",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  output = "markdown"
)


# FIGURE 2
m_fig2 <- lm(
  trustgovind ~ period3 + period2min + protestday + electday +
    ur + urafterprotest + urpostelection +
    apolitical + apolafterprotest + apolpostelection +
    wealth + education + factor(okrug),
  data = df
)

coef_fig2 <- tidy_robust(m_fig2) %>%
  filter(term %in% c(
    "period3", "period2min", "protestday", "electday",
    "ur", "urafterprotest", "urpostelection",
    "apolitical", "apolafterprotest", "apolpostelection"
  )) %>%
  mutate(
    term = recode(
      term,
      period3 = "Post-Protest",
      period2min = "Post-Election",
      protestday = "Protest Day",
      electday = "Election Day",
      ur = "United Russia",
      urafterprotest = "United Russia x Post-Protest",
      urpostelection = "United Russia x Post-Election",
      apolitical = "Apolitical",
      apolafterprotest = "Apolitical x Post-Protest",
      apolpostelection = "Apolitical x Post-Election"
    ),
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

ggplot(coef_fig2, aes(x = estimate, y = fct_rev(term))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Coefficient estimate",
    y = NULL,
    title = "Interaction Model Estimates"
  ) +
  theme_minimal()


# APPENDIX 1: DISTRICT BALANCE
district_vars <- paste0("okrugdv", 1:10)

# need to create these dummy variables first
okrug_dummies <- model.matrix(~ okrug - 1, data = df) %>%
  as.data.frame()

names(okrug_dummies) <- paste0("okrugdv", seq_len(ncol(okrug_dummies)))

df <- bind_cols(df, okrug_dummies)

app1_p1_p2 <- map_dfr(
  district_vars,
  ~run_ttest(df, .x, "period1min",
             filter_expr = period3 != 1 & electday != 1 & protestday != 1)
) %>% mutate(comparison = "Period 1 vs Period 2")

app1_p1_p3 <- map_dfr(
  district_vars,
  ~run_ttest(df, .x, "period1min",
             filter_expr = period2min != 1 & electday != 1 & protestday != 1)
) %>% mutate(comparison = "Period 1 vs Period 3")

app1_p2_p3 <- map_dfr(
  district_vars,
  ~run_ttest(df, .x, "period3",
             filter_expr = period1min != 1 & electday != 1 & protestday != 1)
) %>% mutate(comparison = "Period 2 vs Period 3")



# APPENDIX 2: COVARIATE BALANCE
balance_vars <- c(
  "age", "male", "education", "wealth", "permres",
  "nationality", "unemp", "statesector", "ur", "comp", "opposition"
)

bal_p1_p2 <- map_dfr(
  balance_vars,
  ~run_ttest(df, .x, "period1min",
             filter_expr = period3 != 1 & electday != 1 & protestday != 1)
) %>% mutate(comparison = "Period 1 vs Period 2")

bal_p1_p3 <- map_dfr(
  balance_vars,
  ~run_ttest(df, .x, "period1min",
             filter_expr = period2min != 1 & electday != 1 & protestday != 1)
) %>% mutate(comparison = "Period 1 vs Period 3")

bal_p2_p3 <- map_dfr(
  balance_vars,
  ~run_ttest(df, .x, "period3",
             filter_expr = period1min != 1 & electday != 1 & protestday != 1)
) %>% mutate(comparison = "Period 2 vs Period 3")



# APPENDIX 3: PARTY COMPOSITION
plot_df2 <- df %>%
  filter(period123 %in% c(1, 2, 3)) %>%
  mutate(
    period123 = factor(period123,
                       levels = c(1, 2, 3),
                       labels = c("Pre-election", "Post-election", "Post-protest"))
  ) %>%
  group_by(period123) %>%
  summarise(
    ur = mean(ur, na.rm = TRUE),
    comp = mean(comp, na.rm = TRUE),
    ldpr = mean(ldpr, na.rm = TRUE),
    opposition = mean(opposition, na.rm = TRUE),
    dk = mean(dk, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-period123, names_to = "party", values_to = "mean_share")

ggplot(plot_df2, aes(x = period123, y = mean_share, fill = party)) +
  geom_col(position = "dodge") +
  labs(
    x = NULL,
    y = "Mean",
    title = "Differences in Partisanship Across Periods"
  ) +
  theme_minimal()



# APPENDIX 4: NONRESPONSE BALANCE
nr_vars <- c(
  "trustgovindnr", "trustarmynr", "trustpolicenr", "trustfsbnr",
  "trustcourtnr", "trustmungovnr", "trustfedgovnr",
  "trustdumanr", "trustprocuracynr", "trustunnr"
)

nr_p1_p2 <- map_dfr(
  nr_vars,
  ~run_ttest(df, .x, "period1min",
             filter_expr = period3 != 1 & electday != 1 & protestday != 1)
) %>% mutate(comparison = "Period 1 vs Period 2")

nr_p1_p3 <- map_dfr(
  nr_vars,
  ~run_ttest(df, .x, "period1min",
             filter_expr = period2min != 1 & electday != 1 & protestday != 1)
) %>% mutate(comparison = "Period 1 vs Period 3")

nr_p2_p3 <- map_dfr(
  nr_vars,
  ~run_ttest(df, .x, "period3",
             filter_expr = period1min != 1 & electday != 1 & protestday != 1)
) %>% mutate(comparison = "Period 2 vs Period 3")



# APPENDIX 5: DAILY RESPONSES
df <- df %>%
  mutate(
    trend = case_when(
      DATA == 25 & MONTH == 11 ~ 1,
      DATA == 26 & MONTH == 11 ~ 2,
      DATA == 27 & MONTH == 11 ~ 3,
      DATA == 28 & MONTH == 11 ~ 4,
      DATA == 29 & MONTH == 11 ~ 5,
      DATA == 30 & MONTH == 11 ~ 6,
      DATA == 1  & MONTH == 12 ~ 7,
      DATA == 2  & MONTH == 12 ~ 8,
      DATA == 3  & MONTH == 12 ~ 9,
      DATA == 4  & MONTH == 12 ~ 10,
      DATA == 5  & MONTH == 12 ~ 11,
      DATA == 6  & MONTH == 12 ~ 12,
      DATA == 7  & MONTH == 12 ~ 13,
      DATA == 8  & MONTH == 12 ~ 14,
      DATA == 9  & MONTH == 12 ~ 15,
      DATA == 10 & MONTH == 12 ~ 16,
      DATA == 11 & MONTH == 12 ~ 17,
      DATA == 12 & MONTH == 12 ~ 18,
      DATA == 13 & MONTH == 12 ~ 19,
      DATA == 14 & MONTH == 12 ~ 20,
      DATA == 15 & MONTH == 12 ~ 21,
      DATA == 16 & MONTH == 12 ~ 22,
      DATA == 17 & MONTH == 12 ~ 23,
      DATA == 18 & MONTH == 12 ~ 24,
      DATA == 19 & MONTH == 12 ~ 25,
      DATA == 20 & MONTH == 12 ~ 26,
      DATA == 21 & MONTH == 12 ~ 27,
      DATA == 22 & MONTH == 12 ~ 28,
      DATA == 23 & MONTH == 12 ~ 29,
      DATA == 24 & MONTH == 12 ~ 30,
      TRUE ~ NA_real_
    )
  )

df <- df %>%
  mutate(trend_factor = factor(trend, levels = 1:30))

# similar to Stata regression that includes Dec 5 onward + dec25 + controls
daily_mod <- lm(
  trustgovind ~ relevel(trend_factor, ref = "1") + education + wealth,
  data = df
)

daily_coefs <- tidy_robust(daily_mod) %>%
  filter(str_detect(term, "trend_factor")) %>%
  mutate(
    day_num = as.numeric(str_extract(term, "\\d+")),
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

ggplot(daily_coefs, aes(x = estimate, y = fct_rev(as.factor(day_num)))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Coefficient estimate relative to Nov 25",
    y = "Day index",
    title = "Daily Responses Model"
  ) +
  theme_minimal()

#####################################################
#####################################################
############## End of code replication ##############
#####################################################
#####################################################





# SAVE TABLES AS HTML

# make output folder
out_dir <- "outputs_html"
if (!dir.exists(out_dir)) dir.create(out_dir)

# helper to save a data frame
save_df_html <- function(df, file, title = NULL) {
  modelsummary::datasummary_df(
    df,
    title = title,
    output = file
  )
}

##### TABLE 1
save_df_html(
  table1_tests,
  file = file.path(out_dir, "table1_ttests.html"),
  title = "Table 1. Welch t-tests by period"
)


# TABLE 2
modelsummary::modelsummary(
  mods_table2,
  vcov = function(x) sandwich::vcovHC(x, type = "HC1"),
  coef_map = c(
    "period3" = "Post-Protest",
    "period2min" = "Post-Election",
    "protestday" = "Protest Day",
    "electday" = "Election Day",
    "wealth" = "Wealth",
    "education" = "Education"
  ),
  coef_omit = "factor\\(okrug\\)",
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Adj",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  title = "Table 2. Main regressions",
  output = file.path(out_dir, "table2_main_regressions.html")
)

# TABLE 3
modelsummary::modelsummary(
  mods_table3,
  vcov = function(x) sandwich::vcovHC(x, type = "HC1"),
  coef_map = c(
    "period3" = "Post-Protest",
    "period2min" = "Post-Election",
    "protestday" = "Protest Day",
    "electday" = "Election Day",
    "ur" = "United Russia",
    "urafterprotest" = "United Russia x Post-Protest",
    "urpostelection" = "United Russia x Post-Election",
    "apolitical" = "Apolitical",
    "apolafterprotest" = "Apolitical x Post-Protest",
    "apolpostelection" = "Apolitical x Post-Election"
  ),
  coef_omit = "wealth|education|okrug",
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Adj",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  title = "Table 3. Heterogeneity by partisanship",
  output = file.path(out_dir, "table3_partisanship.html")
)

# APPENDIX 1: DISTRICT BALANCE
app1_all <- dplyr::bind_rows(app1_p1_p2, app1_p1_p3, app1_p2_p3)

save_df_html(
  app1_all,
  file = file.path(out_dir, "appendix1_district_balance.html"),
  title = "Appendix 1. District balance tests"
)

# APPENDIX 2: COVARIATE BALANCE
app2_all <- dplyr::bind_rows(bal_p1_p2, bal_p1_p3, bal_p2_p3)

save_df_html(
  app2_all,
  file = file.path(out_dir, "appendix2_covariate_balance.html"),
  title = "Appendix 2. Covariate balance tests"
)

# APPENDIX 4: NONRESPONSE BALANCE
app4_all <- dplyr::bind_rows(nr_p1_p2, nr_p1_p3, nr_p2_p3)

save_df_html(
  app4_all,
  file = file.path(out_dir, "appendix4_nonresponse_balance.html"),
  title = "Appendix 4. Nonresponse balance tests"
)

# save raw coefficient tables too (for checking exact estimates)
table2_coef_list <- lapply(mods_table2, function(m) {
  broom::tidy(lmtest::coeftest(m, vcov. = sandwich::vcovHC(m, type = "HC1")))
})
table2_coef_df <- dplyr::bind_rows(table2_coef_list, .id = "model")

save_df_html(
  table2_coef_df,
  file = file.path(out_dir, "table2_raw_coefficients.html"),
  title = "Table 2. Raw coefficient output"
)

table3_coef_list <- lapply(mods_table3, function(m) {
  broom::tidy(lmtest::coeftest(m, vcov. = sandwich::vcovHC(m, type = "HC1")))
})
table3_coef_df <- dplyr::bind_rows(table3_coef_list, .id = "model")

save_df_html(
  table3_coef_df,
  file = file.path(out_dir, "table3_raw_coefficients.html"),
  title = "Table 3. Raw coefficient output"
)

cat("\nSaved HTML tables to folder:", normalizePath(out_dir), "\n")
list.files(out_dir)