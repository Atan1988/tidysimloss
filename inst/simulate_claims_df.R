library(dplyr)
library(purrr)

data('Policy_df')
policy_required_field_map <- c('PolicyNo' = 'Policy_Number',
                               'EffectiveDate' = 'Eff_dt',
                               'ExpirationDate' = 'Exp_dt',
                               'NumberOfClaims' = 'total_claims')
frequency_alist <- alist(
  total_claims ~ rpois(lambda),
  lambda = a_lambda +  b_lambda_industry * log(Exposures),
  #a_lambda = 0.05,
  b_lambda_industry = case_when(Industries == 'Healthcare' ~ b3
                                , Industries == 'Construction' ~ b2
                                , TRUE ~ b1) #c(0.1, 0.05, 0.025)
)

frequency_params_components_alist <- alist(
  a_lambda ~ rnorm(mean = 0.15, sd = 0.01),
  b1 ~ rnorm(mean = 0.1, sd = 0.02),
  b2 ~ rnorm(mean = 0.075, sd = 0.015),
  b3 ~ rnorm(mean = 0.05, sd = 0.01),
)

severity_init_components_alist <- alist(
  occurrence_lag ~ rdiscrete(options),
  report_lag ~ rtrunc(FUN = 'exp', Att=0, rTrunc = 5 * 365, rate),
  ini_indemn_paid ~ rnorm(mean = 0, sd = 0), #rtrunc(FUN = 'norm', Att = 0, mean = 500, sd = 150),
  ini_expense_paid ~ rnorm(mean = 0, sd = 0), #rtrunc(FUN = 'norm', Att = 0, mean = 500, sd = 50),
  ini_indemn_reserve ~ rlnorm(meanlog = 5, sdlog = 3),
  ini_expense_reserve ~ rlnorm(meanlog = 5, sdlog = 2),
  rate = 1 / 180,
  options = seq(1, 365, 1)
)

severity_transit_components_alist <- alist(
  closing ~ rbernoulli(p = inv_logit(-0.75 + b_close * age)),
  reopen ~ rbernoulli(p = inv_logit(-8)),
  no_change ~ rbernoulli(p = inv_logit(0.5)),
  indemn_reserve_change ~ rlnorm(meanlog = mu_indemn_res, sdlog = sqrt(mu_indemn_res)),
  expense_reserve_change ~ rlnorm(meanlog = mu_expense_res, sdlog = sqrt(mu_expense_res)),
  percent_indemn_reserve_paid ~ rnorm(mean = 0.75, sd = 0.1),
  percent_expense_reserve_paid ~ rnorm(mean = 0.75, sd = 0.1),
  mu_indemn_res = dlnorm(age, meanlog = 3.5, sdlog = 1),
  mu_expense_res = dlnorm(age, meanlog = 2.5, sdlog = 1),
  increment_mth = 3
)

severity_params_components_alist

expr_evaluation(df = Policy_df, expr_alist = frequency_alist,
                params_alist = frequency_params_components_alist) -> claims_df
