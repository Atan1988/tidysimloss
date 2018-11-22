policy_df
policy_required_field_map <- c('PolicyNo' = 'Policy_Number',
                               'EffectiveDate' = 'Eff_dt',
                               'ExpirationDate' = 'Exp_dt')
frequency_alist <- alist(
  total_claims ~ rpois(lambda),
  lambda = a + b[Industries] * log(Exposures),
  a = 0.05,
  b = c(0.1, 0.05, 0.025)
)
severity_init_components_alist <- alist(
  occurrence_lag ~ rdiscrete(options),
  report_lag ~ rtrunc(FUN = 'exp', Att=0, rTrunc = 5 * 365, rate),
  rate = 1 / 180,
  options = seq(1, 365, 1)
)
severity_transit_components_alist
severity_params_components_alist

expr_evaluation(df = Policy_df, expr_alist = frequency_alist)
