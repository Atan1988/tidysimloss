policy_df
policy_required_field_map <- c('PolicyNo' = 'Policy_Number',
                               'EffectiveDate' = 'Eff_dt',
                               'ExpirationDate' = 'Exp_dt')
frequency_alist <- alist(
  total_claims ~ rpois(lambda),
  occurrence_lag ~ rdiscrete(options),
  report_lag ~ rtrunc(FUN = 'exp', Att=0, rTrunc = 5 * 365, rate),
  rate = 1 / 180,
  options = seq(1, 365, 1)
)
severity_init_components_alist
severity_transit_components_alist
severity_params_components_alist
