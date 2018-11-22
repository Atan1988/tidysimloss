policy_df
policy_required_field_map <- c('PolicyNo' = 'Policy_Number',
                               'EffectiveDate' = 'Eff_dt',
                               'ExpirationDate' = 'Exp_dt')
frequency_alist <- alist(
  total_claims ~ rpois(lambda),
  occurrence_lag ~ rdiscrete(options),
  report_lag ~ ,
)
severity_init_components_alist
severity_transit_components_alist
severity_params_components_alist
