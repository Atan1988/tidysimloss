N_Policies <- 1e3
Eff_yrs <- seq(2010, 2016, 1)
Industries <- c('Energy', 'Construction', 'Healthcare')
Exposures <- alist(Exposures ~ rnorm(mean, sd),
                   mean = a + b_Industries[Industries],
                   sd = 3e6, b_Industries = c(10e6, 25e6, 0), a = 50e6)


policy_alist <- alist(
  Exposures ~ rnorm(mean, sd),
  mean = a + b_Industries[Industries],
  sd = 3e6, b_Industries = c(10e6, 25e6, 0), a = 50e6
)

policy_parameters_alist <- alist(
  Industries ~ rdiscrete(Industry_options),
  Eff_yr ~ rdiscrete(Eff_yrs),
  Industry_options = c('Energy', 'Construction', 'Healthcare'),
  Eff_yrs = seq(2010, 2016, 1)
)

policy_df <- tibble::tibble(Policy_Number = seq(1, N_Policies, 1))
policy_df <- policy_df %>%
  ###evaluate number of claims
  expr_evaluation(expr_alist = policy_alist,
                  params_alist = policy_parameters_alist)

Policy_sim  <- function(N_Policies, Eff_yrs, Industries, Exposures ) {

  Policy_df <- tibble::tibble(
    Policy_Number = seq(1, N_Policies, 1),
    Industries = sample(Industries,  N_Policies, replace = T),
    Eff_yr = sample(Eff_yrs, N_Policies, replace = T)
  ) %>%
  dplyr::mutate(Industries = factor(Industries, levels = !!Industries))

  Policy_df <- Policy_df %>%
     dplyr::mutate(Eff_dt = as.Date(paste0(Eff_yr, '-01-01')) +
                     sample(seq(1, 365, 1), N_Policies, replace = T)) %>%
     dplyr::mutate(Exp_dt = Eff_dt + lubridate::period(1,"years"))


  expr_evaluation(df = Policy_df, expr_alist = Exposures)
}


Policy_df <- Policy_sim(N_Policies, Eff_yrs, Industries, Exposures )
