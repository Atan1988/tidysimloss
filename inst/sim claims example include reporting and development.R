library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(tidysimloss)
library(flexsurv)
library(LaplacesDemon)

policy_alist <- alist(
  Exposures ~ rtrunc(FUN = 'norm', Att = 5e5, mean, sd),
  mean = a + b_Industries[Industries]
)

policy_parameters_alist <- alist(
  ded ~ rdiscrete(ded_list),
  limit ~ rdiscrete(limit_list),
  Effdt ~ rdiscrete(Effdt_list),
  Industries ~ rdiscrete(Industry_options),
  Industry_options = c('Energy', 'Construction', 'Healthcare'),
  Effdt_list =seq(lubridate::as_date('2010-01-01'), lubridate::as_date('2016-12-31'), 1),
  sd = 10e6, b_Industries = c(10e6, 25e6, 0), a = 50e6,
  ded_list = c(0, 2500, 5000, 25000, 50000, 100000),
  limit_list = c(1e6, 2e6, 3e6)
)


frequency_alist <- alist(
  total_claims ~ rpois(lambda),
  lambda = a_lambda * Exposures^b_lambda_industry,
  #a_lambda = 0.05,
  b_lambda_industry = case_when(Industries == 'Healthcare' ~ b3
                                , Industries == 'Construction' ~ b2
                                , TRUE ~ b1) #c(0.1, 0.05, 0.025)
)

frequency_params_components_alist <- alist(
  a_lambda ~ rnorm(mean = 0.3, sd = 0.00),
  b1 ~ rnorm(mean = 0.1, sd = 0.00),
  b2 ~ rnorm(mean = 0.075, sd = 0.00),
  b3 ~ rnorm(mean = 0.05, sd = 0.00)
)

severity_alist <- alist(
  loss ~ rlnorm(meanlog = mu, sdlog = 2.5),
  occurrence_lag ~ rdiscrete(options),
  report_lag ~ rtrunc(FUN = 'exp', Att=0, rTrunc = 5 * 365, rate),
  mu = a_mu + b_mu,
  b_mu = case_when(Industries == 'Healthcare' ~ b3
                   , Industries == 'Construction' ~ b2
                   , TRUE ~ b1),
  rate = 1 / 180,
  options = seq(1, 365, 1)
)

severity_params_alist <- alist(
  a_mu ~ rnorm(mean = 11, sd = 0.00),
  b1 ~ rnorm(mean = 0, sd = 0.00),
  b2 ~ rnorm(mean = 1, sd = 0.00),
  b3 ~ rnorm(mean = -1, sd = 0.00)
)


N_Policies <- 1e3
full_data <- base_simulator(N_Policies,
                            policy_exprs = policy_alist, policy_parameters = policy_parameters_alist,
                            frequency_exprs = frequency_alist,
                            frequnecy_parameters = frequency_params_components_alist,
                            severity_exprs = severity_alist, severity_paramters = severity_params_alist)


full_data1 <- full_data %>% dplyr::filter(freq2 == 0) %>%
  dplyr::mutate(
    scale = ifelse(Industries == "Energy", 20, 40),
    shape = 1.2,
    DOL = Effdt + occurrence_lag,
    DateReported = DOL + report_lag
  )
# alpha0 <- 0.5
# alphas <- c(0.2, 0.4, 0.25, 0.1, 0.05) * alpha0

trans <- full_data1 %>%
  purrrlyr::by_row(
    function(row) {
      tmp_df <-   tibble::tibble(
        q = seq(3, 180, 3)
        , cdf_p_mu = cdf_proportion(q = q, distr = 'llogis', shape = row$shape, scale = row$scale)
      ) %>%
        dplyr::mutate(
          cdf_p = LaplacesDemon::rdirichlet(1, alpha =cdf_p_mu * 0.5) %>% as.vector()
        )
    }
  )


trans %>% tidyr::unnest(cols = '.out') %>%
  dplyr::mutate(trans = round(cdf_p * loss, 0)) %>%
  dplyr::filter(trans > 0) %>%
  dplyr::arrange(
    ClaimNo, q
  ) %>%
  View()

