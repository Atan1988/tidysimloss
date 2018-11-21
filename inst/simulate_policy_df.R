N_Policies <- 1e3
Industries <- c('Energy', 'Construction', 'Healthcare')
Exposures <- alist(Exposures ~ rnorm(mean, sd),
                   mean = a + b_Industries[Industries],
                   sd = 3e6, b_Industries = c(10e6, 25e6, 0), a = 50e6)


Policy_sim  <- function(N_Policies, Industries, Exposures ) {

  Policy_df <- tibble::tibble(
    Policy_Number = seq(1, N_Policies, 1),
    Industries = sample(Industries,  N_Policies, replace = T)
  ) %>%
  dplyr::mutate(Industries = factor(Industries, levels = !!Industries))
}
