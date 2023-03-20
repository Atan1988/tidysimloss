#' @title cdf propotion calc
#' @description helper function to calculate the cdfs in between various qs
#' @param q a vector of numbers
#' @param distr distribution name, for example 'lnorm" for lognormal
#' @param ...  additional distrbituion parameter of the p[distr] function, such as meanlog and sdlog for lnorm
#' @export
cdf_proportion <- function(q, distr = 'exp', ...) {

  p <-  purrr::exec(.fn = paste0('p', distr), q=q, ...)
  p1 <- p - dplyr::lag(p, default = 0)
  p1 / sum(p1)
}
