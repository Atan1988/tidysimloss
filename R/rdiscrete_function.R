#' @title discrete sampling
#' @name rdiscrete
#' @param n number of obs
#' @param options options to be sampled
#' @export
rdiscrete  <- function(n, options) {
  sample(x = options, size = n, replace = T)
}
