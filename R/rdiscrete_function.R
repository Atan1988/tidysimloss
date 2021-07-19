#' @title discrete sampling
#' @name rdiscrete
#' @param n number of obs
#' @param options options to be sampled
#' @export
rdiscrete  <- function(n, options) {
  p <- sample(x = options, size = n, replace = T)
  if(is.character(options)) {
    p <- factor(p, levels = options)
  }
  return(p)
}
