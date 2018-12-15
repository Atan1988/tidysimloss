#' @export
logit <- function(x) {
  make.link('logit')$linkfun(x)
}

#' @export
inv_logit <- function(x) {
  make.link('logit')$linkinv(x)
}

