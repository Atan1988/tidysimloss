#' @title  seudo p function
#' @name pseudo
#' @description  this is a pseudo p function, that is used when p function not available
#' @param Att attachment
#' @param ... additional arguments
#' @export
pseudo <- function(Att, ...) {
  if (is.infinite(Att) & Att > 0) return(1) else return(0)
}


### this module includes generic Truncated functions
#' @title  trunc d function
#' @name dtrunc
#' @description  d truncation function
#' @param Att attachment
#' @param FUN distribution
#' @param rTrunc right truncation
#' @param ... additional arguments
#' @export
dtrunc <- function(x, FUN, Att=0, rTrunc=Inf, ...) {
  d <- get(paste("d", FUN, sep=""))
  if (!exists(paste0("p", FUN))) {
    #cat(paste0("p", FUN), " doesn't exist! ignoring truncation ", "\n")
    p <- pseudo
  } else {
    p <- get(paste("p", FUN, sep=""))
  }

  Sd <- 1 - p(Att, ...)
  Sr <- 1 - p(rTrunc, ...)

  return(d(x, ...)/(Sd - Sr))
}


#' @title  trunc p function
#' @name ptrunc
#' @description  p truncation function
#' @param Att attachment
#' @param FUN distribution
#' @param rTrunc right truncation
#' @param ... additional arguments
#' @export
ptrunc <- function(x, FUN, Att=0, rTrunc=Inf, ...) {
  d <- get(paste("d", FUN, sep=""))

  if (!exists(paste0("p", FUN))) {
    #cat(paste0("p", FUN), " doesn't exist! ignoring truncation ", "\n")
    p <- pseudo
  } else {
    p <- get(paste("p", FUN, sep=""))
  }

  Sd <- 1 - p(Att, ...)
  Sr <- 1 - p(rTrunc, ...)
  return(( p(x, ...) - p(Att, ...)) /(Sd - Sr))
}

#' @title  trunc q function
#' @name qtrunc
#' @description  q truncation function
#' @param Att attachment
#' @param FUN distribution
#' @param rTrunc right truncation
#' @param ... additional arguments
#' @export
qtrunc <- function(x, FUN, Att=0, rTrunc=Inf, ...) {
  d <- get(paste("d", FUN, sep=""))

  if (!exists(paste0("p", FUN))) {
    #cat(paste0("p", FUN), " doesn't exist! ignoring truncation ", "\n")
    p <- pseudo
  } else {
    p <- get(paste("p", FUN, sep=""))
  }

  q <- get(paste("q", FUN, sep=""))

  Sd <- 1 - p(Att, ...)
  Sr <- 1 - p(rTrunc, ...)
  return(q( x * (Sd - Sr) + (1 - Sd), ...))
}

#' @title  trunc r function
#' @name rtrunc
#' @description  r truncation function
#' @param Att attachment
#' @param FUN distribution
#' @param rTrunc right truncation
#' @param ... additional arguments
#' @export
rtrunc <- function(n, FUN, Att=0, rTrunc = Inf, ...) {
    if (length(Att)==1) {
       qtrunc(runif(n, min=0.5/n, max=(n-0.5)/n), FUN=FUN, Att=Att, rTrunc=rTrunc, ...)
    } else {
       n <- length(Att)
       qtrunc(runif(n, min=0.5/n, max=(n-0.5)/n), FUN=FUN, Att=Att, rTrunc=rTrunc, ...)
    }
}



