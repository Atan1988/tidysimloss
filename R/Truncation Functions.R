### this is a pseudo p function, that is used when p function not available
pseudo <- function(Att, ...) {
  if (is.infinite(Att) & Att > 0) return(1) else return(0)
}


### this module includes generic Truncated functions
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

#' @export
rtrunc <- function(n, FUN, Att=0, rTrunc = Inf, ...) {
    if (length(Att)==1) {
       qtrunc(runif(n, min=0.5/n, max=(n-0.5)/n), FUN=FUN, Att=Att, rTrunc=rTrunc, ...)
    } else {
       n <- length(Att)
       qtrunc(runif(n, min=0.5/n, max=(n-0.5)/n), FUN=FUN, Att=Att, rTrunc=rTrunc, ...)
    }
}



