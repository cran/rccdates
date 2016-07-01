#' Lead time from one date to another
#' 
#' @param from,to start and stop dates (in formats that can be coerced by
#'   \code{\link{as.Dates}}).
#' @param neg default value for negative lead times. \code{NULL} means that 
#'   negative lead times are kept as is. \code{NA} by default (changes
#'   negative values to \code{NA}). Any numerical value is accepted
#' @param as name of the class to be assigned to x. Default is \code{numeric}
#'   but could also be set to for example \code{\link{difftime}} or \code{\link{integer}}.
#' @return
#' A vector of class \code{as} (\code{numeric} by default).
#'   
#' @export
#' @name lt
#' @examples
#' lt(from = Sys.Date(), to = Sys.Date() + 10)

lt <- function(from, to, neg = NA, as = "numeric"){
  stopifnot(is.na(neg) || is.null(neg) || is.numeric(neg))
  x <- difftime(as.Dates(to), as.Dates(from), units = "days")
  if (!is.null(neg)) x[x < 0] <- neg
  as(x, as)
}

#' @export
#' @rdname lt
leadtime <- lt