
#' as.Date with format = "\%Y-\%m-\%d"
#' 
#' This is just a wrapper for the standard INCA date format
#' @param ... arguments passed to \code{\link{as.Date}}
#' @export
#' @examples
#' asDate(c("", "", "2014-10-10"))
asDate <- function (...){
  as.Date(..., format = "%Y-%m-%d")
}