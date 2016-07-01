

#' Converting all potential dates in a data.frame
#' 
#' This function takes a data.frame, checks all its variables for potential
#' dates and transform potential date variables to dates.
#' 
#' The function only recognise dates in the standard format used in INCA and
#' Rockan, hence '%Y-%m-%d', '%Y%m%d' and '%y%V' (where the last format is the
#' week number sometimes used for older data). We hope that this older format
#' can be omited in the future since it adds some unclarity if the vector is
#' really a date or not). Dates in other formats (for example as parts of social
#' security numbers, as part of a comment, or as a time stamp) will not be
#' treated as dates.
#' 
#' @section Slow call: Note that the function call can be very slow for big data
#'   sets (each individual cell is checked). You probably only run this function
#'   once per dataset and could hopefully live with this drawback :-)
#'   
#' @param x a data.frame, numeric vector or character vector, possibly with
#'   potential date variable(s)
#' @param progress_bar Should a progress bar be printed to show the progress of
#'   the call? \code{TRUE} by default.
#' @param rtr Should dates in formats found in RTR be recognised as dates. This
#'   controls \code{yyww, yyyyww, yyyy0000, yyyymm00} but each of them can also
#'   be set individually. \code{TRUE} by default.
#' @param yyww,yyyyww Should entries of the old yyww/yyyyww-formats be
#'   recognised as dates? Same as \code{rtr} by default.
#' @param yyyy0000,yyyymm00 Should entries with unknown month and/or day (set to
#'   zero) be accepted as dates? If yes, dates in the form "yyyy0000" will be
#'   set to "yyyy0701" and "yyyymm00" to "yyyymm15". Same as \code{rtr} by
#'   default.
#' @param ask,name arguments passed to \code{\link{exceed_threshold}}
#' @param ... arguments passed to \code{\link[rccmisc]{exceed_threshold}}
#' @return the given input with potantial date variable(s) converted to date(s)
#'   and other variable(s) intact
#' @examples
#' 
#' # Let's say we have a data.frame with one date column and one column that is almost
#' # (but not excactly) a date:
#' test_data <- data.frame(
#'                  not_date = c("19121212", "1912-12-12", "2014-01-01", "121212"),
#'                  date     = c(19121212, "1912-12-12", "2014-01-01", "6405"))
#' as.Dates(test_data) # Only recognizes the "date" column as date
#' 
#' # Inform that the "not_date" column might also be a date candidate if fixed:
#' as.Dates(test_data, threshold = .5)
#' 
#' # Force the not_date column to date
#' as.Dates(test_data, threshold = .5, force = TRUE)
#' @seealso \link{as.Date}
#' @name as.Dates
#' @export
as.Dates <- function(x, ...){
  UseMethod("as.Dates")
}