


################################################################################
#                                                                              #
#                               Helper functions                               #
#                                                                              #
################################################################################


######## Functions checking for potential year, month and day of month #########
# This is not so sofisticated but the purpose is just to make a rough test.
# Further testing is later done using R:s standard date functions.
is_year  <- function(pot_year_val, years = 1800:(as.numeric(format(Sys.Date(), format = "%Y")) + 20)){
  rccmisc::is_numeric(pot_year_val) & 
    suppressWarnings(rccmisc::as_numeric(pot_year_val)  %in% c(NA, years))
}  
is_month <- function(pot_month_val, zero = FALSE){
  MONTHS <- if (zero) c(NA, 0:12) else c(NA, 1:12)
  rccmisc::is_numeric(pot_month_val) & 
    suppressWarnings(rccmisc::as_numeric(pot_month_val) %in% MONTHS)
}
is_day   <- function(pot_day_val, zero = FALSE){
  DAYS <- if (zero) c(NA, 0:31) else c(NA, 1:31)
  rccmisc::is_numeric(pot_day_val) & 
    suppressWarnings(rccmisc::as_numeric(pot_day_val)   %in% DAYS)
}
is_date  <- function(pot_date, yyyy0000 = FALSE, yyyymm00 = FALSE){
  nchar(as.character(pot_date)) == 8 &
    rccmisc::is_numeric(pot_date) &
    is_year(substr(pot_date, 1, 4))  &
    is_month(substr(pot_date, 5, 6), zero = yyyy0000) &
    is_day(substr(pot_date, 7, 8), zero = yyyy0000 || yyyymm00)
}

# Is the variable in the old yyww-format?
is_yyww <- function(x, years_allowed = 50:99){
  
  x <- as.character(x)
  
  !identical(x, character(0)) & # Not empty string
    rccmisc::is_numeric(x) & # should be numeric
    nchar(x) == 4  &
    substr(x, 1, 2) %in% as.character(years_allowed) &
    substr(x, 3, 4) %in% c(paste0("0", 1:9), 10:53)
}

# Is the variable in the old yyyyww-format?
is_yyyyww <- function(x, ...){
  x <- as.character(x)
  nchar(x) == 6 & 
    substr(x, 1, 2) == "19" & 
    is_yyww(substr(x, 3, 6), ...)
}


#' Coerce yyww-variable to a valid date.
#'
#' @param x a vector that can be coerced to a date of the form yV (yyww) or YV (yyyyww)
#' @param format_check Should the format of \code{x} be checked to be in correct 
#' format before we try to change it? \code{TRUE} by default. Set to \code{FALSE}
#' if the format is already checked outside the function to avoid multiple checks 
#' (that could be time consuming for big data sets).
#' @return a date vector
#' @name as.yyww

as.yyww <- function(x, format_check = TRUE){
  
  if (format_check) stopifnot(all(is_yyww(x)))
  
  x    <- as.character(x)
  year <- paste0("19", substr(x, 1, 2))
  week <- as.numeric(substr(x, 3, 4))
  
  as.Date(paste0(year, "-01-01")) + 
    (as.integer(format(as.Date(paste0(year, "-01-03")), "%w")) + 6) %% 7 + 7 * (week - 1) - 1
}

#' @rdname as.yyww
as.yyyyww <- function(x, format_check = TRUE){
  if (format_check) stopifnot(all(is_yyyyww(x)))
  as.yyww(substr(as.character(x), 3, 6))
}


## Version of as.Date that also handles day and month 00
as_Date <- function(x, ...){
  stopifnot(is_date(x, ...))
  x <- as.character(x)
  Y <- substr(x, 1, 4)
  m <- ifelse(substr(x, 5, 6) == "00", "07", substr(x, 5, 6))
  d <- ifelse(substr(x, 7, 8) == "00", "15", substr(x, 7, 8))
  as.Date(paste0(Y, m, d), format = "%Y%m%d")
}


## If variable (with "-" excluded) is numeric, it should be character without scientific format
as_numeric_character <- function (x) {
  if (all(rccmisc::is_numeric(gsub("-", "", as.character(x))), na.rm = TRUE)){
    x <- as.character(format(x, scientific = FALSE))
    x <- gsub(" ", "", x) # trim (needed because extra spacec added to ensure non scientific notation)
    x <- gsub("NA", NA, x)
  } else{
    x <- as.character(x)
  }
  x <- gsub("-", "", x) # Transform possible RTR-dates to INCA-format 
  x <- gsub(" ", "", x) # trim (needed because extra spacec added to ensure non scientific notation)
  x
}
