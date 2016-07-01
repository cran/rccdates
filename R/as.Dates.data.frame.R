
#' @rdname as.Dates
#' @export
as.Dates.data.frame <- function(x, progress_bar = TRUE, ...){
  
  ## Inform that some vectors are already dates
  already_dates <- names(x)[sapply(x, inherits, "Date")]
  if (any(unlist(lapply(x, inherits, "Date")))){
    message("There are already ", sum(unlist(lapply(x, inherits, "Date"))), " Date variables in ", deparse(substitute(x)), ". ",
            "We will try to find more!")
  }
  ## Inform that some vectors are already years
  already_years <- names(x)[sapply(x, is.year)]
  if (any(sapply(x, class) == "year")){
    message("There are already ", table(unlist(lapply(x, class)))[["year"]], " year variables in ", deparse(substitute(x)), ". ",
            "We will try to find more!")
  }
  
  x_original <- x

  ## Progress bar only for large data sets
  if (progress_bar) progress_bar  <- prod(dim(x)) > 1e5
  
  # The actual transformation
  if (progress_bar){
    message("\nIdentifying dates ...\n")
    pb <- utils::txtProgressBar(0, ncol(x), style = 3)
  }
    for (i in seq_along(x)){
      x[[i]] <- suppressMessages(as.Dates(x[[i]], name = names(x)[i], ask = TRUE, ...))
      if (progress_bar) utils::setTxtProgressBar(pb, i)
    }
  if (progress_bar) close(pb)
  
  
  # Give warning if no variables changed
  if (identical(x, x_original)) {
    warning("No variables coerced to dates!")
  
  ## If some variables were coerced to dates, names of these are reported  
  } else{
    
    ## Identified date variables:
    dates_after <- names(x)[sapply(x, inherits, "Date")]
    new_dates <- dates_after[!(dates_after %in% already_dates)]
    if (!identical(new_dates, character(0))){
      message("The following variables were recognised as potential dates and therefore coerced to such: \n* ",
              paste(new_dates, collapse = "\n* ")
              )
    }
    
    ## Identified year variables:
    years_after <- names(x)[sapply(x, is.year)]
    new_years <- years_after[!(years_after %in% already_years)]
    if (!identical(new_years, character(0))){
      message("\nThe following variables were recognised as potential year variables and therefore coerced to such (see '?as.year'): \n* ",
              paste(new_years, collapse = "\n* ")
      )
    }
  }
  
  x
}