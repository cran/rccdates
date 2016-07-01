#' @rdname as.Dates
#' @export
as.Dates.default <- function(x, rtr = TRUE, yyww = rtr, yyyyww = rtr, 
                             yyyy0000 = rtr, yyyymm00 = rtr, ask = FALSE, name = "x", ...){
  
  ## Keep original x for output if not date
  stopifnot(is.atomic(x))
  x_orig <- x
    
  ## Return x asis if just empty
  if (all(is.na(x) | x == "")){
    return(x)
    
  ## Return as year if possible
  } else if (all(is_year(x_orig), na.rm = TRUE)){
    return(as.year(x_orig))
  }
  
  
  x <- as_numeric_character(x)
  x[is.na(x)] <- ""
    
  # A helper function to identify weeks in the old format
  identify_dates <- function (out_vec, is_f, as_f, ...) {
    naout <- is.na(out_vec)
    pot <- is_f(x[naout]) # index for cells that are yyww
    if (any(pot)){
      out_vec[naout][pot] <- 
        as.character(as_f(x[naout][pot], ...)) 
    }
    out_vec
  }
  
  
  ## Check all cells of x if they match a date format
  ## The code is messy but hopefully more efiicient than a for loop 
  ## (which would be clearer to read)
  out_vec <- rep(NA_character_, length(x))
  # Empty cells
  out_vec[x == ""] <- ""
  if (yyww)   out_vec <- identify_dates(out_vec, is_yyww, as.yyww, format_check = FALSE)
  if (yyyyww) out_vec <- identify_dates(out_vec, is_yyyyww, as.yyyyww, format_check = FALSE)
              out_vec <- identify_dates(out_vec, function(d) {is_date(d, yyyy0000, yyyymm00)}, as_Date, yyyy0000, yyyymm00)
  
  ## We would like to test if x_orig can be transformed to a date
  transformed <- suppressMessages(as.Date(out_vec, format = "%Y-%m-%d"))
  
  ## If it can, that transformation is returned, otherwise o_orig is returned unchanged
  rccmisc::exceed_threshold(x_orig, transformed, ask = ask, var_name = name, ...)
}


