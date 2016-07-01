

##########################################################################################
#                                                                                        #
#                                     as/is-methods                                      #
#                                                                                        #
##########################################################################################


#' Year vectors
#' 
#' Creates or coerces objects of type \code{year}.
#' 
#' The year class also inherits methods from the character class but not from
#' numeric. (It does not make any sence to, for example, multiply two years with
#' each other). There are however methods for subtraction and it is also
#' possible to add a integer to a year etcetera.
#' @param x object to be coerced or tested
#' @param a,b years to be added or subtracted
#' @return Vector of class \code{year} and "AsIs" (see function \code{\link{I}}).
#' @export
#' @name as.year
#' @examples
#' as.year("2012")
#' as.year(Sys.Date())
#' rccmisc::width(c(2012, 2014))
as.year <- function(x){
  x <- UseMethod("as.year")
  I(x)
}

#' @export
as.year.year <- function(x){
  x
}

#' @rdname as.year
#' @export
as.year.Date <- function(x){
  as.year(format(x, format = "%Y"))
}

#' @rdname as.year
#' @export
as.year.default <- function(x){
  x <- as.character(substring(x, 1, 4)) 
  if(all(is_year(x), na.rm = TRUE)){
    x[x == ""] <- NA
    return(structure(x, class = c("AsIs", "year", "character")))
  } else{
    stop("'x' can not be coerced to year!")
  }
}

#' @rdname as.year
#' @export
is.year <- function(x){
  inherits(x, "year")
}



##########################################################################################
#                                                                                        #
#                                       S3 methods                                       #
#                                                                                        #
##########################################################################################

#' @export
`[<-.year` <- function(x, ..., value){
  if (!all(is_year(value), na.rm = TRUE)){
    stop("You can not insert non year values into a year vector! ",
         "First transform your year object to character/numeric etc.")
  }
  NextMethod()
}


#' @export
`+.year` <- function(a, b){
  stopifnot(rccmisc::is.wholenumber(b))
  as.year(as.numeric(a) + b)
}



#' @rdname as.year
#' @export
`-.year`    <- function(a, b) as.numeric(a) - as.numeric(b)

#' @rdname as.year
#' @export
width.year  <- function(x) rccmisc::width(as.numeric(x))

#' @export
as.data.frame.year <- as.data.frame.vector
#' @export
`[.year` <- rccmisc::create_s3_method("[")
#' @export
`min.year` <- rccmisc::create_s3_method("min")
#' @export
`max.year` <- rccmisc::create_s3_method("max")
#' @export
range.year <- rccmisc::create_s3_method("range")
#' @export
rep.year <- rccmisc::create_s3_method("rep")
#' @export 
print.year <- rccmisc::create_s3_print(as.character)