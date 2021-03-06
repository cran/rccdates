% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internals.R
\name{as.yyww}
\alias{as.yyww}
\alias{as.yyyyww}
\title{Coerce yyww-variable to a valid date.}
\usage{
as.yyww(x, format_check = TRUE)

as.yyyyww(x, format_check = TRUE)
}
\arguments{
\item{x}{a vector that can be coerced to a date of the form yV (yyww) or YV (yyyyww)}

\item{format_check}{Should the format of \code{x} be checked to be in correct 
format before we try to change it? \code{TRUE} by default. Set to \code{FALSE}
if the format is already checked outside the function to avoid multiple checks 
(that could be time consuming for big data sets).}
}
\value{
a date vector
}
\description{
Coerce yyww-variable to a valid date.
}

