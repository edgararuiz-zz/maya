#' Determines if it is a leap year
#'
#' It follows the established rules to figure out if is a leap year 
#' (https://en.wikipedia.org/wiki/Leap_year).  It uses astronomial
#' year numbering for years before the Gregorian calendar was established.  
#' (https://en.wikipedia.org/wiki/Astronomical_year_numbering)
#' 
#' The year is designated as leap if: the year number is divisible by 400,
#' or if the year number is divisible by 4, but it is not divisible by
#' 100.
#'
#' @param year The year to evaluate. It expects a integer number. It
#' also expects the astronomical number of years older than 1. For example,
#' to evaluate year 401 BCE, use -400.
#'
#' @examples
#'
#'is_leap_year(2012)
#'is_leap_year(-401)
#'# To evaluate multiple years
#'as.integer(lapply(2000:2005, is_leap_year))
#'
#' @export
is_leap_year <- function(year) {
  is_leap_year <- FALSE
  div_400 <- year / 400 == floor(year / 400) # Leap year
  div_100 <- year / 100 == floor(year / 100) # Not leap year
  div_4 <- year / 4 == floor(year / 4) # Leap year
  if (div_400) is_leap_year <- TRUE
  if (div_100 && !div_400) is_leap_year <- FALSE
  if (div_4 && !div_100) is_leap_year <- TRUE
  if (year == 0) is_leap_year <- TRUE
  is_leap_year
}
