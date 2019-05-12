#' Converts mayan long count date to gregorian date
#'
#' It converts a long count mayan calendar entry into a gregorian calendar date. It 
#' returns a single vector with 4 values: year number, month number, day number, 
#' and if the date is Before Common Era (BCE). The year is returned as a calendar year,
#' not as an astronomical year.  The year number will always be positive, the logical BCE 
#' variable indicates if the year is under year 1.
#'  
#' It expects 5 integer numbers representing each of the day cycles in the long count
#' calendar. The lowest count is kin, which is equivalent to a day. 20 days, or kins
#' is a winal. 18 winals is a tun. 20 tuns is a katun. 20 katuns is a baktun.  This means
#' that a baktun represents a cycle of 144,000 days (https://en.wikipedia.org/wiki/Maya_calendar).
#' 
#' The count begins from what has been determined to be the equivalent of mayan long count
#' date 0.0.0.0.0.  The equivalent in the Gregorian calendar is August 11 3114 BCE.
#'
#' @param baktun The number of 144,000 day cycles 
#' @param katun The number of 7,200 day cycles
#' @param tun The number of 360 day cycles
#' @param winal The number of 20 day cycles
#' @param kin The number of days
#'
#' @examples
#'
#' mayan_to_gregorian2(0, 0, 0, 0, 0)
#' mayan_to_gregorian2(13, 0, 0, 0, 0)
#' 
#' @export
mayan_to_gregorian2 <- function(baktun, katun, tun, winal, kin) {
  pl <- period_factors()
  total_days <- (baktun * pl$baktun) + (katun * pl$katun) + (tun * pl$tun) + (winal * pl$winal) + (kin * pl$kin)
  add_days(3114, 8, 11, TRUE, total_days)
}

#' Converts mayan long count date to gregorian date
#'
#' @param date Character vector 
#' @param output Expected output
#' 
#' @examples
#'
#' mayan_to_gregorian("0.0.0.0.0")
#' mayan_to_gregorian("13.0.0.0.0")
#' 
#' @export
mayan_to_gregorian <- function(date = NULL, 
                               output = c("calendar", "named_vector", "astronomical", "number")) {
  if(!is.character(date)) stop("Expects the date as a character")
  if(!(output[[1]] %in% c("named_vector", "calendar", "astronomical", "number"))) 
    stop(paste0(output, " not a valid output option"))
  sp <- strsplit(date, "\\.")
  sp <- lapply(sp, as.integer)
  sp <- sp[[1]]
  if(length(sp) != 5) stop("Function needs exactly five long count numbers")
  m <- mayan_to_gregorian2(sp[1], sp[2], sp[3],sp[4], sp[5])
  if(output[[1]] == "named_vector") 
    return(m)
  if(output[[1]] == "calendar") 
    m <- paste0(month.name[m[2]], " ",  m[[3]], ", ", m[[1]], ifelse(m[[4]], " BCE", " CE"))
  if(output[[1]] == "astronomical") 
    m <- paste0(ifelse(m[[4]], paste0("-", m[[1]] - 1), m[[1]]), "/", m[[2]], "/", m[[3]])
  if(output[[1]] == "number") 
    m <- as.integer(date_as_number(m[1], m[2], m[3], m[4]))
  m
}

#' @export
gregorian_to_mayan <- function(x) UseMethod("gregorian_to_mayan")

#' @export
gregorian_to_mayan.gregorian_date <- function(x) {
  pl <- period_factors()
  from_origin <- diff_days(3114, 8, 11, TRUE, x$year, x$month, x$day, x$bce)
  baktun <- floor((from_origin / pl$baktun))
  katun <- floor((from_origin - (baktun * pl$baktun)) / pl$katun)
  tun <- floor((from_origin - (baktun * pl$baktun) - (katun * pl$katun)) / pl$tun)
  winal <- floor((from_origin - (baktun * pl$baktun) - (katun * pl$katun) - (tun * pl$tun)) / pl$winal)
  kin <- floor((from_origin - (baktun * pl$baktun) - (katun * pl$katun) - (tun * pl$tun) - (winal * pl$winal)))
  
  haab <- haab_from_base(from_origin)
  h <- haab_names()
  haab[[3]] <- h[haab[[1]]]
  
  tzolkin <- tzolkin_from_base(from_origin)
  t <- tzolkin_names()
  tzolkin[[3]] <- t[tzolkin[[1]]]
  
  d <- c(baktun, katun, tun, winal, kin, haab)
  names(d) <- c("baktun", "katun", "tun", "winal", "kin", "haab_month", "haab_day", "haab_name")
  d
}

#' @export
gregorian_to_mayan.Date <- function(x) {
 x <- as_gregorian_date(x) 
 gregorian_to_mayan(x)
}



