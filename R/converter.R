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



days_to_eoy <- function(year, month, day) {
  roy <-  c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  roy[2] <- roy[2] + is_leap_year(year)
  roy <- roy[month:12]
  roy[1] <- roy[1] - day
  sum(roy)
}

days_to_boy <- function(year, month, day) {
  roy <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  roy[2] <- roy[2] + is_leap_year(year)
  roy <- roy[1:month]
  roy[month] <- day
  sum(roy)
}

days_in_year <- function(year, month, day, month1, day1) {
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  roy <- month_days
  roy[2] <- roy[2] + is_leap_year(year)
  if (month != month1) {
    roy <- roy[month:month1]
    roy[month] <- roy[month] - day
    roy[month1] <- day1
    sum(roy)
  } else {
    day1 - day
  }
}

date_as_number <- function(year, month, day, bce) {
  year <- as.character(year)
  month <- as.character(month)
  day <- as.character(day)
  year <- ifelse(nchar(year) < 2, paste0(rep(0, 2 - nchar(year)), year), year)
  month <- ifelse(nchar(month) < 2, paste0(rep(0, 2 - nchar(month)), month), month)
  day <- ifelse(nchar(day) < 2, paste0(rep(0, 2 - nchar(day)), day), day)
  x <- as.numeric(paste0(year, month, day))
  ifelse(bce, -x, x)
}

#' Number of days between two gregorian dates
#'
#' It determines the number of days between two gregorian dates.  It works 
#' independently from any R date/time function. An advantage of this function 
#' is that it is accepts dates older than year 1 CE.  It uses calendar and 
#' not astronomical year numbering. 
#'
#' @param year_1 A positive integer
#' @param month_1 A positive integer
#' @param day_1 A positive integer
#' @param bce_1 Logical variable, indicates if the date is Before Common Era
#' @param year_2 A positive integer
#' @param month_2 A positive integer
#' @param day_2 A positive integer
#' @param bce_2 Logical variable, indicates if the date is Before Common Era
#' 
#' @examples
#'
#' diff_days(3114, 8, 11, TRUE, 2012, 12, 21, FALSE)
#' 
#' @export
diff_days <- function(year_1, month_1, day_1, bce_1,
                      year_2, month_2, day_2, bce_2) {
  number_date1 <- date_as_number(year_1, month_1, day_1, bce_1)
  number_date2 <- date_as_number(year_2, month_2, day_2, bce_2)
  
  one_is_recent <- number_date1 > number_date2
  
  if (bce_1) year_1 <- -(year_1 - 1)
  if (bce_2) year_2 <- -(year_2 - 1)
  
  if (year_1 == year_2) {
    return(days_in_year(
      year_1, month_1, day_1, month_2, day_2
    ))
  }
  adj_1 <- -1
  adj_2 <- -1
  if (one_is_recent) {
    from <- days_to_eoy(year_2, month_2, day_2)
    to <- days_to_boy(year_1, month_1, day_1)
    adj_2 <- 1
  } else {
    from <- days_to_eoy(year_1, month_1, day_1)
    to <- days_to_boy(year_2, month_2, day_2)
    adj_1 <- 1
  }
  #if (bce_1 != bce_2) from <- from + 1
  
  adj_year_1 <- year_1 + adj_1
  adj_year_2 <- year_2 + adj_2
  bulk_days <- 0
  if (one_is_recent) {
    add_bulk <- adj_year_2 < adj_year_1
  } else {
    add_bulk <- adj_year_2 > adj_year_1
  }
  if (add_bulk) {
    in_between <- (year_2 + adj_2):(year_1 + adj_1)
    leap_years <- sum(as.integer(lapply(in_between, is_leap_year)))
    bulk_days <- (length(in_between) * 365) + leap_years
  }
  td <- from + bulk_days + to
  if (one_is_recent) td <- (-td)
  td
}

#' Adds a number of days to a date
#'
#' It adds a number of days to the passed date, and returns a new date. It
#' performs the calculations independently from any R date/time function.
#' An advantage of this function is that it is accepts dates older than year 
#' 1 CE.  It uses calendar and not astronomical year numbering.  It only
#' adds days, passing a negative no_days returns an error.
#'
#' @param year A positive integer
#' @param month A positive integer
#' @param day A positive integer
#' @param bce Logical variable, indicates if the date is Before Common Era
#' @param no_days A positive integer. The number of days to count from the
#' date passed
#'
#' @examples
#'
#' add_days(3114, 8, 11, TRUE, 1872000)
#' 
#' # These should return the same date
#' as.Date("2000-03-03") + 1000
#' add_days(2000, 3, 3, FALSE, 1000)
#' 
#' @export
add_days <- function(year, month, day, bce, no_days) {
  if (no_days < 0) stop("Only positive values are allowed for no_days")
  
  adj_year <- ifelse(bce, -(year -1), year)
  
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  rmd <- month_days
  rmd[2] <- rmd[2] + is_leap_year(adj_year)
  rmd <- rmd[month:12]
  rmd[1] <- rmd[1] - day 
  rmd_agg <- as.integer(lapply(seq_along(rmd), function(x) sum(rmd[1:x])))
  rmd_over <- sum(rmd_agg < no_days)
  
  if(rmd_over == 0) {
    full_days <- no_days + day
    full_months <- month 
    full_year <- year
    is_bce <- bce
  } else {
    if(rmd_over < length(rmd_agg)) {
      rmd_full <- rmd_agg[rmd_over]
      full_days <- no_days - rmd_full
      full_months <- month + rmd_over
      full_year <- year
      is_bce <- bce
    } else {
      days_left <- no_days - rmd_agg[length(rmd_agg)]
      cycles <- ceiling(days_left / 365)
      yr_cycles <- seq_len(cycles) + adj_year
      ydays <- as.integer(lapply(yr_cycles, function(x) 365 + is_leap_year(x)))
      ydays_agg <- as.integer(lapply(seq_along(ydays), function(x) sum(ydays[1:x])))
      ydays_over <- sum(ydays_agg < days_left)
      curr_year <- adj_year + ydays_over + 1
      rmd <- month_days
      rmd[2] <- rmd[2] + is_leap_year(curr_year)
      rmd_agg <- as.integer(lapply(seq_along(rmd), function(x) sum(rmd[1:x])))
      curr_days <- days_left - ifelse(ydays_over > 0, ydays_agg[ydays_over], 0) 
      rmd_over <- sum(rmd_agg < curr_days)
      full_months <- rmd_over + 1
      full_days <- curr_days - ifelse(rmd_over > 0, rmd_agg[rmd_over],0)
      full_year <- ifelse(curr_year <= 0, -(curr_year - 1), curr_year)
      is_bce <- ifelse(curr_year <= 0, TRUE, FALSE)
    }
  }
  d <- c(abs(full_year), full_months, full_days, is_bce)
  names(d) <- c("year", "month", "day", "is_bce")
  d
}

day_rotation <- function(no_days, cycle_size, start_from) {
  w_nd <- start_from + no_days
  full <- floor(w_nd / cycle_size) * cycle_size
  w_nd - full
}
