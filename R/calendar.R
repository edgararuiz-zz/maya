period_factors <- function() {
  list(
    baktun = 144000,
    katun = 7200,
    tun = 360,
    winal = 20,
    kin = 1
  )
}

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
  if (year == 0) is_leap_year <- FALSE
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
  if (bce_1 != bce_2) from <- from + 1

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

#' Converts a gregorian date to mayan long count date
#'
#' It converts a gregorian calendar date into mayan long count date. It expects
#' 4 arguments: year number, month number, day number, and if the date is Before 
#' Common Era (BCE). It expects a calendar year not as an astronomical year.  
#' Dates before year 1, have to be passed by setting bce to TRUE.
#'  
#' It returns a vector with 5 named values. The lowest count is kin, which is equivalent 
#' to a day. 20 days, or kins is a winal. 18 winals is a tun. 20 tuns is a katun. 
#' 20 katuns is a baktun.  This means that a baktun represents a cycle of 144,000 days 
#' (https://en.wikipedia.org/wiki/Maya_calendar).
#' 
#' The count begins from what has been determined to be the equivalent of mayan long count
#' date 0.0.0.0.0.  The equivalent in the Gregorian calendar is August 11 3114 BCE.
#'
#' @param year A positive integer
#' @param month A positive integer
#' @param day A positive integer
#' @param bce Logical variable, indicates if the date is Before Common Era
#'
#' @examples
#'
#' # August 11 3114 BCE
#' gregorian_to_mayan(3114, 8, 11, TRUE)
#' # May 19 143 CE
#' gregorian_to_mayan(143, 5, 19, FALSE)
#' 
#' @export
gregorian_to_mayan <- function(year, month, day, bce) {
  pl <- period_factors()
  from_origin <- diff_days(3114, 8, 11, TRUE, year, month, day, bce)
  baktun <- floor((from_origin / pl$baktun))
  katun <- floor((from_origin - (baktun * pl$baktun)) / pl$katun)
  tun <- floor((from_origin - (baktun * pl$baktun) - (katun * pl$katun)) / pl$tun)
  winal <- floor((from_origin - (baktun * pl$baktun) - (katun * pl$katun) - (tun * pl$tun)) / pl$winal)
  kin <- floor((from_origin - (baktun * pl$baktun) - (katun * pl$katun) - (tun * pl$tun) - (winal * pl$winal)))
  d <- c(baktun, katun, tun, winal, kin)
  names(d) <- c("baktun", "katun", "tun", "winal", "kin")
  d
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
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  total_cycles <- ceiling(no_days / 365) * 2
  if (bce) year <- -(year - 1)
  dte <- days_to_eoy(year, month, day)
  if (no_days > dte) {
    no_days <- no_days - dte
    for (i in seq_len(total_cycles)) {
      yr <- year + i
      year_days <- 365 + is_leap_year(yr)
      if (no_days >= year_days) {
        no_days <- no_days - year_days
      } else {
        break()
      }
    }
    full_years <- year + i
    if (is_leap_year(yr)) month_days[[2]] <- 29
    for (i in seq_along(month_days)) {
      nd <- no_days - month_days[i]
      if (nd >= 0) {
        no_days <- nd
      } else {
        break()
      }
    }
    full_months <- i
    full_days <- no_days
  } else {
    yr <- year
    full_years <- year
    sel <- month_days[month:12]
    sel[1] <- sel[1] - day
    for (i in seq_along(sel)) {
      if (sel[i] >= no_days) {
        full_months <- month + i - 1
        if (full_months == month) {
          full_days <- day + no_days
        } else {
          full_days <- no_days
        }
        break()
      } else {
        no_days <- no_days - sel[i]
      }
    }
  }
  is_bce <- FALSE
  if (full_years < 0) {
    ({
      full_years <- full_years - 1
      is_bce <- TRUE
    })
  }
  if (full_years > 0 && bce) full_days <- full_days - 1
  d <- c(abs(full_years), full_months, full_days, is_bce)
  names(d) <- c("year", "month", "day", "is_bce")
  d
}

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
#' mayan_to_gregorian(0, 0, 0, 0, 0)
#' mayan_to_gregorian(13, 0, 0, 0, 0)
#' 
#' @export
mayan_to_gregorian <- function(baktun, katun, tun, winal, kin) {
  pl <- period_factors()
  total_days <- (baktun * pl$baktun) + (katun * pl$katun) + (tun * pl$tun) + (winal * pl$winal) + (kin * pl$kin)
  add_days(3114, 8, 11, TRUE, total_days)
}
