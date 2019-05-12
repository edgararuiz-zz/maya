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
add_days <- function(x, no_days) UseMethod("add_days")

#' @export
add_days.Date <- function(x, no_days) {
  yr <- as.integer(format(x, "%Y"))
  bce <- FALSE
  if(yr < 0) {
    yr <- abs(yr) + 1
    bce <- TRUE}
  mn <- as.integer(format(x, "%m"))
  dy <- as.integer(format(x, "%d"))
  add_days2(yr, mn, dy, bce, no_days)
}

#' @export
add_days.gregorian_date <- function(x, no_days) {
  add_days2(x$year, x$month, x$day, x$bce, no_days)
}

add_days2 <- function(year, month, day, bce, no_days) {
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
  gregorian_date(abs(full_year), full_months, full_days, is_bce)
}
