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
diff_days <- function(from_date, to_date) UseMethod("diff_days")

#' @export
diff_days.Date <- function(from_date, to_date) {
  d1 <- date_split(from_date)
  d2 <- date_split(to_date)
  diff_days2(
    d1$year, d1$month, d1$day, d1$bce,
    d2$year, d2$month, d2$day, d2$bce
  )
}

#' @export
diff_days.gregorian_date <- function(from_date, to_date) {
  diff_days2(
    from_date$year, from_date$month, from_date$day, from_date$bce,
    to_date$year, to_date$month, to_date$day, to_date$bce
  )
}

diff_days2 <- function(year_1, month_1, day_1, bce_1,
                      year_2, month_2, day_2, bce_2) {
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  nd1 <- date_as_number(year_1, month_1, day_1, bce_1)
  nd2 <- date_as_number(year_2, month_2, day_2, bce_2)
  
  if(bce_1) year_1 <- -(year_1 - 1)
  if(bce_2) year_2 <- -(year_2 - 1)
  
  if(nd1 >= nd2) {
    year_a <- year_2; month_a <- month_2; day_a <- day_2
    year_b <- year_1; month_b <- month_1; day_b <- day_1
    negative <- TRUE
  } else {
    year_a <- year_1; month_a <- month_1; day_a <- day_1
    year_b <- year_2; month_b <- month_2; day_b <- day_2
    negative <- FALSE
  }
  
  if(year_a == year_b) {
    if(month_a == month_b) {
      res <- day_b - day_a
    } else {
      md <- month_days
      if(is_leap_year(year_a)) month_days[[2]] + 1
      md <- md[month_a:month_b]
      md[[1]] <- md[[1]] - day_a
      md[length(md)] <- day_b
      res <- sum(md)
    }
  } else {
    yrs <- as.integer(lapply(year_a:year_b, is_leap_year))
    yrs <- 365 + yrs
    md <- month_days
    md[[1]] <- md[[1]] + is_leap_year(year_a)
    md <- md[month_a:12]
    md[[1]] <- md[[1]] - day_a
    yrs[[1]] <- sum(md)
    md <- month_days
    md[[2]] <- md[[2]] + is_leap_year(year_b)
    md <- md[1:month_b]
    md[[length(md)]] <- day_b
    yrs[[length(yrs)]] <- sum(md)
    res <- sum(yrs)
  }
  if(negative) res <- -(res)
  res
}