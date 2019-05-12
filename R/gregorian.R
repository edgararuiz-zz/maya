#' @export
gregorian_date <- function(year_no, month_no, day_no, bce) {
  # ------------------- Validations ----------------
  ai <- as.logical(lapply(c(year_no, month_no, day_no), is_integer))
  if(!all(ai)) stop("Year, month and day must be an integer number")
  ai <- as.logical(lapply(c(year_no, month_no, day_no), function(x) x > 0)) 
  if(!all(ai)) stop("Year, month and day must be a positive number")
  bce <- as.logical(bce)
  if(month_no > 12) stop("Month entry not valid. Valid numbers are between 1 and 12")
  if(day_no > 31) stop("Day entry not valid. Valid numbers are between 1 and 31")
  # -------------------------------------------------
  dn <- day_name(year_no, month_no, day_no, bce)
  structure(
    list(
      year = year_no,
      month = month_no,
      day = day_no,
      bce = bce,
      day_name = dn
    ),
    class = "gregorian_date"
  )
}

setOldClass("gregorian_date")

#' @export
print.gregorian_date <- function(x, ...) {
  cat(
    paste0(
      x$day_name$name, " ", 
      month.name[x$month], " ", 
      x$day, ", ", 
      x$year," ", 
      ifelse(x$bce, "BCE", "CE"))
    )
  invisible(x)
}

#' @export
as_gregorian_date <- function(x) UseMethod("as_gregorian_date")

#' @export
as_gregorian_date.Date <- function(x) {
  yr <- as.integer(format(x, "%Y"))
  bce <- FALSE
  if(yr < 0) {
    yr <- abs(yr) + 1
    bce <- TRUE}
  mn <- as.integer(format(x, "%m"))
  dy <- as.integer(format(x, "%d"))
  gregorian_date(yr, mn, dy, bce)
}
