day_rotation <- function(no_days, cycle_size, start_from) {
  w_nd <- start_from + no_days
  full <- floor(w_nd / cycle_size) * cycle_size
  w_nd - full
}

day_name <- function(year_no, month_no, day_no, bce) {
  day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  no_days <- diff_days2(2019, 5, 12, 0, year_no, month_no, day_no, bce)
  new_day <- day_rotation(no_days, 7, 6) + 1
  list(
    name = day_names[new_day],
    number = new_day
  )
}

is_integer <- function(number) {
  floor(number) == number
}

date_as_number <- function(year, month, day, bce) {
  if(bce) year <- year - 1
  year <- as.character(year)
  month <- as.character(month)
  day <- as.character(day)
  year <- ifelse(nchar(year) < 2, paste0(rep(0, 2 - nchar(year)), year), year)
  month <- ifelse(nchar(month) < 2, paste0(rep(0, 2 - nchar(month)), month), month)
  day <- ifelse(nchar(day) < 2, paste0(rep(0, 2 - nchar(day)), day), day)
  x <- as.numeric(paste0(year, month, day))
  ifelse(bce, -x, x)
}

date_split <- function(x) {
  yr <- as.integer(format(x, "%Y"))
  bce <- FALSE
  if(yr < 0) {
    yr <- abs(yr) + 1
    bce <- TRUE
    }
  mn <- as.integer(format(x, "%m"))
  dy <- as.integer(format(x, "%d"))
  list(
    year = yr,
    month = mn,
    day = dy,
    bce = bce
  )
}
