period_factors <- function() {
  list(
    baktun = 144000,
    katun = 7200,
    tun = 360,
    winal = 20,
    kin = 1
  )
}

#' @export
is_leap <- function(yr) {
  is_leap <- FALSE
  div_400 <- yr / 400 == floor(yr / 400) # Leap year
  div_100 <- yr / 100 == floor(yr / 100) # Not leap year
  div_4 <- yr / 4 == floor(yr / 4) # Leap year
  if (div_400) is_leap <- TRUE
  if (div_100 && !div_400) is_leap <- FALSE
  if (div_4 && !div_100) is_leap <- TRUE
  if (yr == 0) is_leap <- FALSE
  is_leap
}

days_to_eoy <- function(year, month, day) {
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  roy <- month_days
  roy[2] <- roy[2] + is_leap(year)
  roy <- roy[month:12]
  roy[1] <- roy[1] - day
  sum(roy)
}

days_to_boy <- function(year, month, day) {
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  roy <- month_days
  roy[2] <- roy[2] + is_leap(year)
  roy <- roy[1:month]
  roy[month] <- day
  sum(roy)
}

days_in_year <- function(year, month, day, month1, day1) {
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  roy <- month_days
  roy[2] <- roy[2] + is_leap(year)
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
    leap_years <- sum(as.integer(lapply(in_between, is_leap)))
    bulk_days <- (length(in_between) * 365) + leap_years
  }
  td <- from + bulk_days + to
  if (one_is_recent) td <- (-td)
  td
}

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
      year_days <- 365 + is_leap(yr)
      if (no_days >= year_days) {
        no_days <- no_days - year_days
      } else {
        break()
      }
    }
    full_years <- year + i
    if (is_leap(yr)) month_days[[2]] <- 29
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

#' @export
mayan_to_gregorian <- function(baktun, katun, tun, winal, kin) {
  pl <- period_factors()
  total_days <- (baktun * pl$baktun) + (katun * pl$katun) + (tun * pl$tun) + (winal * pl$winal) + (kin * pl$kin)
  add_days(3114, 8, 11, TRUE, total_days)
}
