#' @export
mayan_to_gregorian <- function(x) UseMethod("mayan_to_gregorian")

#' @export
mayan_to_gregorian.mayan_date <- function(x) {
  d <- add_days2(3114, 8, 11, 1, x$from_origin) 
  gregorian_date(d[[1]], d[[2]], d[[3]], d[[4]])
}

#' @export
mayan_to_gregorian.character <- function(x) {
  sp <- strsplit(x, "\\.")
  sp <- lapply(sp, as.integer)
  sp <- sp[[1]]
  if(length(sp) != 5) stop("Function needs exactly five long count numbers")
  my <- mayan_date(sp[1], sp[2], sp[3],sp[4], sp[5])
  mayan_to_gregorian(my)
}

#' @export
gregorian_to_mayan <- function(x) UseMethod("gregorian_to_mayan")

#' @export
gregorian_to_mayan.gregorian_date <- function(x) {
  pl <- lg_months()
  from_origin <- diff_days2(3114, 8, 11, TRUE, x$year, x$month, x$day, x$bce)
  baktun <- floor((from_origin / pl$baktun))
  katun <- floor((from_origin - (baktun * pl$baktun)) / pl$katun)
  tun <- floor((from_origin - (baktun * pl$baktun) - (katun * pl$katun)) / pl$tun)
  winal <- floor((from_origin - (baktun * pl$baktun) - (katun * pl$katun) - (tun * pl$tun)) / pl$winal)
  kin <- floor((from_origin - (baktun * pl$baktun) - (katun * pl$katun) - (tun * pl$tun) - (winal * pl$winal)))
  mayan_date(baktun, katun, tun, winal, kin)
}

#' @export
gregorian_to_mayan.Date <- function(x) {
 x <- as_gregorian_date(x) 
 gregorian_to_mayan(x)
}
