#' @export
mayan_date <- function(baktun, katun, tun, winal, kin) {
  # ------------------- Validations ----------------
  ai <- as.logical(lapply(c(baktun, katun, tun, winal, kin), is_integer))
  if(!all(ai)) stop("All entries must be an integer number")
  ai <- as.logical(lapply(c(baktun, katun, tun, winal, kin), function(x) x >= 0)) 
  if(!all(ai)) stop("All entries must be a positive number")
  # -------------------------------------------------
  lg <- long_count_days(baktun, katun, tun, winal, kin)
  haab <- haab_from_base(lg)
  tzolkin <- tzolkin_from_base(lg)
  structure(
    list(
      baktun = baktun,
      katun = katun,
      tun = tun,
      winal = winal,
      kin = kin,
      tzolkin = tzolkin,
      haab = haab
    ),
    class = "mayan_date"
  )
}

setOldClass("mayan_date")

#' @export
print.mayan_date <- function(x, ...) {
  ld <- paste0(
    x$baktun, " ", "baktun", ifelse(x$baktun > 1, "s ", " "), 
    x$katun, " ", "katun", ifelse(x$katun > 1, "s ", ""), 
    x$tun, " ", "tun", ifelse(x$tun > 1, "s ", " "), 
    x$winal, " ", "winal", ifelse(x$winal > 1, "s ", " "), 
    x$kin, " ", "kin", ifelse(x$kin > 1, "s ", " ") 
    )
  haab <-  paste(x$haab$day, x$haab$month_name)
  tzolkin <- paste(x$tzolkin$day_1, x$tzolkin$day_2_name)
  cat(
    paste(ld, tzolkin, haab, sep = " | ")
  )
  invisible(x)
}

long_count_days <- function(baktun, katun, tun, winal, kin) {
  lg <- lg_months()
  sum(c(
    baktun * lg$baktun,
    katun * lg$katun,
    tun * lg$tun,
    winal * lg$winal,
    kin * lg$kin  
  ))
}

lg_months <- function() {
  list(
    baktun = 144000,
    katun = 7200,
    tun = 360,
    winal = 20,
    kin = 1
  )
}

tzolkin_names <- function(){
  c("Imix", "Ikʼ", "Akʼbʼal", "Kʼan", "Chikchan", "Kimi", "Manikʼ", 
    "Lamat", "Muluk", "Ok", "Chuwen", "Ebʼ", "Bʼen", "Ix", "Men", 
    "Kibʼ", "Kabʼan", "Etzʼnabʼ", "Kawak", "Ajaw")
}

haab_names <- function() {
  c("Pop", "Woʼ", "Sip", "Sotzʼ", "Sek", "Xul", "Yaxkʼin", 
    "Mol", "Chʼen", "Yax", "Sak", "Keh", "Mak", "Kʼank'in", 
    "Muwan", "Pax", "Kʼayab", "Kumkʼu", "Wayebʼ")
}

day_to_haab <- function(day_number) {
  mayan_month <- ifelse(day_number > 5, floor(day_number / 20), 0)
  mayan_day <- day_number -(mayan_month * 20)
  list(
    day = as.integer(mayan_day + 1),
    month = as.integer(mayan_month + 1),
    month_name = haab_names()[as.integer(mayan_month + 1)]
  )
}

haab_from_base <- function(no_days) {
  d <- day_rotation(no_days, 365, 347)
  day_to_haab(d)
}

tzolkin_round <- function() {
  data.frame(
    day_no = 1:260,
    day = rep(1:13, 20),
    named_day = rep(1:20, 13),
    day_name = tzolkin_names()[rep(1:20, 13)]
  ) 
}

day_to_tzolkin <- function(day_number) {
  t <- tzolkin_round()[day_number, ]
  list(
    day_1 = t$day,
    day_2 = t$named_day,
    day_2_name = as.character(t$day_name)
  )
}

tzolkin_from_base <- function(no_days) {
  d <- day_rotation(no_days, 260, 160)
  day_to_tzolkin(d)
}
