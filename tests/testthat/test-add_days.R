context("add_days")

test_that("Special add_days cases", {
  expect_equivalent(add_days(gregorian_date(3114, 8, 11, TRUE), 1)$astronomical, "-3113-08-12")
  expect_equivalent(add_days(gregorian_date(3114, 8, 11, TRUE), 30)$astronomical, "-3113-09-10")
  expect_equivalent(add_days(gregorian_date(3114, 8, 11, TRUE), 1872000)$astronomical, "2012-12-21")
  expect_equivalent(add_days(as.Date("2012-01-01"), 1)$astronomical, "2012-01-02")
})

test_that("Special diff_day cases", {
 expect_equal(
   diff_days(
     gregorian_date(2014, 2, 5, FALSE), 
     gregorian_date(2000, 2, 4, FALSE)
     ), -5114
   )
 expect_equal(
   diff_days(
     gregorian_date(2012, 12,21, FALSE), 
     gregorian_date(3114,8,11, TRUE)
     ), -1872000
   )
 expect_equal(
   diff_days(
     gregorian_date(2014, 2, 5, FALSE), 
     gregorian_date(2014, 3, 4, FALSE)
     ), 27
   )
 expect_equal(
   diff_days(as.Date("2012-01-01"), as.Date("2012-01-10"))
   , 9)
})

# test_that("Mayan to gregorian converts correctly", {
#   expect_equivalent(mayan_to_gregorian2( 0,  0, 0,  0,  0), c(3114,  8, 11, TRUE))
#   expect_equivalent(mayan_to_gregorian2(13,  0, 0,  0,  0), c(2012, 12, 21, FALSE))
#   expect_equivalent(mayan_to_gregorian2( 7, 16, 6, 16, 18), c(  32,  9,  1, TRUE))
#   expect_equal(mayan_to_gregorian("0.0.0.0.0"), "August 11, 3114 BCE")
#   expect_equal(mayan_to_gregorian("13.0.0.0.0"), "December 21, 2012 CE")
#   expect_equal(mayan_to_gregorian("0.0.0.0.0", "astronomical"), "-3113/8/11")
#   expect_equal(mayan_to_gregorian("0.0.0.0.0", "number"), -31130811)
# })

# test_that("Gregorian to mayan converts correctly", {
#   expect_equivalent(gregorian_to_mayan2(3114,  8, 11, TRUE),  c( 0,  0, 0,  0,  0))
#   expect_equivalent(gregorian_to_mayan2(2012, 12, 21, FALSE), c(13,  0, 0,  0,  0))
#   expect_equivalent(gregorian_to_mayan2(  32,  9,  1, TRUE),  c( 7, 16, 6, 16, 18))
#   expect_equal(gregorian_to_mayan("August 11, 3114 BCE"), "0.0.0.0.0")
#   expect_equal(gregorian_to_mayan("December 21, 2012 CE"), "13.0.0.0.0")
# })




