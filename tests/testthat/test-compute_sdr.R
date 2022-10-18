test_that("compute_sdr works", {

  countries <- c("Italia", "Alemania", "France", "Polska")

  agg <- c("yes", "row")

  res1 <- compute_sdr(country = "Italia")

  res2 <- sapply(agg, function(x) compute_sdr(
    country = countries,
    agg = x
  ))

  # checking that all returned sdr are indeed numeric values

  sapply(c(res1, res2), function(x) expect_true(is.numeric(x)))

})
