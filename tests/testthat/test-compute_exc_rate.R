test_that("compute_exc_rate works", {

  res <- compute_exc_rate("USA", "EMU", 2019)

  res1 <- compute_exc_rate("IND", "MEX", 2019)

  sapply(c(res, res1), function(x) expect_true(is.numeric(x)))

  sapply(c(res, res1), function(x) expect_false(is.na(x)))


})
