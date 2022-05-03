test_that("compute_growth_rate works", {

  vect <- 1

  for (i in 2:10) {

    vect[i] <- round(stats::runif(1) * 10)

  }

  res <- compute_growth_rate(vect)

  expect_length(res, length(vect)) # result must be a vector of the same size of input vector

  expect_true(is.na(res[1])) # first element must be NA

  sapply(res[-1], function(x) expect_true(is.numeric(x))) # all other elements numeric

})
