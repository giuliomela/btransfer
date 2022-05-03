test_that("compte_gdp_dfl works", {

  res <- sapply(c("EMU", "USA", "MEX", "IND"), function(x)
    compute_gdp_dfl(x, 2019))

  sapply(res, function(x) expect_true(is.numeric(x)))

  sapply(res, function(x) expect_false(is.na(x)))

})
