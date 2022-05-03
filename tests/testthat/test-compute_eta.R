test_that("compute_eta works", {

  # testing that the result is numeric

  res <- compute_eta("ITA", 2019, 10, 1.35)

  expect_true(is.numeric(res))

  expect_false(is.na(res))


})
