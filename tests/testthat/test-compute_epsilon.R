test_that("compute_epsilon works", {

  # checking that the output is numeric
  expect_true(is.numeric(compute_epsilon(20000)))

  expect_false(is.na(compute_epsilon(20000)))


})
