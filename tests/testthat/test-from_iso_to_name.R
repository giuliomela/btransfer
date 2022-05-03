test_that("from_iso_to_name works", {

  # checking that the output is a character string and that is not NA

  res <- from_iso_to_name("ITA")

  expect_true(is.character(res))

  expect_false(is.na(res))

  # checking error message

  expect_error(from_iso_to_name("ITE"), "Please provide a valid ISO")

})
