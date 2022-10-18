test_that("compute_macro_var works", {

  # computing the function over all possible variables - single country

  vars <- c("gdp", "gdp_capita", "gni", "pop", "gni_capita", "death_rate")

  res <- NULL

  for (i in vars) {

    res[[i]] <- compute_macro_var("ITA", 2018, i)[["value"]]

  }

  # computing the function over all possible variables - aggregates

  agg <- c("yes", "row")

  res1 <- NULL

  for (i in agg) {

    res1[[i]] <- compute_macro_var(c("ITA", "FRA", "USA", "IND", "ESP"), 2018,
                                   agg = i)[["value"]]

  }

  # computing the function for years into the future

  res2 <- sapply(2050, function(x) compute_macro_var("ITA", x)[["value"]])

  res3 <- compute_macro_var("ITA", 2020, avg = TRUE)[["avg_value"]]

  res4 <- compute_macro_var("ITA", 2020, growth_rt = TRUE)[["growth_rate"]]

  res_all <- c(res, res1, res2, res3, res4)

  sapply(res_all, function (x) expect_true(is.numeric(x))) # result must be numeric

  sapply(res_all, function (x) expect_false(is.na(x))) # result must not be NA

  # checking error messages

  expect_error(compute_macro_var("ITA", 2051), "Please provide a valid year. Year
                                              must be between 1961 and 2050")

  expect_error(compute_macro_var("ITA", 2018, "gdp_capite"), "Please provide a valid variable name")

  expect_error(compute_macro_var(c("ITA", "FRA"), 2018), "Multiple countries can be
                                                         selected only if 'agg' option
                                                         is set to either 'yes' or 'row'")


})
