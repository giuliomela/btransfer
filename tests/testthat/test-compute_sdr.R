test_that("compute_sdr works", {

  countries <- c("Italia", "Alemania", "France", "Polska")

  yrs <- c(2010, 2019, 2000)

  agg <- c("yes", "no", "row")

  grid <- expand.grid(yrs = yrs, agg = agg)

  res <- mapply(compute_sdr, policy_yr = grid$yrs, aggregate = grid$agg,
                MoreArgs = list(countries = countries), SIMPLIFY = FALSE) # a list

  res_df <- do.call("rbind", res)

  # checking that all the results are tibbles/data.frames

  sapply(res, function (x) expect_true(is.data.frame(x)))

  # checking that all numbers in the tibble are indeed numbers

  for (i in names(res_df)[3:6]) {

    expect_true(is.numeric(res_df[[i]]))

  }

  # checking that there are no NA values

  for (i in names(res_df)[3:6]) {

    expect_false(any(is.na(res_df[[i]])))

  }

})
