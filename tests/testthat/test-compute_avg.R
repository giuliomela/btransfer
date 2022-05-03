test_that("compute_avg works", {

  # checking that the output is a numeric

  expect_numeric <- function(var, type) {

    expect_true(is.numeric(compute_avg(iso = "ITA", start_yr = 2010,
                            end_yr = 2019, var = var, type = type)))
  }

  vars <- c("gdp", "gdp_capita", "gni", "pop", "death_rate", "gni_capita")

  types <- c("levels", "growth_rt")

  all_vars <- expand.grid(var = vars, type = types)

  sapply(vars, function(x) expect_numeric(var = x, type = "levels"))

  sapply(vars, function(x) expect_numeric(var = "gdp_capita", type = "growth_rt"))

  # checking error messages

  expect_error(compute_avg(iso = "ITA", start_yr = 2010,
                           end_yr = 2019, var = "gdp_capita", type = "level"),
               "Please provide a valid type variable")

  expect_error(compute_avg(iso = "ITA", start_yr = 2010,
                           end_yr = 2009, var = "gdp_capita", type = "levels"),
               "'end_yr' cannot be earlier or the same as 'start_yr'")

  expect_error(compute_avg(iso = "ITA", start_yr = 2010,
                           end_yr = 2019, var = "gdp_capite", type = "levels"),
               "Please provide
                                                                   a valid variable name")

  max_yr <- max(btransfer::wb_series$year)

  min_yr <- min(btransfer::wb_series$year)

  expect_error(compute_avg(iso = "ITA", start_yr = 2010,
                           end_yr = max_yr + 1, var = "gdp_capite", type = "levels"))


})
