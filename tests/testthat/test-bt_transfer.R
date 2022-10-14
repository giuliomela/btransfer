test_that("bt_transfer works", {

  sample_policy <- c("Chile", "United States", "China",
                        "Italy", "France", "Libya")

  sample_study <- c("Mexico", "United States", "European Union",
                    "India", "Australia")

  agg <- c("med", "nor", "bal", "blk", "atl")

  res1 <- lapply(sample_study, function(x) bt_transfer(policy_site = sample_study,
                                           study_site = x))

  res2 <- lapply(c("yes", "no", "row"), function(x) bt_transfer(policy_site = sample_study,
                                                       aggregate_policy = x))

  res3 <- lapply(c("USD", "EUR"), function(x) bt_transfer(policy_site = "Italy",
                                                               study_currency = x))

  res4 <- lapply(c("USD", "EUR"), function(x) bt_transfer(policy_site = sample_study,
                                                                 policy_currency = x))

  res5 <- lapply(c(2010, 2019, 2030, 2040), function(x) bt_transfer(policy_site = sample_study,
                                                                 policy_yr = x))

  res6 <- lapply(2015:2019, function(x) bt_transfer(policy_site = sample_study,
                                                    study_yr = x))

  res7 <- lapply(sample_policy, function (x) bt_transfer(policy_site = x,
                                                         policy_currency = "LCU"))

  res8 <- mapply(function (x, y) bt_transfer(study_site = x, policy_site = y, aggregate_policy = "yes"),
                 agg, agg)

  res_list <- list(res1, res2, res3, res4, res5, res6, res7)

  res_list <- lapply(res_list, function(x) do.call("rbind", x))

  res_df <- do.call("rbind", res_list)

  # testing that the output is always a tibble

  sapply(res_list, function(x) expect_output(str(x), "tibble"))

  # testing that transfer factor is not an NA (in all the above examples it must not be NA)

  sapply(res_df$bt_fct, function(x) expect_false(is.na(x)))

  sapply(res8[nrow(res8), ], function(x) expect_false(is.na(x)))

  expect_error(bt_transfer("med", "med"))

})
