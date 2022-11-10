test_that("btransfer works", {

  sample_policy <- c("Chile", "United States", "China",
                        "Italy", "France", "Libya")

  sample_study <- c("Mexico", "United States", "European Union",
                    "India", "Australia")

  agg <- c("med", "nor", "bal", "blk", "atl")

  res1 <- lapply(sample_study, function(x) btransfer(policy_site = "Unione Europea",
                                           study_site = x, policy_yr = 2021,
                                           ref_yr = 2021,
                                           policy_currency = "EMU"))

  res2 <- lapply(c("yes", "row"), function(x) btransfer(policy_site = sample_policy,
                                                              study_site = "Unione Europea", policy_yr = 2021,
                                                              ref_yr = 2021,
                                                              policy_currency = "EMU",
                                                              agg_policy = x))

  res3 <- lapply(c("USA", "EMU"), function(x) btransfer(policy_site = sample_policy,
                                                        study_site = "Unione Europea", policy_yr = 2021,
                                                        ref_yr = 2021,
                                                        policy_currency = "EMU",
                                                        study_currency = x,
                                                        agg_policy = "row"))

  res4 <- lapply(c("USA", "EMU"), function(x) btransfer(policy_site = sample_policy,
                                                        study_site = "Unione Europea", policy_yr = 2021,
                                                        ref_yr = 2021,
                                                        policy_currency = x,
                                                        agg_policy = "yes"))

  res5 <- lapply(c(2010, 2019, 2030, 2040), function(x) btransfer(policy_site = "Mexico",
                                                                  study_site = "Unione Europea", policy_yr = x,
                                                                  ref_yr = 2021,
                                                                  policy_currency = "MEX",
                                                                  agg_policy = "no"))

  res6 <- lapply(2015:2019, function(x) btransfer(policy_site = "Italia",
                                                  study_site = "Unione Europea",
                                                  study_yr = x,
                                                  policy_yr = 2021,
                                                  ref_yr = 2021,
                                                  policy_currency = "EMU",
                                                  agg_policy = "no"))

  res7 <- mapply(function (x, y) btransfer(study_site = x, policy_site = y, agg_policy = "yes",
                                             policy_yr = 2021, ref_yr = 2021, policy_currency = "EMU"),
                 agg, agg)

  res_all <- c(res1, res2, res3, res4, res5, res6, res7)

  # testing that the output is always a tibble

  sapply(res_all, function(x) expect_output(str(x), "num"))

  # testing that transfer factor is not an NA (in all the above examples it must not be NA)

  sapply(res_all, function(x) expect_false(is.na(x)))

  expect_error(transfer(study_site = "med", policy_site = "med", agg_policy = "no",
                        policy_yr = 2021, ref_yr = 2021, policy_currency = "EMU"))

})
