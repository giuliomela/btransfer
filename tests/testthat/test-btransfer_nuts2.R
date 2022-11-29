test_that("btransfer_nuts2 works", {

  study_site <- "Unione Europea"

  policy_site <- c("Umbria", "Puglia", "Lombardia")

  results <- btransfer_nuts2(study_site = study_site,
                             policy_site = policy_site,
                             policy_yr = 2019,
                             ref_yr = 2019)

  expect_true(is.numeric(results$trans_fct))


})
