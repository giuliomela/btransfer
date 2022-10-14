#' Compute exchange rates between two currencies in given year
#'
#' This function computes nominal exchange rates between two custom currencies for
#' a given year. Exchange rate data are from the World Bank database. Euro is
#' denoted with the code `EMU`. For US dollar use `USA`.
#'
#' @param cur_from A string. Country code (ISO, 3 digits) to which the origin
#' currency belongs.
#' @param cur_to A string. Country code (ISO, 3 digits) to which the destination
#' currency belongs.
#' @param ref_yr A double. Year for which the exchange rate must be calculated
#' @return A double. Exchange rate for the currency pair and the year selected.
#' @source \url{https://data.worldbank.org/}
compute_exc_rate <- function (cur_from, cur_to, ref_yr) {

  iso3c <- year <- NULL

  if (cur_from == cur_to) {

    1

  } else {

  # downloading series

  codes <- sapply(c(cur_from, cur_to),
                  function(x) paste0("WB/WDI/A-", "PA.NUS.FCRF", "-", x))

  exc_rates <- rdbnomics::rdb(codes)

  exc_rates <- exc_rates[exc_rates$original_period == as.character(ref_yr), ]

  # converting factor from study country currency to USD and then to the desired output currency

  exc_rate_fct <- exc_rates[exc_rates$country == cur_to, ]$value /
    exc_rates[exc_rates$country == cur_from, ]$value

  }


}
