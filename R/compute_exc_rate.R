#' Compute exchange rates between two currencies in given year
#'
#' This function computes nominal exchange rates between two custom currencies for
#' a given year. Exchange rate data are from the World Bank database. Euro is
#' denoted with the code \code{EMU}.
#'
#' @param cur_from A string. Country code (ISO, 3 digits) to which the origin
#' currency belongs.
#' @param cur_to A string. Country code (ISO, 3 digits) to which the destination
#' currency belongs.
#' @param ref_year A double. Year for which the exchange rate must be calculated
#' @return A double. Echange rate for the currency pair and the year selected.
#' @source \url{https://data.worldbank.org/}
compute_exc_rate <- function (cur_from, cur_to, ref_year) {

  iso3c <- year <- NULL

  exc_rates <- subset(btransfer::wb_series, iso3c %in% c(cur_from, cur_to) &
                        year == ref_year,
                      select = c("iso3c", "exc_rate"))

  # converting factor from study country currency to USD and then to the desired output currency

  exc_rate_fct <- exc_rates[exc_rates$iso3c == cur_to, ]$exc_rate /
    exc_rates[exc_rates$iso3c == cur_from, ]$exc_rate

  exc_rate_fct

}
