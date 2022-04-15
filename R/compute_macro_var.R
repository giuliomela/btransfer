#' Returns macro-economic variables for selected countries and years
#'
#' This function extracts and returns macro-economic variables from the
#' \code{\link{wb_series}} database, which are in turn retrieved from the
#' World Bank database. The user can specify the country and the year of interest.
#' In case the selected year is in the future, the function estimates future
#' values of the selected variable based on the average growth rates of the last
#' \code{n} years. Where \code{n} is the number of years between the selected
#' reference year and the latest year with available data. IN case such difference
#' is lower than 5 years, 5 years is used a window to compute the average.
#'
#' @param country_iso A string. The ISO code (3 digits) of the country of interest.
#' Codes \code{WLD} and \code{EUU} stand for the world and the European Union
#' respectively.
#' @param ref_year A double. Year of interest. Can be a year into the future up
#' to 2050. Years before 1961 are not accepted neither.
#' @param var A string. The name of the macro-economic variable to be extracted.
#' It can assume the following values: \code{gdp}, \code{gdp_capita}, \code{gni},
#' \code{pop} and \code{gni_capita}, \code{death_rate}.
#' @return A double with the value of the selected variable for the country and year
#' of interest.
#' @seealso \code{\link{wb_series}} for more information on variables.
#'
#' @examples
#' compute_macro_var("ITA", 2016, "gdp")
#' compute_macro_var("USA", 2030, "gdp")
compute_macro_var <- function (country_iso, ref_year, var = "gdp_capita") {

  if (ref_year > 2050 | ref_year < 1961) stop("Please provide a valid year. Year
                                              must be between 1961 and 2050")

  if (!is.element(var, c("gdp", "gni", "gdp_capita", "death_rate",
  "pop", "gni_capita"))) stop("Please provide a valid variable name")

  last_yr <- max(btransfer::wb_series$year)

if (ref_year <= last_yr) {

  subset(btransfer::wb_series, iso3c == country_iso & year == ref_year)[[var]]

} else {

  if (var == "death_rate") stop ("It is not possible to estimate future death rates")

  hor <- ifelse(ref_year - last_yr < 5, 5, ref_year - last_yr)

  latest_value <- subset(btransfer::wb_series, iso3c == country_iso & year == last_yr)[[var]]

  var_growth <- paste0(var, "_growth")

  avg_growth_rate <- mean(subset(btransfer::wb_growth,
                                 iso3c == country_iso & year > (last_yr - hor) &
                                   year <= last_yr)[[var_growth]], na.rm = TRUE) / 100

  fcast_value <- latest_value * (1 + avg_growth_rate)^hor

  fcast_value

}

}
