#' Computes the GDP deflator for a given country/currency and year
#'
#' This function computes the GDP deflator for a country provided by the user.
#' Data are from the World Bank database and cover most countries in the world.
#' Data on the Euro zone are from Eurostat.
#'
#' @param country_iso A character string. The ISO code (3 digits) of the country of interest.
#' Default is the Euro Area (\code{EMU}).
#' @param year A double. The year for which the GDP deflator must be calculated.
#' calculated.
#' @returns A numeric value: the GDP deflator for the country/year combination
#' selected.
#' @source \url{https://ec.europa.eu/eurostat/data/database} and \url{https://data.worldbank.org/}
#'
#' @examples
#' compute_gdp_dfl("ITA", 2019)
#' compute_gdp_dfl(year = 2014)
compute_gdp_dfl <- function (country_iso = "EMU", ref_year) {

  if (country_iso == "EMU"){

    gdp_defl <- subset(btransfer::gdp_defl_eurostat, eu_code == "EA" & year == ref_year)$gdp_defl

  } else {

    gdp_defl <- subset(btransfer::wb_series, iso3c == country_iso
                       & year == ref_year)$gdp_defl

  }

    gdp_defl

}
