#' Identifies ISO codes of a given country or aggregate
#'
#' This function exploits the \code{countrycode} package to identify the ISO
#' codes (3 digits) of a given country.
#'
#' @param country_name A single string. The name of the country for which the
#' ISO code must be identified. Country names can be in any language. The function
#' returns ISO codes for the European Union ad the world as well, but, in these
#' cases, only English, French, Italian, Spanish, Portuguese, German and Dutch
#' are supported.
#' @return A string, the ISO code of the country of interest.
#' @seealso \code{\link[countrycode]{countryname}}
iso_codes <- function (country_name) {

  world_names <- c("Mondo", "World", "Monde", "Mundo", "Welt",
                   "Wereld")

  eu_names <- c("Unione Europea", "European Union", "EU", "UE", "Union Européenne",
                "Unión Europea", "União Europeia", "Europäische Union", "Europese Unie")

  if (country_name %in% world_names) {

    "WLD"

  } else if (country_name %in% eu_names) {

    "EUU"

  } else {

  countrycode::countryname(country_name, "iso3c")

  }

}
