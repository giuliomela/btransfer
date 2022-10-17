#' Identifies ISO codes of a given country or aggregate
#'
#' This function exploits the \code{countrycode} package to identify the ISO
#' codes (3 digits) of a given country.
#'
#' @param country_name A single string. The name of the country for which the
#'     ISO code must be identified. Country names can be in any language. The function
#'     returns ISO codes for the European Union ad the world as well, but, in these
#'     cases, only English, French, Italian, Spanish, Portuguese, German and Dutch
#'     are supported. The function can also return the ISO codes of the countries that
#'     make up the maritime aggregates provided by the Handbook on the external cost
#'     of transport:
#'     - Mediterranean sea (`med`)
#'     - Black sea (`blk`)
#'     - North sea (`nor`)
#'     - Atlantic ocean (`atl`), European countries only
#'     - Baltic sea (`bal`)
#' @return A string, the ISO code of the country of interest.
#' @seealso \code{\link[countrycode]{countryname}}
#' @examples
#' iso_codes("Italia")
iso_codes <- function (country_name) {

  world_names <- c("Mondo", "World", "Monde", "Mundo", "Welt",
                   "Wereld")

  eu_names <- c("Unione Europea", "European Union", "EU", "UE", paste0("Union Europ",
  "\U00E9", "enne"), paste0("Uni", "\U00F3", "n Europea"),
  paste0("Uni", "\U00C3", "o Europeia"), paste0("Europ",
  "\U00E4", "ische Union"), "Europese Unie")

  agg_names <- c("med", "bal", "blk", "atl", "nor")

  if (country_name %in% world_names) {

    "WLD"

  } else if (country_name %in% eu_names) {

    "EUU"

  } else if (country_name %in% agg_names) # maritime aggregates provided by the HB

    agg_composition[agg_composition$code == country_name, ]$iso3c

  else {

    countrycode::countryname(country_name, "iso3c")

  }

}
