#' Returns country names (in English) from ISO
#'
#' This function returns the English name of a country starting from its ISO code.
#'
#' @param iso A string. The a country's Iso code (3 digits). Special values
#' admitted are \code{WLD} (returns \code{World}) and \code{EUU} (returns
#' \code{European Union}). Function is based on \code{\link[countrycode]{countrycode}}.
#' @return A string. The English name of the country of interest.
#' @seealso \code{\link[countrycode]{countrycode}}
#'
#' @examples
#' from_iso_to_name("ITA")
#' from_iso_to_name("WLD")
from_iso_to_name <- function (iso) {

  if (iso == "WLD") {

    "World"

  } else if (iso == "EUU") {

    "European Union"

  } else {

    countrycode::countrycode(iso, origin = "iso3c", destination = "country.name.en")

  }

}
