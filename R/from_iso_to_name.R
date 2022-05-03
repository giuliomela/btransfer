#' Returns country names (in English) from ISO
#'
#' This function returns the English name of a country starting from its ISO code.
#'
#' @param iso A string. The a country's Iso code (3 digits). Special values
#' admitted are \code{WLD} (returns \code{World}) and \code{EUU} (returns
#' \code{European Union}). Function is based on \code{\link[countrycode]{countrycode}}.
#' @return A string. The English name of the country of interest.
#' @seealso \code{\link[countrycode]{countrycode}}
from_iso_to_name <- function (iso) {

  iso3c <- NULL

  all_isos <- subset(countrycode::codelist, !is.na(iso3c))$iso3c

  # adding wolrd and EU isos

  all_isos <- c("WLD", "EUU", all_isos)

  if (!is.element(iso, all_isos))stop("Please provide a valid ISO")

  if (iso == "WLD") {

    "World"

  } else if (iso == "EUU") {

   "European Union"

  } else {

    countrycode::countrycode(iso, origin = "iso3c", destination = "country.name.en")

  }

}
