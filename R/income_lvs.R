#' World Bank country classification according to income levels
#'
#' The dataset contains the latest available country classification according to
#' income levels. The classification is based on GNI per capita and is updated
#' annually.
#'
#' @format a tibble
#' \describe{
#' \item{iso3c}{Three-digit country code}
#' \item{income_level_iso3c}{Income classification code}
#' \item{income_level}{Income classification - full name}
#' }
#' @source \url{https://data.worldbank.org/}
"income_lvs"
