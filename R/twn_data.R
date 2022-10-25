#' Macroeconomic variables for Taiwan
#'
#' Since the World Bank does not publish data regarding Taiwan, it is
#' difficult to automatically retrieve the data from other sources, especially
#' when Taiwan is part of an aggregate. Therefore data for Taiwan are downloaded
#' manually once a year (or when needed) and made available as external data.
#' The script to download Taiwan data is in the `DATASET` file.
#'
#' @format A tibble
#' \describe{
#' \item{original_period}{reference period (year)}
#' \item{value}{value}
#' \item{series_code}{variable of interest}
#' \item{countries}{Taiwan ISO code}
#' }
"twn_data"
