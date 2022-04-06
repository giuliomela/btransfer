#' GDP deflators for European countries provided by Eurostat
#'
#' GDP implicit deflator to be used in unit value transfer. Data are provided by
#' Eurostat and refer to EU countries, as well as the EU as a whole and the Euro
#' area.
#'
#' @format A tibble
#' \describe{
#' \item{eu_code}{Country code (Eurostat codes)}
#' \item{year}{Year}
#' \item{gdp_defl}{GDP deflator (base year = 2015)}
#' }
#' @source \url{https://ec.europa.eu/eurostat/data/database}
"gdp_defl_eurostat"
