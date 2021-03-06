#' World bank data to be used in unit value transfers
#'
#' This dataset contains the latest available WB data on GDP and GDP per capita
#' (constant international dollars), GNI (current prices), population, GDP deflators and
#' exchange rates (USD per unit of LCU). Such data are used to perform unit value
#' transfer with income adjustment.
#'
#' @format A tibble
#' \describe{
#' \item{iso3c}{Country code (Iso 3 digits)}
#' \item{country}{Country name in English}
#' \item{year}{year}
#' \item{gdp_defl}{GDP deflator (WB code: NY.GDP.DEFL.ZS)}
#' \item{gdp}{GDP at constant prices PPP (WB code: NY.GDP.MKTP.PP.KD)}
#' \item{gdp_capita}{GDP per capita at constant prices PPP (WB code: NY.GDP.PCAP.PP.KD)}
#' \item{gni}{GNI at constant prices (converted from current with US GDP deflator, base year = yea rused by the WB for income classification), Atlas method (Wb code: NY.GNP.ATLS.CD)}
#' \item{exc_rate}{Exchange rate, USD per LCU (WB code: PA.NUS.FCRF)}
#' \item{pop}{Total population (WB code: SP.POP.TOTL)}
#' \item{gni_capita}{Gross national income per capita obtained dividing gni by pop}
#' \item{deaths}{Total number of deaths - calculated starting from death rate and population data, thousand deaths}
#' }
#' @source \url{https://data.worldbank.org/}
"wb_series"
