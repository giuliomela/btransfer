#' World Bank data to be used in unit value transfer - growth rates
#'
#' This dataset contains annual growth rates for the indicators used in unit
#' value transfer. Such data are used to "forecast" future indicator levels to
#' perform benefit transfer to years in the future. Data computed using
#' \code{compute_growth_rt}  using data contained in dataset
#' \code{\link{wb_series}}
#'
#' @format A tibble
#' \describe{
#' \item{iso3c}{Country code (Iso 3 digits)}
#' \item{year}{year}
#' \item{gdp_growth}{Annual real GDP growth}
#' \item{gdp_capita_growth}{Annual real GDP per capita growth}
#' \item{gni_growth}{Annual real GNI growth}
#' \item{gni_capita_growth}{Annual real GNI per capita growth}
#' \item{pop_growth}{Annual population growth rate}
#' }
#' @seealso \code{\link{wb_series}}
"wb_growth"
