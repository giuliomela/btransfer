#' Average and marginal income tax rates of OECD countries
#'
#' This dataset contains data on average and marginal income tax rates of OECD
#' countries retrieved from the OECD tax database.Data refer to a single person that
#' earns the 100% of the average income at national level.
#'
#' \describe{
#' \item{iso3c}{Country codes (ISO3C), string.}
#' \item{year}{Year, numeric.}
#' \item{indicator}{2_5 stands for average tax rate, 3_1 for marginal tax rate.}
#' \item{value}{indicator value (in percentage).}
#' }
#' @source \url{https://www.oecd.org/ctp/tax-policy/tax-database/}
"tax_data"
