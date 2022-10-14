#' Computes the GDP deflator for a given country/currency and year
#'
#' This function that returns a factor to convert monetary value from a base to a
#' reference year usding the GDP deflator of a given country/currency .
#' Data are from the World Bank and Eurostat databases and cover most countries in the world.
#'
#' @param country_iso A character string. The ISO code (3 digits) of the country of interest.
#' Default is the Euro Area `EMU`.
#' @param base_yr A numeric value. The year from which price levels the monetary value must be converted.
#' @param ref_yr A numeric value. The year to which price levels the monetary value must be converted.
#' @returns A numeric value: the factor which the monetary value to be converted must be multiplied by.
#' @source \url{https://db.nomics.world/}
compute_gdp_dfl <- function (country_iso = "EMU", base_yr, ref_yr) {

  wb_code <- paste0("WB/WDI/A-NY.GDP.DEFL.ZS-", country_iso)

  eurostat_code <- "Eurostat/nama_10_gdp/A.PD05_EUR.B1GQ.EA"

  code <- ifelse(country_iso == "EMU", eurostat_code, wb_code)

  defl_db <- rdbnomics::rdb(ids = code)

  defl_db <- defl_db[, c("original_period", "value")]

  defl_db$original_period <- as.numeric(defl_db$original_period)

  range_yr <- min(defl_db$original_period):max(defl_db$original_period)

  #checking that base and reference years are between the supported range

  if(!is.element(base_yr, range_yr) |
     !is.element(ref_yr, range_yr))
    stop(paste0("Base and Reference years must be between ",
                min(range_yr),
                " and ",
                max(range_yr)))

  fct <- defl_db[defl_db$original_period == ref_yr, ]$value /
    defl_db[defl_db$original_period == base_yr, ]$value

  fct

}
