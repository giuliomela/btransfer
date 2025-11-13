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
compute_gdp_dfl <- function (country_iso = "EMU", base_yr, ref_yr, growth_rate_int = 20) {

  # Variables for R CMD check
  original_period <- value <- NULL

  wb_code <- paste0("WB/WDI/A-NY.GDP.DEFL.ZS-", country_iso)
  eurostat_code <- "Eurostat/NAMA_10_GDP/A.PD05_EUR.B1GQ.EA"
  code <- ifelse(country_iso == "EMU", eurostat_code, wb_code)

  # 1. DOWNLOAD DATA (with tryCatch and suppression)
  defl_db <- NULL

  tryCatch(
    {
      invisible(capture.output(
        suppressWarnings(
          data <- rdbnomics::rdb(ids = code)
        )
      ))
      defl_db <- data
    },
    error = function(e) {
      warning(paste("Error downloading deflator data for:", country_iso), call. = FALSE) # ⚠️ Warning in English
    }
  )

  # 2. IMMEDIATE CHECK for successful download
  if (is.null(defl_db) || nrow(defl_db) == 0) {
    stop(paste("Deflator data is unavailable for", country_iso, "on DBnomics.")) # ⚠️ Stop in English
  }

  # 3. Data preparation
  defl_db <- defl_db[, c("original_period", "value")]
  defl_db$original_period <- as.numeric(defl_db$original_period)

  last_yr <- max(defl_db$original_period, na.rm = TRUE)

  # -----------------------------------------------------------
  # EXTRAPOLATION LOGIC
  # -----------------------------------------------------------

  years_to_predict <- unique(c(base_yr, ref_yr))
  years_to_predict <- years_to_predict[years_to_predict > last_yr]

  if (length(years_to_predict) > 0) {

    hist_period <- seq(last_yr - growth_rate_int, last_yr, by = 1)
    hist_data <- defl_db[defl_db$original_period %in% hist_period, ]

    if (nrow(hist_data) < 2) {
      stop(paste0("Insufficient historical data to compute inflation rate for ", country_iso)) # ⚠️ Stop in English
    }

    growth_rate <- compute_growth_rate(hist_data$value, avg = TRUE)

    forecast_data <- data.frame(
      original_period = years_to_predict,
      value = NA_real_
    )

    last_deflator_value <- defl_db[defl_db$original_period == last_yr, ]$value

    for (yr in years_to_predict) {
      fcast_h <- yr - last_yr
      forecast_value <- last_deflator_value * (1 + growth_rate)^fcast_h

      forecast_data[forecast_data$original_period == yr, "value"] <- forecast_value
    }

    defl_db <- dplyr::bind_rows(defl_db, forecast_data)
  }

  # -----------------------------------------------------------
  # FINAL FACTOR CALCULATION
  # -----------------------------------------------------------

  defl_ref <- defl_db[defl_db$original_period == ref_yr, ]$value
  defl_base <- defl_db[defl_db$original_period == base_yr, ]$value

  if (is.na(defl_ref) || is.na(defl_base)) {
    stop(paste0("Could not find deflator for required base year (", base_yr, ") or reference year (", ref_yr, ").")) # ⚠️ Stop in English
  }

  fct <- defl_ref / defl_base

  return(fct)
}
