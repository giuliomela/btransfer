#' Compute average values of macro-economic variables
#'
#' This function computes average values of some macro-economic variables
#' for selected countries and time intervals. Underlying data can be either
#' levels (\code{\link{wb_series}}) or growth rates (\code{\link{wb_growth}}).
#'
#' @param iso A string. A country's ISO code (3 digits). The function accepts
#' \code{WLD} and \code{EUU} for the world and the European Union respectively.
#' @param start_yr A double. Beginning of the time window over which the average
#' is computed.
#' @param end_yr A double. End of the time window over which the average
#' is computed.
#' @param var A string. The short name of the macro-economic variable of interest.
#' Possible values are \code{gdp}, \code{gdp_capita}, \code{gni},
#' \code{pop} and \code{death_rate}, \code{gni_capita}.
#' @param type A string. Indicates the type of data over which compute the average
#' values. It can assume two values: \code{levels} or \code{growth_rt}.
#' @return A double. The average growth rate of the selected variable for the
#' country and time window of interest.

compute_avg <- function (iso, start_yr, end_yr, var, type = "levels") {

  year <- iso3c <- NULL

  if (!is.element(type, c("levels", "growth_rt"))) stop ("Please provide a valid type variable")

  if (start_yr >= end_yr) stop ("'end_yr' cannot be earlier or the same as 'start_yr'")

  if (type == "levels") {

    data <- btransfer::wb_series

  } else {

    data <- btransfer::wb_growth

  }

  if (start_yr < min(data$year) |
  end_yr > max(data$year)) stop(paste0("Please provide a valid year. Year
                                              must be between 1960 and ",
                                                       max(data$year)))

  if (!is.element(var, c("gdp", "gni", "gdp_capita", "pop", "gni_capita", "death_rate"))) stop("Please provide
                                                                   a valid variable name")

  variable <- ifelse(type == "levels", var, paste0(var, "_growth"))

  series <- subset(data, year <= end_yr &
                   year > start_yr & iso3c == iso)[[variable]]

  mean(series, na.rm = TRUE)

}
