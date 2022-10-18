#' Computing eta for social discount rate estimation
#'
#' This function computes \code{eta}, the elasticity of marginal utility of consumption,
#' following the equal-sacrifice income tax approach to be used to estimate a
#' country's social discount rate using the Ramsey's rule.
#' Eta is estimated from marginal and average tax rates from the OECD tax database.
#'
#' @param iso_code A string. An ISO code (3 digits) representing the country for which
#' eta is to be calculated. \code{OAVG} returns OECD's average values.
#' @param policy_yr A double. The last year (4 digits) used to compute the average mean
#' and marginal tax rates, over an h-long period of time.
#' @param h A double. Number of the years oever which computing average values.
#' @param eta_lit A double. A custom value for \code{eta}, in case a literature value needs
#' to be used, tipically for non-OECD countries.
#' @return A double. The estimate of the elasticity of marginal utility of consumption
#' for the selected country, over an h-long period starting backwards from \code{policy_yr}
#' @export
#'
#' @examples
#' compute_eta("ITA", 2021, 20)
compute_eta <- function(iso_code, policy_yr, h, eta_lit = 1.35) {

  iso3c <- year <- Time <- ObsValue <- INDICATOR <- COU <- NULL

  # tax data is pre-loaded

  oecd_countries <- c("AUS", "AUT","BEL", "CAN", "CZE", "DNK", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL",
                      "IRL", "ITA", "JPN", "KOR", "LUX",  "MEX", "NLD", "NZL", "NOR", "POL", "PRT",
                      "SVK", "ESP", "SWE", "CHE", "TUR", "GBR", "USA", "CHL", "EST", "ISR",
                      "SVN", "OAVG", "LVA", "E22", "LTU", "COL", "CRI" )

  if (is.element(iso_code, oecd_countries)) {

  from_yr <- policy_yr - h - 1 # year from which computing averages

  # Downloading data from the OECD database

  oecd_filter <- list(c("2_5", "3_1"),
                      "SINGLE2") # used data for single people earning 110% of average salary

  tax_data <- OECD::get_dataset("AWCOMP", filter = oecd_filter)

  tax_data <- within(tax_data,{
    year <- as.numeric(Time)
    value <- as.numeric(ObsValue)
    indicator <- INDICATOR
    iso3c <- COU
  })

  tax_data <- subset(tax_data, select = c(iso3c, year, indicator, value))

  tax_data <- tax_data %>%
    dplyr::filter(iso3c == iso_code & year > from_yr & year <= policy_yr) %>%
    dplyr::group_by(iso3c, indicator) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE) / 100) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = indicator, values_from = value)

  names(tax_data) <- c("iso3c", "avg_tax", "mar_tax")

  tax_data$eta <- log(1 - tax_data$mar_tax) / log(1 - tax_data$avg_tax)

  tax_data$eta

  } else {

    eta_lit

  }



}


