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
#' @return A dataframe of 2 variables. The ISO3 code of the country and the
#' estimate of the elasticity of marginal utility of consumption
#' for the selected country, over an h-long period starting backwards from \code{policy_yr}
#' @export
#'
#' @examples
#' compute_eta("ITA", 2019, 10)
compute_eta <- function(iso_code, policy_yr, h, eta_lit) {

  # tax data is pre-loaded

  if (is.element(iso_code, unique(tax_data$iso3c))) {

  from_yr <- policy_yr - h - 1 # year from which computing averages

  data <- subset(tax_data, iso3c == iso_code & year > from_yr & year <= policy_yr)

  avg_tax_rt <- mean(data[data$indicator == "2_5", ]$value, na.rm = TRUE) / 100

  mar_tax_rt <- mean(data[data$indicator == "3_1", ]$value, na.rm = TRUE) / 100

  eta <- log(1 - mar_tax_rt) / log(1 - avg_tax_rt)

  } else {

    eta <- eta_lit

  }

  data.frame(iso3c = iso_code, eta = eta)

}


