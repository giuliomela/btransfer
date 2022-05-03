#' Computing the social discount rate with Ramsey's rule.
#'
#' This function computes the social discount rate using the Ramsey's rule for all countries in the world.
#' Data used for the computation are from the World Bank and the OECD.
#' One of the key parameters of the Ramsey's equation is the elasticity of marginal utility of consumption (eta)
#' that can be  estimated, following the equal-sacrifice income tax approach, from marginal and average tax rates.
#' Since tax data are avilable for OECD countries only, for non-OECD countries eta is assumed to be equal to 1.35,
#' which is the mean value that emerged from an expert survey (about 180 interviews)
#' carried out by Drupp et al. For more information on the Ramsey's rule please refer to
#' \href{https://www.oecd-ilibrary.org/sites/9789264085169-11-en/index.html?itemId=/content/component/9789264085169-11-en}{the OECD's guide on cost benefit analysis and the environment}
#'
#'
#' @param countries A string vector of country names (in any language, see \code{\link[countrycode]{countryname}} package for details).
#' In case \code{aggregate = "yes"} or \code{aggregate = "row"}, the list of countries is
#' used to compute the SDR for that group of countries or for the rest of the world respectively.
#' The user can select also the world or the European Union by providing the name in either English,
#' French, Italian, Spanish, Portuguese, German or Dutch.
#' @param policy_yr A double: the policy year, meant as the year to which the analysis refers. Default is 2019.
#' @param h A double: number of years over which calculate Ramsey's rule parameters. Default is 10.
#' @param aggregate A string that can assume three values. If "no", individual SDR for the selected countries  are
#' provided. If "yes", the SDR for the aggregate made up by the selected countries is provided. If "row", the
#' SDR for the ROW (with respect to the selected countries) is provided. Default is "no".
#' @param eta_lit A double. Value of the elasticity of marginal utility of consumption for non-OECD countries,
#' that is those for which no tax data are available. This parameter is used also for the ROW estimate.
#' @param aggregate_name A string. In case the SDR must be calculated for an aggregate, the user can provide
#' the aggregate name to be displayed in the output tibble. Default is set to "aggregate".
#' @return A tibble containing country names, Ramsey's equation parameters and
#'  the social discount rate calculated for the selected countries or aggregates.
#' @export
#'
#' @examples
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska"))
#'  compute_sdr("Italy", 2018, 15)
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska"), aggregate = "yes")
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska"), aggregate = "row")
compute_sdr <- function(countries, policy_yr = 2019, h = 10, aggregate = "no", eta_lit = 1.35,
                        aggregate_name = "aggregate"){

  iso3c <- year <- death_rate <- gdp_capita_growth <- gdp <- pop <- NULL

# Identifying the iso3c codes of the countries of interest. Names can be provided in any language

  country_iso <- sapply(countries, iso_codes)

# identifying gdp per capita growth rates

  if (aggregate == "no") {

    sdr_data <- dplyr::tibble(iso3c = country_iso)

    sdr_data$gdp_capita_growth <- sapply(sdr_data$iso3c, function (x) {

      compute_avg(x, start_yr = policy_yr - h, end_yr = policy_yr, var = "gdp_capita", type = "growth_rt")

    })

    # adding death rates

    sdr_data$death_rate <- sapply(sdr_data$iso3c, function (x) {

      compute_avg(x, start_yr = policy_yr - h, end_yr = policy_yr, var = "death_rate")

    })

  # computing etas

  sdr_data$eta <- sapply(sdr_data$iso3c, function(x){

    compute_eta(x, policy_yr, h, eta_lit)

  })

  # adding country names

  sdr_data$country <- sapply(sdr_data$iso3c, from_iso_to_name)

  # computing SDR

  sdr_data$sdr <- sdr_data$death_rate / 1000 + sdr_data$eta * sdr_data$gdp_capita_growth

  sdr <- sdr_data[, c("iso3c", "country", "gdp_capita_growth", "death_rate", "eta", "sdr")]

  sdr

  } else { # aggregates analysis

    sdr_data <- dplyr::tibble(year = seq(policy_yr - h, policy_yr, by = 1))

    vars <- c("gdp_capita", "death_rate")

 # computing aggregate gdp capita and death rates

    for (i in vars) {

      sdr_data[[i]] <- sapply(sdr_data$year,
                                   function(x) compute_macro_var(country_iso, x, i, aggregate))

    }

    # computing gdp growth rate

    sdr_data$gdp_capita_growth <- compute_growth_rate(sdr_data$gdp_capita)

    sdr_data <- sdr_data[sdr_data$year != policy_yr - h, ]

    # computing eta

    if (aggregate == "yes") { # computes weighted average of individual countries etas

    eta_db <- dplyr::tibble(iso3c = country_iso)

    eta_db$eta <- sapply(eta_db$iso3c, function (x) compute_eta(x, policy_yr, h, eta_lit))

    eta_db$pop <- sapply(eta_db$iso3c, function (x) compute_avg(x, start_yr = policy_yr - h,
                                                               end_yr = policy_yr, var = "pop"))

    eta <-  stats::weighted.mean(eta_db$eta, eta_db$pop)

    } else {

      eta <- eta_lit # in case the aggregate is the ROW, the eta from the literature is used

    }

    # computing average values for the aggregate

    sdr_data <- dplyr::tibble(iso3c = NA_character_, country = aggregate_name,
                              gdp_capita_growth = mean(sdr_data$gdp_capita_growth, na.rm = T),
                              death_rate = mean(sdr_data$death_rate, na.rm = T),
                              eta = eta)

    sdr_data$sdr <- sdr_data$death_rate / 1000 + sdr_data$eta * sdr_data$gdp_capita_growth


    sdr_data

  }

}

