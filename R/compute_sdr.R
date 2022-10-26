#' Computing the social discount rate with Ramsey's rule.
#'
#' This function computes the social discount rate using the Ramsey's rule for all countries in the world.
#' Data used for the computation are from the World Bank and the OECD.
#' One of the key parameters of the Ramsey's equation is the elasticity of marginal utility of consumption (eta)
#' that can be  estimated, following the equal-sacrifice income tax approach, from marginal and average tax rates.
#' Since tax data are available for OECD countries only, for non-OECD countries eta is assumed to be equal to 1.35,
#' which is the mean value that emerged from an expert survey (about 180 interviews)
#' carried out by Drupp et al. For more information on the Ramsey's rule please refer to
#' \href{https://www.oecd-ilibrary.org/sites/9789264085169-11-en/index.html?itemId=/content/component/9789264085169-11-en}{the OECD's guide on cost benefit analysis and the environment}
#'
#'
#' @param country A string vector of country names (in any language, see \code{\link[countrycode]{countryname}} package for details).
#'     In case `agg = "yes"` or `agg = "row"`, the list of countries is
#'     used to compute the SDR for that group of countries or for the rest of the world respectively.
#'     The user can select also the world or the European Union by providing the name in either English,
#'     French, Italian, Spanish, Portuguese, German or Dutch.
#' @param policy_yr A double: the policy year, meant as the year to which the analysis refers. Default is 2019.
#' @param h A double: number of years over which calculate Ramsey's rule parameters. Default is 10.
#' @param agg A string that can assume three values. If "no", individual SDR for the selected countries  are
#'     provided. If "yes", the SDR for the aggregate made up by the selected countries is provided. If "row", the
#'     SDR for the ROW (with respect to the selected countries) is provided. Default is "no".
#' @param eta_lit A double. Value of the elasticity of marginal utility of consumption for non-OECD countries,
#'     that is those for which no tax data are available. This parameter is used also for the ROW estimate.
#' @return A tibble containing country names, Ramsey's equation parameters and
#'     the social discount rate calculated for the selected countries or aggregates.
#' @export
#'
#' @examples
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska"))
#'  compute_sdr("Italy", 2018, 15)
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska"), agg = "yes")
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska"), agg = "row")
compute_sdr <- function(country, policy_yr = 2021, h = 20, agg = "no", eta_lit = 1.35){

  iso3c <- year <- death_rate <- gdp_capita_growth <- gdp <- pop <- original_period <- variable <- value <- NULL

# Identifying the iso3c codes of the countries of interest. Names can be provided in any language

  country_iso <- sapply(country, iso_codes)

  avg_period <- seq(policy_yr - h + 1, policy_yr, by = 1)

# Downloading gdp per capita growth rates

  if (agg == "no") {

    # DBnomics codes

    wb_codes <- tidyr::tibble(indicator = c("SP.DYN.CDRT.IN", "NY.GDP.PCAP.KD.ZG"),
                              variable = c("death_rate", "gdp_capita"))

    codes <- NULL

    for (i in wb_codes$indicator){

      for (j in c(country_iso)){

        codes[[paste0(i, "-", j)]] <- paste0("WB/WDI/A-", i, "-", j)

      }

    }

    codes <- unlist(codes)

    data_raw <- rdbnomics::rdb(codes)

    data_raw$original_period <- as.numeric(data_raw$original_period)

    sdr_data <- data_raw %>%
      dplyr::left_join(wb_codes) %>%
      dplyr::filter(original_period %in% avg_period) %>%
      dplyr::group_by(country, variable) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = variable, values_from = value)

  # computing etas

    sdr_data <- sdr_data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(eta = btransfer::compute_eta(country, policy_yr, h, eta_lit),
                    country = from_iso_to_name(country)) %>%
      dplyr::ungroup()

  # adding country names

  #sdr_data$country <- sapply(sdr_data$iso3c, from_iso_to_name)

  # computing SDR

  sdr_data$sdr <- sdr_data$death_rate / 1000 + sdr_data$eta * sdr_data$gdp_capita / 100

  sdr_data$sdr

  } else { # aggregates analysis

    vars <- c("gdp_capita", "death_rate")

    # computing average GDP per capita growth in the aggregate

    gdp_capita_growth <- compute_macro_var(
      country_iso = country_iso,
      ref_yr = policy_yr,
      var = "gdp_capita",
      agg = agg,
      growth_rate_int = h,
      growth_rt = TRUE
    )[["growth_rate"]]

   # computing the average death rate in the aggregate

    avg_death_rate <- compute_macro_var(
      country_iso = country_iso,
      ref_yr = policy_yr,
      var = "death_rate",
      agg = agg,
      growth_rate_int = h,
      avg = TRUE
    )[["avg_value"]] # already expressed in deaths/population

    eta <- eta_lit

 sdr <- avg_death_rate + eta * gdp_capita_growth

 sdr

  }

}

