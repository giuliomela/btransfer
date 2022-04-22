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
#' @param country A string vector of country names (in any language, see \code{\link[countrycode]{countryname}} package for details).
#' In case \code{aggregate = TRUE}, the list of countries is used to compute the SDR for that group of countries.
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
#' the aggregate name to be displayed in the output tibble. Default is set to "none".
#' @return A tibble containing country names, Ramsey's equation parameters and
#'  the social discount rate calculated for the selected countries or aggregates.
#' @export
#'
#' @examples
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska"))
#'  compute_sdr("Italy", 2018, 15)
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska", aggregate = "yes"))
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska", aggregate = "row"))
compute_sdr <- function(country, policy_yr = 2019, h = 10, aggregate = "no", eta_lit = 1.35,
                        aggregate_name = "none"){

  iso3c <- year <- death_rate <- gdp_capita_growth <- gdp <- pop <- NULL

# Identifying the iso3c codes of the countries of interest. Names can be provided in any language

  country_iso <- iso_codes(country)

# identifying gdp per capita growth rates

  if (aggregate == "no") {

    sdr_data <- dplyr::tibble(iso3c = country_iso)

    sdr_data$gdp_capita_growth <- sapply(sdr_data$iso3c, function (x) {

      compute_avg(x, start_yr = policy_yr - h, end_yr = policy_yr, var = "gdp_capita", type = "growth_rt")

    })

    # adding death rates

    sdr_data$death_rt <- sapply(sdr_data$iso3c, function (x) {

      compute_avg(x, start_yr = policy_yr - h, end_yr = policy_yr, var = "death_rate")

    })

  # computing etas

  sdr_data$eta <- sapply(sdr_data$iso3c, function(x){

    compute_eta(x, policy_yr, h, eta_lit)

  })

  # adding country names

  sdr_data$country <- from_iso_to_name(sdr_data$iso3c)

  # computing SDR

  sdr_data$sdr <- sdr_data$death_rt / 1000 + sdr_data$eta * sdr_data$gdp_capita_growth

  sdr <- sdr_data[, c("iso3c", "country", "gdp_capita_growth", "death_rt", "eta", "sdr")]

  } else if (aggregate == "yes") { # aggregates analysis

    disagg_values <- tidyr::expand_grid(iso3c = country_iso,
                       year = seq(policy_yr - h, policy_yr, by = 1))

    vars <- c("gdp", "pop", "death_rate")

 # computing aggregate gdp and pop
    for (i in vars) {

      disagg_values[[i]] <-purrr::map2_dbl(disagg_values$iso3c, disagg_values$year,
                                       function(x, y) compute_macro_var(x, y, i))

    }


    sdr_data <- dplyr::tibble(year = seq(policy_yr - h, policy_yr, by = 1))

    # computing aggregate gdp and pop data

    for (i in c("gdp", "pop")) {

      sdr_data[[i]] <- tapply(disagg_values[[i]],
                              disagg_values[["year"]], sum)

    }

    # computing gdp per capita and gdp growth rates

    sdr_data$gdp_capita <- sdr_data$gdp / sdr_data$pop

    # computing growth rates

    sdr_data$gdp_growth <- compute_growth_rate(sdr_data$gdp_capita)

    # computing aggregate death rates

    agg_death_rates <- by(disagg_values, disagg_values$year, function(DF){
      aggregate(death_rate ~ year, DF, function(y, w)
        weighted.mean(y, w = DF[['pop']], na.rm = TRUE))
    })


    agg_death_rates <- do.call("rbind", agg_death_rates)

    # adding death rates to the sdr_data tibble

    sdr_data <- merge(sdr_data, agg_death_rates)

    # computing eta

    eta_db <- dplyr::tibble(iso3c = country_iso)

    eta_db$eta <- sapply(eta_db$iso3c, function (x) compute_eta(x, policy_yr, h, eta_lit))

    eta_db$pop <- sapply(eta_db$iso3c, function (x) compute_avg(x, start_yr = policy_yr - h,
                                                               end_yr = policy_yr, var = "pop"))

    sdr_data <- sdr_data[sdr_data$year != policy_yr - h, ]

    sdr_data <- dplyr::tibble(iso3c = NA, country = aggregate_name,
                              gdp_capita_growth = mean(sdr_data$gdp_growth, na.rm = T),
                              death_rate = mean(sdr_data$death_rate, na.rm = T),
                              eta = weighted.mean(eta_db$eta, eta_db$pop))

    sdr_data$sdr <- sdr_data$death_rate / 1000 + sdr_data$eta * sdr_data$gdp_capita_growth


    sdr_data


    # weighting eta according to population

    avg_pop <- subset(btransfer::wb_series, iso3c %in% country_iso & year > policy_yr - h &
                        year <= policy_yr, select = c(iso3c, year, pop))

    avg_pop_l <- split(avg_pop, avg_pop$iso3c)

    avg_pop_l <- lapply(avg_pop_l, function (x) {

      data.frame(iso3c = unique(x$iso3c),
                 pop = mean(x$pop, na.rm = TRUE))

    })

    avg_pop <- do.call("rbind", avg_pop_l)

    eta_countries <- merge(eta_countries, avg_pop)

    eta <- stats::weighted.mean(eta_countries$eta, eta_countries$pop)

  } else if (aggregate == "row"){ # computes the sdr of the rest of the world (with respect to selected countries)

    disagg_values <- tidyr::expand_grid(iso3c = c(country_iso, "WLD"),
                                        year = seq(policy_yr - h, policy_yr, by = 1))

    vars <- c("gdp", "pop", "death_rate")


    # computing aggregate gdp and pop
    for (i in vars) {

      disagg_values[[i]] <-purrr::map2_dbl(disagg_values$iso3c, disagg_values$year,
                                           function(x, y) compute_macro_var(x, y, i))

    }


    sdr_data <- dplyr::tibble(year = seq(policy_yr - h, policy_yr, by = 1))

    # computing aggregate gdp and pop data

    for (i in c("gdp", "pop")) {

      sdr_data[[i]] <- tapply(disagg_values[[i]],
                              disagg_values[["year"]], sum)

    }

    # computing gdp per capita and gdp growth rates

    sdr_data$gdp_capita <- sdr_data$gdp / sdr_data$pop

    # computing growth rates

    sdr_data$gdp_growth <- compute_growth_rate(sdr_data$gdp_capita)



  }

  # computing SDR

  if (aggregate == "no") {

    sdr

  } else {

  sdr <- death_rt / 1000 + eta * gdp_growth

  country_name <- ifelse(aggregate == "yes", aggregate_name, "ROW")

  tidyr::tibble(iso3c = NA_character_,
                country = country_name, gdp_capita_growth = gdp_growth,
                death_rt = death_rt,
                eta = eta_lit, sdr = sdr)

  }

}

