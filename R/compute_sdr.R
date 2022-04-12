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
#' @param policy_yr A double: the policy year, meant as the year to which the analysis refers. Default is 2019.
#' @param h A double: number of years over which calculate Ramsey's rule parameters. Default is 10.
#' @param aggregate A string that can assume three values. If "no", individual SDR for the selected countries  are
#' provided. If "yes", the SDR for the aggregate made up by the selected countries is provided. If "row", the
#' SDR for the ROW (with respect to the selected countries) is provided. Default is "no".
#' @param eta_lit A double. Value of the elasticity of marginal utility of consumption for non-OECD countries,
#' that is those for which no tax data are available. This parameter is used also for the ROW estimate.
#' @return A tibble containing country names, Ramsey's equation parameters and
#'  the social discount rate calculated for the selected countries or aggregates.
#' @export
#'
#' @examples
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska"))
#'  compute_sdr("Italy", 2018, 15)
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska", aggregate = "yes"))
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska", aggregate = "row"))
compute_sdr <- function(country, policy_yr = 2019, h = 10, aggregate = "no", eta_lit = 1.35){

  iso3c <- year <- death_rate <- gdp_capita_growth <- gdp <- pop <- NULL

# Identifying the iso3c codes of the countryies of interest. Names can be provided in any language
  country_iso <- countrycode::countryname(sourcevar = country,
                                  destination = "iso3c")

# identifying gdp per capita growth rates

  if (aggregate == "no") {

  # Computing average growth rates

  gdp_growth <- subset(btransfer::wb_growth, iso3c %in% country_iso & year > policy_yr - h &
           year <= policy_yr)

  gdp_growth_l <- split(gdp_growth, gdp_growth$iso3c)

  gdp_growth_l <- lapply(gdp_growth_l, function (x) {

    data.frame(iso3c = unique(x$iso3c),
               gdp_capita_growth = mean(x$gdp_capita_growth, na.rm = TRUE) / 100)

  })

  gdp_growth <- do.call("rbind", gdp_growth_l)

  # computing average ten-year death rate for selected countries

  death_rt <- subset(btransfer::wb_series, iso3c %in% country_iso & year > policy_yr - h &
                       year <= policy_yr, select = c(iso3c, year, death_rate))

  death_rt_l <- split(death_rt, death_rt$iso3c)

  death_rt_l <- lapply(death_rt_l, function(x){
    data.frame(iso3c = unique(x$iso3c),
               death_rt = mean(x$death_rate, na.rm = TRUE))
  })

  death_rt <- do.call("rbind", death_rt_l)

  rownames(death_rt) <- NULL

  # computing etas

  eta_l <- lapply(country_iso, function(x) compute_eta(x, policy_yr, h, eta_lit))

  eta <- do.call("rbind", eta_l)

  sdr <- Reduce(merge, list(gdp_growth, death_rt, eta))

  sdr <- transform(sdr,
                   sdr = death_rt / 1000 + eta * gdp_capita_growth)

  tidyr::as_tibble(sdr)

  } else if (aggregate == "yes") { # aggregates analysis

    # In case of aggregates, average growth rates must be computed from gdp and population data

    gdp_pop <- subset(btransfer::wb_series, iso3c %in% country_iso & year >= policy_yr - h &
                        year <= policy_yr, select = c(iso3c, year, gdp, pop))

    gdp_pop_l <- split(gdp_pop, gdp_pop$year)

    gdp_pop_l <- lapply(gdp_pop_l, function(x){
      data.frame(year = unique(x$year),
                 gdp = sum(x$gdp, na.rm = TRUE),
                 pop = sum(x$pop, na.rm = TRUE))
    })

    gdp_pop <- do.call("rbind", gdp_pop_l)

    gdp_pop$gdp_capita <- gdp_pop$gdp / gdp_pop$pop

    gdp_pop <- transform(gdp_pop,
                            gdp_growth = c(NA, gdp_capita[-1] / gdp_capita[-nrow(gdp_pop)] - 1))

    gdp_growth <- gdp_pop[, c("year", "gdp_growth")]

    rownames(gdp_growth) <- NULL

    gdp_growth <- mean(gdp_growth$gdp_growth[-1])

    # computing aggregate death rates

    death_rt <- subset(btransfer::wb_series, iso3c %in% country_iso & year > policy_yr - h &
                         year <= policy_yr, select = c(iso3c, year, death_rate, pop))

    # computing year-by-year avg death rates of the aggregate

    death_rt_l <- split(death_rt, death_rt$year)

    death_rt_l <- lapply(death_rt_l, function(x){
      data.frame(year = unique(x$year),
                 death_rt = stats::weighted.mean(x$death_rate, x$pop))
    })

    death_rt <- do.call("rbind", death_rt_l)

    rownames(death_rt) <- NULL

    death_rt <- mean(death_rt$death_rt)

    # computing eta

    eta_l <- lapply(country_iso, function (x) compute_eta(x, policy_yr, h, eta_lit))

    eta_countries <- do.call("rbind", eta_l)

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


  # computing the SDR

    sdr <- death_rt / 1000 + eta * gdp_growth

    tidyr::tibble(death_rt = death_rt, eta = eta, gdp_growth = gdp_growth, sdr = sdr)

  } else if (aggregate == "row"){ # computes the sdr of the rest of the world (with respect to selected countries)

    gdp_pop <- subset(btransfer::wb_series, iso3c %in% c(country_iso, "WLD") & year >= policy_yr - h &
             year <= policy_yr, select = c(iso3c, year, gdp, pop))

    gdp_pop <- transform(gdp_pop, gdp = ifelse(iso3c != "WLD", - gdp, gdp),
                      pop = ifelse(iso3c != "WLD", - pop, pop))

    gdp_pop_l <- split(gdp_pop, gdp_pop$year)

    gdp_pop_l <- lapply(gdp_pop_l, function (x){

      data.frame(year = unique(x$year),
      gdp = sum(x$gdp, na.rm = TRUE),
      pop = sum(x$pop, na.rm = TRUE))

    })

    gdp_pop <- do.call("rbind", gdp_pop_l)

    gdp_capita <- transform(gdp_pop, gdp_capita = gdp / pop)

    gdp_capita <- transform(gdp_capita,
                         gdp_growth = c(NA, gdp_capita[-1] / gdp_capita[-nrow(gdp_pop)] - 1))

    gdp_growth <- gdp_capita[, c("year", "gdp_growth")]

    rownames(gdp_growth) <- NULL

    gdp_growth <- mean(gdp_growth$gdp_growth[-1])

    # computing row  death rate

    row_countries <- setdiff(btransfer::country_list, country_iso)

    row_countries <- setdiff(btransfer::wb_series$iso3c, c(iso3c, "WLD"))

    death_rt <- subset(btransfer::wb_series, iso3c %in% row_countries & year > policy_yr - h &
                         year <= policy_yr,
                       select = c(iso3c, year, death_rate, pop))


    death_rt_l <- split(death_rt, death_rt$year)

    death_rt_l <- lapply(death_rt_l, function(x) {

      data <- stats::na.omit(x) # removes NA rows

      stats::weighted.mean(data$death_rate, data$pop, na.rm = TRUE)

    })

    death_rt <- mean(do.call("rbind", death_rt_l))

    # computing SDR (using eta from the literature)

    sdr <- death_rt / 1000 + eta_lit * gdp_growth

    tidyr::tibble(death_rt = death_rt, eta = eta_lit, gdp_growth = gdp_growth, sdr = sdr)

  }

}

