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
#' @examples
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska"))
#'  compute_sdr("Italy", 2018, 15)
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska", aggregate = "yes"))
#'  compute_sdr(c("Italia", "Alemania", "France", "Polska", aggregate = "row"))
compute_sdr <- function(country, policy_yr = 2019, h = 10, aggregate = FALSE, eta_lit = 1.35){

# Identifying the iso3c codes of the countryies of interest. Names can be provided in any language
  iso3c <- countrycode::countryname(sourcevar = country,
                                  destination = "iso3c")

# identifying gdp per capita growth rates

  if (aggregate == FALSE) {

  # Downloading data from the WB API

  wb_series <- wbstats::wb_data(c("NY.GDP.PCAP.KD.ZG", "SP.DYN.CDRT.IN"),
                                 country = iso3c,
                                 start_date = policy_yr - 9, end_date = policy_yr)

  wb_series <- wb_series[, c("iso3c", "date", "NY.GDP.PCAP.KD.ZG", "SP.DYN.CDRT.IN")]

  wb_series$NY.GDP.PCAP.KD.ZG <- wb_series$NY.GDP.PCAP.KD.ZG / 100 # adjusting growth rates (original data are in %)

  # computing average growth rates
  if(length(unique(wb_series$iso3c)) > 1){

    wb_series_l <- split(wb_series, wb_series$iso3c)

    gdp_growth_l <- lapply(wb_series_l, function(x){
    data.frame(iso3c = unique(x$iso3c),
               gdp_growth = mean(x$NY.GDP.PCAP.KD.ZG))
  })

  gdp_growth <- do.call("rbind", gdp_growth_l)

  rownames(gdp_growth) <- NULL



  }else{

    gdp_growth <- data.frame(iso3c = unique(wb_series$iso3c),
                             gdp_growth = mean(wb_series$NY.GDP.PCAP.KD.ZG))

  }

  # computing average ten-year death rate for selected countries

  death_rt_l <- lapply(wb_series_l, function(x){
    data.frame(iso3c = unique(x$iso3c),
               death_rt = mean(x$SP.DYN.CDRT.IN))
  })

  death_rt <- do.call("rbind", death_rt_l)

  rownames(death_rt) <- NULL


  # downloading taxation data from OECD
  # Taxing Wages - Comparative tables ID: AWCOMP

  # setting filters for data extraction (2_5 net personal average tax rate, 3_1 net personal marginal tax rate)

  oecd_filter <- list(c("2_5", "3_1"), "SINGLE2") # used data for single people earning 110% of average salary

  tax_data <- OECD::get_dataset("AWCOMP", filter = oecd_filter)

  tax_data <- within(tax_data,{
    year <- as.numeric(Time)
    value <- as.numeric(ObsValue)
  })

  tax_data <- subset(tax_data, year <= policy_yr & year > policy_yr - h & COU %in% iso3c,
                     select = c("COU", "INDICATOR", "year", "value"))

  # computing etas for countries with tax data

  split.vrs <- c("INDICATOR", "COU")

  tax_data_l <- split(tax_data, tax_data[, split.vrs])

  tax_data_l <- lapply(tax_data_l, function(x){
    data.frame(iso3c = unique(x$COU),
               indicator = unique(x$INDICATOR),
               value = mean(x$value) / 100)
  })

  tax_data <- do.call("rbind", tax_data_l)

  rownames(tax_data) <- NULL

  tax_data <- tidyr::pivot_wider(tax_data, names_from = indicator, values_from = value)

  # data frame with eta values by country
  eta_real <- within(tax_data, {
    eta <- log(1 - `3_1`) / log(1 - `2_5`)
  })[,c("iso3c", "eta")]

  # assigning eta from the literature to other countries

  eta_est <- data.frame(iso3c = setdiff(iso3c, eta_real$iso3c),
                        eta = rep(eta_lit, length(setdiff(iso3c, eta_real$iso3c))))

  eta <- rbind(eta_real, eta_est)


  sdr <- Reduce(merge, list(gdp_growth, death_rt, eta))

  sdr <- within(sdr, {
    country_name <- countrycode::countrycode(iso3c, origin = "iso3c",
                                             destination = "country.name")
    sdr <- death_rt / 1000 + eta * gdp_growth
  })

  sdr <- subset(sdr, select = c("iso3c", "country_name", "gdp_growth", "death_rt", "eta", "sdr"))

  tidyr::as_tibble(sdr)

  } else if (aggregate == "yes") { # aggregates analysis

    # In case of aggregates, average growth rates must be computed from gdp and population data

    wb_series <- wbstats::wb_data(c("NY.GDP.MKTP.KD", "SP.POP.TOTL", "SP.DYN.CDRT.IN"), country = iso3c,
                                start_date = policy_yr - h, end_date = policy_yr)

    gdp_pop <- wb_series[, c("iso3c", "date", "NY.GDP.MKTP.KD", "SP.POP.TOTL")]

    gdp_pop_l <- split(gdp_pop, gdp_pop$date)

    gdp_pop_l <- lapply(gdp_pop_l, function(x){
      data.frame(year = unique(x$date),
                 gdp = sum(x$NY.GDP.MKTP.KD),
                 pop = sum(x$SP.POP.TOTL))
    })

    gdp_pop <- do.call("rbind", gdp_pop_l)

    gdp_pop$gdp_capita <- gdp_pop$gdp / gdp_pop$pop

    gdp_pop <- transform(gdp_pop,
                            gdp_growth = c(NA, gdp_capita[-1] / gdp_capita[-nrow(gdp_pop)] - 1))

    gdp_growth <- gdp_pop[, c("year", "gdp_growth")]

    rownames(gdp_growth) <- NULL

    gdp_growth <- mean(gdp_growth$gdp_growth[-1])

    # computing aggregate death rates

    death_rt <- subset(wb_series, date != policy_yr - 10,
                       select = c(iso3c, date, SP.DYN.CDRT.IN, SP.POP.TOTL))

    # computing year-by-year avg death rates of the aggregate

    death_rt_l <- split(death_rt, death_rt$date)

    death_rt_l <- lapply(death_rt_l, function(x){
      data.frame(year = unique(x$date),
                 death_rt = weighted.mean(x$SP.DYN.CDRT.IN, x$SP.POP.TOTL))
    })

    death_rt <- do.call("rbind", death_rt_l)

    rownames(death_rt) <- NULL

    death_rt <- mean(death_rt$death_rt)

    # computing aggregate eta

    oecd_filter <- list(c("2_5", "3_1"), "SINGLE2") # used data for single people earning 110% of average salary

    tax_data <- OECD::get_dataset("AWCOMP", filter = oecd_filter)

    tax_data <- within(tax_data,{
      year <- as.numeric(Time)
      value <- as.numeric(ObsValue)
    })

    # list of countries with available tax data
    tax_data_countries <- unique(tax_data$COU)

    tax_data <- subset(tax_data, year <= policy_yr & year > policy_yr - h & COU %in% iso3c,
                       select = c("COU", "INDICATOR", "year", "value"))

    # computing eta

    tax_data <- tidyr::pivot_wider(tax_data, names_from = INDICATOR, values_from = value)

    pop <- wb_series[, c("iso3c", "date", "SP.POP.TOTL")]

    pop <- subset(pop, date != policy_yr - h)

    pop_l <- split(pop, pop$iso3c)

    pop <- do.call("rbind", lapply(pop_l, function(x){
      data.frame(COU = unique(x$iso3c),
                 pop = mean(x$SP.POP.TOTL))
    }
    )
    )

    rownames(pop) <- NULL

    #tax_data <- merge(tax_data, pop) # adding pop data to compute weighted averages

    tax_data_l <- split(tax_data, tax_data$COU)

    tax_data <- do.call("rbind", lapply(tax_data_l, function(x){
      data.frame(COU = unique(x$COU),
                 mar = mean(x$`3_1`) / 100,
                 avg = mean(x$`2_5`) / 100
      )}
    )
    )

    rownames(tax_data) <- NULL

    # computing eta for countries with tax data

    eta_real <- within(tax_data, {
      eta <- log(1 - mar) / log(1 - avg)
      mar <- NULL
      avg <- NULL
    })

    # assigning eta from the literature survey (see Drupp et al.): 1.35

    eta_est <- data.frame(COU = setdiff(iso3c, eta_real$COU),
                          eta = rep(eta_lit, length(setdiff(iso3c, eta_real$COU))))

    # computing weighted average aggregate eta (population is the weights)

    eta <- rbind(eta_real, eta_est)

    eta <- merge(eta, pop)

    eta <- weighted.mean(eta$eta, eta$pop) # weighted average of eta for the aggregate considered

  # computing the SDR

    sdr <- death_rt / 1000 + eta * gdp_growth

    tidyr::tibble(death_rt = death_rt, eta = eta, gdp_growth = gdp_growth, sdr = sdr)
  } else if (aggregate == "row"){ # computes the sdr of the rest of the world (with respect to selected countries)

    all_countries <- subset(wbstats::wb_countries(), region != "Aggregates")$iso3c # all countries in WB database

    wb_series <- wbstats::wb_data(c("NY.GDP.MKTP.KD", "SP.POP.TOTL", "SP.DYN.CDRT.IN"),
                                  country = c(all_countries, "WLD"),
                                  start_date = policy_yr - h, end_date = policy_yr)

    gdp_wld <- subset(wb_series, iso3c == "WLD",
                      select = c("date", "NY.GDP.MKTP.KD", "SP.POP.TOTL"))

    gdp_pop <- subset(wb_series,
                      select = c("iso3c", "date", "NY.GDP.MKTP.KD", "SP.POP.TOTL"))

    gdp_pop <- gdp_pop[which(gdp_pop$iso3c %in% iso3c), , drop = FALSE]

    gdp_pop_l <- split(gdp_pop, gdp_pop$date)

    gdp_pop_l <- lapply(gdp_pop_l, function(x){
      data.frame(date = unique(x$date),
                 gdp = sum(x$NY.GDP.MKTP.KD, na.rm = T),
                 pop = sum(x$SP.POP.TOTL, na.rm = T))
    })

    gdp_pop <- do.call("rbind", gdp_pop_l)

    gdp_pop_row <- merge(gdp_pop, gdp_wld)

    gdp_pop_row <- within(gdp_pop_row, {
      gdp_capita <- (NY.GDP.MKTP.KD - gdp) / (SP.POP.TOTL - pop)
    })

    gdp_pop_row <- gdp_pop_row[, c("date", "gdp_capita")]

    gdp_pop_row <- transform(gdp_pop_row,
                         gdp_growth = c(NA, gdp_capita[-1] / gdp_capita[-nrow(gdp_pop_row)] - 1))

    gdp_growth <- gdp_pop_row[, c("date", "gdp_growth")]

    names(gdp_growth) <- c("year", "gdp_growth")

    gdp_growth <- mean(gdp_growth$gdp_growth[-1])

    # computing row  death rate

    row_countries <- setdiff(wb_series$iso3c, c(iso3c, "WLD"))

    death_rt <- subset(wb_series, iso3c %in% row_countries,
                       select = c(iso3c, date, SP.DYN.CDRT.IN, SP.POP.TOTL))


    death_rt_l <- split(death_rt, death_rt$date)

    death_rt_l <- lapply(death_rt_l, function(x) {

      data <- na.omit(x) # removes NA rows

      weighted.mean(data$SP.DYN.CDRT.IN, data$SP.POP.TOTL, na.rm = TRUE)

    })

    death_rt <- mean(do.call("rbind", death_rt_l))

    # computing SDR (using eta from the literature)

    sdr <- death_rt / 1000 + eta_lit * gdp_growth

    tidyr::tibble(death_rt = death_rt, eta = eta_lit, gdp_growth = gdp_growth, sdr = sdr)

  }

}

