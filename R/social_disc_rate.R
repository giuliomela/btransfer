compute_sdr <- function(country, policy_yr, h){

# Identifying the iso3c codes of the countryies of interest. Names can be provided in any language
  iso3c <- countrycode::countryname(sourcevar = country,
                                  destination = "iso3c")

# Downloading gdp per capita growth data from the WB API

  gdp_growth <- wbstats::wb_data("NY.GDP.PCAP.KD.ZG", country = iso3c)

  gdp_growth <- gdp_growth[, c("iso3c", "date", "NY.GDP.PCAP.KD.ZG")]

  names(gdp_growth) <- c("iso3c", "year", "gdp_growth")

  gdp_growth <- subset(gdp_growth, year <= policy_yr & year > policy_yr - h)

  gdp_growth$gdp_growth <- gdp_growth$gdp_growth / 100

  # computing average growth rates
  if(length(unique(gdp_growth$iso3c)) > 1){

  gdp_growth_l <- split(gdp_growth, gdp_growth$iso3c)

  gdp_growth_l <- lapply(gdp_growth_l, function(x){
    data.frame(iso3c = unique(x$iso3c),
               gdp_growth = mean(x$gdp_growth))
  })

  gdp_growth <- do.call("rbind", gdp_growth_l)

  rownames(gdp_growth) <- NULL

  }else{

    gdp_growth <- data.frame(iso3c = unique(gdp_growth$iso3c),
                             gdp_growth = mean(gdp_growth$gdp_growth))

  }

# downloading mortality rates for the computation of the social discount rate (deaths per 1000 people)

  death_rt <- wbstats::wb_data("SP.DYN.CDRT.IN", country = country)

  death_rt <- death_rt[, c("iso3c", "date", "SP.DYN.CDRT.IN")]

  names(death_rt) <- c("iso3c", "year", "death_rt")

# computing average ten-year death rate for selected countries

  death_rt <- subset(death_rt, year %in% seq(policy_yr - h + 1, policy_yr, 1))

  death_rt_l <- split(death_rt, death_rt$iso3c)

  death_rt_l <- lapply(death_rt_l, function(x){
    data.frame(iso3c = unique(x$iso3c),
               death_rt = mean(x$death_rt))
  })

  death_rt <- do.call("rbind", death_rt_l)

  rownames(death_rt) <- NULL

# downloading taxation data from OECD
# Taxing Wages - Comparative tables ID: AWCOMP

  # setting filters for data extraction (2_5 net personal average tax rate, 3_1 net personal marginal tax rate)

  oecd_filter <- list(c("2_5", "3_1"), "SINGLE2", iso3c) # used data for single people earning 110% of average salary

  tax_data <- OECD::get_dataset("AWCOMP", filter = oecd_filter)

  tax_data <- within(tax_data,{
    year <- as.numeric(Time)
    value <- as.numeric(ObsValue)
  })

  tax_data <- subset(tax_data, year <= policy_yr & year > policy_yr - h,
                     select = c("COU", "INDICATOR", "year", "value"))

  # computing etas

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
  eta <- within(tax_data, {
    eta <- log(1 - `3_1`) / log(1 - `2_5`)
  })[,c("iso3c", "eta")]


  sdr <- Reduce(merge, list(gdp_growth, death_rt, eta))

  sdr <- within(sdr,{
    sdr <- death_rt / 1000 + eta * gdp_growth
  })

  sdr

}
