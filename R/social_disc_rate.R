social_disc_rate <- function(country, policy_yr){

# Identifying the iso3c codes of the countryies of interest
  iso3c <- countrycode::countrycode(country,
                                  origin = "country.name",
                                  destination = "iso3c")

# Downloading gdp data from the WB API

  gdp_capita <- wbstats::wb_data("NY.GDP.PCAP.PP.KD", country = iso3c)

  gdp_capita <- gdp_capita[, c("iso3c", "date", "NY.GDP.PCAP.PP.KD")]

  names(gdp_capita) <- c("iso3c", "year", "gdp_capita")

# downloading mortality rates for the computation of the social discount rate (deaths per 1000 people)

  death_rt <- wbstats::wb_data("SP.DYN.CDRT.IN", country = country)

  death_rt <- death_rt[, c("iso3c", "date", "SP.DYN.CDRT.IN")]

  names(death_rt) <- c("iso3c", "year", "death_rt")

# computing verage ten-year death rate for selected countries

  death_rt <- subset(death_rt, year %in% seq(policy_yr - 9, policy_yr, 1))

  death_rt_l <- split(death_rt, death_rt$iso3c)

  death_rt_l <- lapply(death_rt_l, function(x){
    data.frame(iso3c = unique(x$iso3c),
               death_rt = mean(x$death_rt))
  })

  death_rt <- do.call("rbind", death_rt_l)

  rownames(death_rt) <- NULL


}
