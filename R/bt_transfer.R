bt_transfer <- function (study_site = "EUU", policy_site, study_yr = 2016, policy_yr = 2019,
                         aggregate = "no", currency = "EUR") {

  # income levels and epsilon

  income_lvs <- wbstats::wb_countries()[, c("iso3c", "income_level_iso3c", "income_level")]

  income_lvs <- subset(income_lvs, !is.na(income_level_iso3c))

  epsilon <- income_class[, c("income_level_iso3c", "epsilon")]

  epsilon <- merge(income_lvs, epsilon)[, c("iso3c", "epsilon")]

  epsilon$epsilon <- as.numeric(epsilon$epsilon)

  # adding epsilon for the EUU

  epsilon[nrow(epsilon) + 1,] = c("EUU", 0.2)

  # Downloading GDP deflators

  if (currency == "EUR") {

    # downloading eurozone gdp deflator, for damage cost factors expressed in euro

    gdp_defl <- eurostat::get_eurostat("nama_10_gdp", time_format = "num",
                             filters = list(unit = "PD15_EUR", na_item = "B1GQ"))

    gdp_defl_study <- subset(gdp_defl, geo == "EA" & time == study_yr)$values

    # multiplying factor to be used to account for inflation from study to policy year
    gdp_defl_fct <- subset(gdp_defl, geo == "EA" & time == policy_yr)$values / gdp_defl_study

  } else if (currency == "USD") {

    defl_exc <- wbstats::wb_data(c("NY.GDP.DEFL.ZS", "PA.NUS.FCRF"), country = c("EMU", "USA"),
                     start_date = study_yr, end_date = policy_yr)

    gdp_defl_study <- subset(defl_exc, date == study_yr & iso3c == "USA")$NY.GDP.DEFL.ZS

    gdp_defl_fct <- subset(defl_exc, date == policy_yr & iso3c == "USA")$NY.GDP.DEFL.ZS / gdp_defl_study

    # USD-EUR policy year exchange rate. USD per EUR (values in dollars ,ust be multiplied for this factor)

    us_euro <- subset(defl_exc, date == policy_yr & iso3c == "EMU")$PA.NUS.FCRF

    }

  # identifying iso3c codes of provided study and policy sites

  if (study_site == "EUU") {

    iso_study <- study_site

    iso_policy <- countrycode::countryname(policy_site, "iso3c")

  } else {

    iso_study <- countrycode::countryname(study_site, "iso3c")

    iso_policy <- countrycode::countryname(policy_site, "iso3c")

  }

  if (aggregate == "no") {

    # donwloading gdp per capita data (PPP) and GNI per capita (Atlas method)

    wb_series <- wbstats::wb_data(c("NY.GDP.PCAP.PP.KD"),
                                  country = c(iso_study, iso_policy),
                     start_date = study_yr, end_date = policy_yr)

    wb_series <- wb_series[, c("iso3c", "date", "NY.GDP.PCAP.PP.KD")]


    # assigning epsilon according to income levels

    bt_fct <- merge(wb_series, epsilon, all.x = TRUE)

    study_site_gdp <- subset(bt_fct, date == study_yr & iso3c == study_site)$NY.GDP.PCAP.PP.KD

    bt_fct <- subset(bt_fct, date == policy_yr)

    bt_fct <- within(bt_fct, {

      bt_fct <- (NY.GDP.PCAP.PP.KD / study_site_gdp)^epsilon

    })




  }



}
