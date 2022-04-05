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


    # assigning epsilon according to income levels and computing transfer factor (adjusted for inflation)

    bt_fct <- merge(wb_series, epsilon, all.x = TRUE)

    study_site_gdp <- subset(bt_fct, date == study_yr & iso3c == study_site)$NY.GDP.PCAP.PP.KD

    bt_fct <- subset(bt_fct, date == policy_yr)

    bt_fct$epsilon <- as.numeric(bt_fct$epsilon)

    bt_fct <- within(bt_fct, { # value transfer factors adjusted for inflation

      bt_fct <- ((NY.GDP.PCAP.PP.KD / study_site_gdp)^epsilon) * gdp_defl_fct

    })

 if (currency == "EUR") {

   bt_fct

 } else if (currency == "USD") {

   bt_fct$bt_fct <- bt_fct$bt_fct* us_euro

   bt_fct
 }


  } else if (aggregate == "yes") {

    # downloading gdp (PPP) and population data to compute aggregate's gdp per capita.
    # donwloading also GNI per capita (current USD) to identify appropriate epsilon

    # downloading gdp per capita data of the study site

    study_site_gdp <- wbstats::wb_data(c("NY.GDP.PCAP.PP.KD"),
                     country = iso_study,
                     start_date = study_yr, end_date = policy_yr)

    study_site_gdp <- subset(study_site_gdp, date == study_yr & iso3c == study_site)$NY.GDP.PCAP.PP.KD

    wb_series <- wbstats::wb_data(c("NY.GDP.MKTP.PP.KD", "SP.POP.TOTL"),
                                  country = iso_policy,
                                  start_date = study_yr, end_date = policy_yr)

    gdp_pop <- wb_series[, c("iso3c", "date", "NY.GDP.MKTP.PP.KD",
                               "SP.POP.TOTL")]

    bt_fct <- merge(gdp_pop, epsilon, all.x = TRUE)

    bt_fct$epsilon <- as.numeric(bt_fct$epsilon)

    bt_fct_l <- split(bt_fct, bt_fct$date)

    bt_fct_l <- lapply(bt_fct_l, function (x) {

      data.frame(date = unique(x$date),
                 NY.GDP.MKTP.PP.KD = sum(x$NY.GDP.MKTP.PP.KD),
                 SP.POP.TOTL = sum(x$SP.POP.TOTL),
                 epsilon = weighted.mean(x$epsilon, x$SP.POP.TOTL))
    })

    bt_fct <- do.call("rbind", bt_fct_l)

    bt_fct <- within(bt_fct, {

      gdp_capita <- NY.GDP.MKTP.PP.KD / SP.POP.TOTL
      NY.GDP.MKTP.PP.KD <- NULL
      SP.POP.TOTL <- NULL

    })

    # calculating trasnfer factors

    bt_fct <- subset(bt_fct, date == policy_yr)

    bt_fct <- within(bt_fct, { # value transfer factors adjusted for inflation

      bt_fct <- ((gdp_capita / study_site_gdp)^epsilon) * gdp_defl_fct

    })

    if (currency == "EUR") {

      bt_fct

    } else if (currency == "USD") {

      bt_fct$bt_fct <- bt_fct$bt_fct* us_euro

      bt_fct
    }

  }else if (aggregate == "row")


}
