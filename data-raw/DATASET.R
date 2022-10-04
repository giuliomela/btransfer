## code to prepare `DATASET` dataset goes here

latest_yr <- 2021

eta_lit <- 1.35 # value of eta (an element of Ramsey's rule) from the literature

# list of countries (WB) - no aggregates (ISO codes only)

country_list <- wbstats::wb_countries()

country_list <- country_list[country_list$region != "Aggregates", ]$iso3c

# loading income classification
income_class <- read.csv(here::here("data-raw/income_class.csv"), sep = ";")

# building a database with epsilon associated to income levels



# downloading data from the world bank database

wb_series <- wbstats::wb_data(c("NY.GDP.PCAP.PP.KD", "NY.GDP.MKTP.PP.KD",
                   "SP.POP.TOTL", "NY.GNP.ATLS.CD", "NY.GDP.DEFL.ZS", "PA.NUS.FCRF", "SP.DYN.CDRT.IN"),
                   country = c("all", "WLD", "EUU"))

names(wb_series) <- c("iso2c", "iso3c", "country", "year", "gdp_defl", "gdp", "gdp_capita",
                      "gni", "exc_rate", "death_rate", "pop")

wb_series <- wb_series[, -1]

wb_series$deaths <- wb_series$death_rate * wb_series$pop

# expressing gni in USD at the price levels by the WB for latest income classification

defl_fct <- subset(wb_series, iso3c == "USA", select = c(year, gdp_defl))

defl_latest_yr <- defl_fct[defl_fct$year == latest_yr, ]$gdp_defl

defl_fct$defl_fct <- defl_latest_yr / defl_fct$gdp_defl

defl_fct$gdp_defl <- NULL

wb_series <- merge(wb_series, defl_fct)

wb_series$gni <- wb_series$gni * wb_series$defl_fct

wb_series$defl_fct <- NULL

# computing GNI per capita

wb_series$gni_capita <- wb_series$gni / wb_series$pop

wb_series <- dplyr::as_tibble(wb_series)

# Computing growth rates

selected_series <- wb_series[, c("year", "iso3c", "gdp", "gdp_capita", "gni", "gni_capita", "pop")]

wb_growth_g <- dplyr::group_by(selected_series, iso3c)

wb_growth <- dplyr::mutate(wb_growth_g, dplyr::across(gdp:pop, btransfer:::compute_growth_rate,
                                                      .names = "{.col}_growth"))

wb_growth <- dplyr::ungroup(wb_growth)

wb_growth <- dplyr::select(wb_growth, c(year, iso3c, dplyr::ends_with("growth")))

wb_growth <- dplyr::arrange(wb_growth, iso3c, year)

# downloading Euro Area gdp deflator

gdp_defl_eurostat <- eurostat::get_eurostat("nama_10_gdp", time_format = "num",
                                   filters = list(unit = "PD15_EUR", na_item = "B1GQ"))

gdp_defl_eurostat <- gdp_defl_eurostat[, -c(1,2)]

names(gdp_defl_eurostat) <- c("eu_code", "year", "gdp_defl")


# downloading taxation data from OECD (to be used in SDR calculation)
# Taxing Wages - Comparative tables ID: AWCOMP

# setting filters for data extraction (2_5 net personal average tax rate, 3_1 net personal marginal tax rate)

oecd_filter <- list(c("2_5", "3_1"), "SINGLE2") # used data for single people earning 110% of average salary

tax_data <- OECD::get_dataset("AWCOMP", filter = oecd_filter)

tax_data <- within(tax_data,{
  year <- as.numeric(Time)
  value <- as.numeric(ObsValue)
  indicator <- INDICATOR
  iso3c <- COU
})

tax_data <- subset(tax_data, select = c(iso3c, year, indicator, value))


usethis::use_data(income_class, wb_series, gdp_defl_eurostat, wb_growth, tax_data,
                  country_list,
                  overwrite = TRUE)

