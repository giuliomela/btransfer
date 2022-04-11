## code to prepare `DATASET` dataset goes here

latest_yr <- 2020

eta_lit <- 1.35 # value of eta (an element of Ramsey's rule) from the literature

# list of countries (WB) - no aggregates (ISO codes only)

country_list <- wbstats::wb_countries()

country_list <- country_list[country_list$region != "Aggregates", ]$iso3c

# loading income classification
income_class <- read.csv(here::here("data-raw/income_class.csv"), sep = ";")

# downloading data from the world bank database

wb_series <- wbstats::wb_data(c("NY.GDP.PCAP.PP.KD", "NY.GDP.MKTP.PP.KD",
                   "SP.POP.TOTL", "NY.GNP.ATLS.CD", "NY.GDP.DEFL.ZS", "PA.NUS.FCRF", "SP.DYN.CDRT.IN"),
                   country = c("all", "WLD", "EUU"))

names(wb_series) <- c("iso2c", "iso3c", "country", "year", "gdp_defl", "gdp", "gdp_capita",
                      "gni", "exc_rate", "death_rate", "pop")

wb_series <- wb_series[, -1]

# expressing gni in USD at the price levels by the WB for latest income classification

defl_fct <- subset(wb_series, iso3c == "USA", select = c(year, gdp_defl))

defl_latest_yr <- defl_fct[defl_fct$year == latest_yr, ]$gdp_defl

defl_fct$defl_fct <- defl_latest_yr / defl_fct$gdp_defl

defl_fct$gdp_defl <- NULL

wb_series <- merge(wb_series, defl_fct)

wb_series$gni <- wb_series$gni * wb_series$defl_fct

wb_series$defl_fct <- NULL

# downloading wb data growth rates (for benefit transfers to the future)

wb_growth <- wbstats::wb_data(c("NY.GDP.PCAP.KD.ZG", "SP.POP.GROW",
                                "NY.GNP.MKTP.KD.ZG", "NY.GDP.MKTP.KD.ZG"),
                              country = c("all", "WLD", "EUU"))

names(wb_growth) <- c("iso2c", "iso3c", "country", "year", "gdp_growth", "gdp_capita_growth",
                      "gni_growth", "pop_growth")

wb_growth <- wb_growth[, -1]

# downloading Euro Area gdp deflator

gdp_defl_eurostat <- eurostat::get_eurostat("nama_10_gdp", time_format = "num",
                                   filters = list(unit = "PD15_EUR", na_item = "B1GQ"))

gdp_defl_eurostat <- gdp_defl_eurostat[, -c(1,2)]

names(gdp_defl_eurostat) <- c("eu_code", "year", "gdp_defl")

# Downloading WB income classification

income_lvs <- wbstats::wb_countries()[, c("iso3c", "income_level_iso3c", "income_level")]

income_lvs <- subset(income_lvs, !is.na(income_level_iso3c))

# computing income level for the world aggregate

gni_capita_wld <- subset(wb_series, iso3c == "WLD" & year == latest_yr)

gni_capita_wld <- gni_capita_wld$gni /gni_capita_wld$pop

epsilon_agg <- income_class

epsilon_agg$gni_wld <- ifelse(gni_capita_wld < income_class$max & gni_capita_wld >= income_class$min,
                              TRUE, FALSE)

income_class_wld <- data.frame(iso3c = "WLD",
                               income_level_iso3c = epsilon_agg[epsilon_agg$gni_wld == TRUE, ]$income_level_iso3c,
                               income_level = epsilon_agg[epsilon_agg$gni_wld == TRUE, ]$income_level)

income_lvs <- rbind(income_lvs, income_class_wld)

# Computing etas for benefit transfer

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


usethis::use_data(income_class, wb_series, gdp_defl_eurostat, income_lvs, wb_growth, tax_data,
                  country_list,
                  overwrite = TRUE)

