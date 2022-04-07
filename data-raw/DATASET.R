## code to prepare `DATASET` dataset goes here

# loading income classification
income_class <- read.csv(here::here("data-raw/income_class.csv"), sep = ";")

# downloading data from the world bank database

wb_series <- wbstats::wb_data(c("NY.GDP.PCAP.PP.KD", "NY.GDP.MKTP.PP.KD",
                   "SP.POP.TOTL", "NY.GNP.ATLS.CD", "NY.GDP.DEFL.ZS", "PA.NUS.FCRF", ),
                   country = c("all", "WLD", "EUU"))

names(wb_series) <- c("iso2c", "iso3c", "country", "year", "gdp_defl", "gdp", "gdp_capita",
                      "gni", "exc_rate", "pop")

wb_series <- wb_series[, -1]

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

usethis::use_data(income_class, wb_series, gdp_defl_eurostat, income_lvs, wb_growth, overwrite = TRUE)

