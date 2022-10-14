## code to prepare `DATASET` dataset goes here

latest_yr <- 2021

eta_lit <- 1.35 # value of eta (an element of Ramsey's rule) from the literature

# list of countries (WB) - no aggregates (ISO codes only)

country_list <- wbstats::wb_countries()

country_list <- country_list[country_list$region != "Aggregates", ]$iso3c

# loading income classification
income_class <- read.csv(here::here("data-raw/income_class.csv"), sep = ";")

# building a database with epsilon associated to income levels

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

# Loading composition of maritime aggregates included in the Handbook on the external cost of transport

agg_composition <- readxl::read_excel(here::here("data-raw/agg_composition.xlsx"))


usethis::use_data(income_class, wb_series, gdp_defl_eurostat, wb_growth, tax_data,
                  country_list, agg_composition,
                  overwrite = TRUE)

