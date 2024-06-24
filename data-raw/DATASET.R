## code to prepare `DATASET` dataset goes here

library(tidyverse)

latest_yr <- 2022

# list of countries (WB) - no aggregates (ISO codes only)

country_list <- wbstats::wb_countries()

country_list <- country_list[country_list$region != "Aggregates", ]$iso3c

# loading income classification
income_classification <- readxl::read_xlsx(here::here("data-raw/income_class.xlsx")) %>%
  mutate(max = as.numeric(max))

# Loading composition of maritime aggregates included in the Handbook on the external cost of transport

agg_composition <- readxl::read_excel(here::here("data-raw/agg_composition.xlsx"))

### Downloading Taiwan Data

# GDP capita (constant international dollars)

# Wdition of the WEO IMF

weo_edition <- "WEO:2022-10"

gdp_capita_twn <- rdbnomics::rdb(paste0("IMF/",
                                        weo_edition,
                  "/TWN.NGDPRPPPPC.purchasing_power_parity_2017_international_dollar"))

# population

pop_twn <- rdbnomics::rdb(paste0(
  "IMF/", weo_edition, "/TWN.LP.persons"
))

pop_twn$value <- pop_twn$value * 10^6

# GNI capita

gni_capita_twn <- rdbnomics::rdb("UNCTAD/NGTAPCA/A.us-dollars-at-current-prices-per-capita.china-taiwan-province-of") # current prices

# death rate

death_rt_twn <- rdbnomics::rdb("UNDATA/DF_UNDATA_WPP/SP_DYN_CDRT.A._T._T._T.158.M")

death_rt_twn$original_period <- as.numeric(death_rt_twn$original_period)

# linear interpolation of values (UN data are available for every 5 years)

interpolated <- tibble(original_period = min(death_rt_twn$original_period):latest_yr)

interpolated <- interpolated %>%
  rowwise() %>%
  mutate(value = approx(death_rt_twn$original_period, death_rt_twn$value, xout = original_period)[["y"]]) %>%
  ungroup()

death_rt_twn <- interpolated %>%
  mutate(var = "death_rate")

# cleaning data

twn_data <- list(gdp_capita_twn, gni_capita_twn, pop_twn)

names(twn_data) <- c("gdp_capita_twn", "gni_capita_twn", "pop_twn")


twn_data <- lapply(names(twn_data),
       function (x) {
         db <- twn_data[[x]][, c("original_period", "value")]
         db$original_period = as.numeric(db$original_period)
         db$var <- str_remove(x, "_twn")
         db[db$original_period <= latest_yr, ] # retaining only actual data (some variables have forecasted values)
                    }
       )

twn_data[[6]] <- death_rt_twn

twn_data <- do.call(rbind, twn_data) # macroeconomic data of Taiwan

twn_data <- twn_data %>%
  pivot_wider(names_from = var, values_from = value) %>%
  mutate(gni = gni_capita * pop,
         gdp = gdp_capita * pop,
         deaths  = death_rate / 1000 * pop) %>% # computing total deaths, GNI and GDP from per capita data
  pivot_longer(!original_period, names_to = "var", values_to = "value") %>%
  rename(series_code = var) %>%
  mutate(countries = "TWN")

# loading Italy's NUTS2 codes

nuts2_codes <- read.csv(here::here("data-raw/nuts2_codes.csv"))

# computing GDP and GNI

usethis::use_data(income_classification, country_list, agg_composition, twn_data, nuts2_codes,
                  overwrite = TRUE)

