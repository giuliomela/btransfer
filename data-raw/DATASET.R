## code to prepare `DATASET` dataset goes here

latest_yr <- 2021

# list of countries (WB) - no aggregates (ISO codes only)

country_list <- wbstats::wb_countries()

country_list <- country_list[country_list$region != "Aggregates", ]$iso3c

# loading income classification
income_class <- read.csv(here::here("data-raw/income_class.csv"), sep = ";")

# Loading composition of maritime aggregates included in the Handbook on the external cost of transport

agg_composition <- readxl::read_excel(here::here("data-raw/agg_composition.xlsx"))


usethis::use_data(income_class, country_list, agg_composition,
                  overwrite = TRUE)

