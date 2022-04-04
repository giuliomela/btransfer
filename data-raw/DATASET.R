## code to prepare `DATASET` dataset goes here

# loading income classification
income_class <- read.csv(here::here("data-raw/income_class.csv"), sep = ";")

usethis::use_data(income_class, overwrite = TRUE)
