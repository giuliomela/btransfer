#' Returns macro-economic variables for selected countries and years
#'
#' This function extracts and returns macro-economic variables from the
#' World Bank database (via DbNomics). The user can specify the country and the year of interest.
#' In case the selected year is in the future, the function estimates future
#' values of the selected variable based on the average growth rates of the last
#' `n` years (a parameter that can, in turn, be chosen by the user).
#' The function downloads the most recent data avilable.
#'
#' @param country_iso A string. The ISO code (3 digits) of the country of interest.
#'     Codes `WLD` and `EUU` stand for the world and the European Union
#'     respectively. In case `agg` is set to either `yes` or `row`
#'     more than one iso code can be provided.
#' @param ref_yr A double. Year of interest. Can be a year into the future up
#'     to 2050. Years before 1961 are not accepted neither.
#' @param var A string. The name of the macro-economic variable to be extracted.
#'     It can assume the following values: `gdp`, `gdp_capita`, `gni`,
#'     `pop`, `gni_capita` and `death_rate`.
#' @param agg A string. It can assume three values: `no`, the default, `yes`
#'     and `row`. Indicates whether the variable of interest must be computed for an
#'     aggregate. In case `row`, the ROW is built as the difference between the selected
#'     countries from the world total.
#' @param growth_rate_int a numeric value. The number of years to be considered to calculate
#'     average growth rates for the variable of interest in case `ref_yr` is set into the
#'     future.
#' @return A list with the value of the selected variable for the country and year
#'     of interest (`value`) and the latest year of avilable data used in the computarion (`last_value`)
compute_macro_var <- function (country_iso, ref_yr, var = "gdp_capita", agg = "no",
                               growth_rate_int = 10) {

  iso3c <- year <- NULL

  if (ref_yr > 2050 | ref_yr < 1961) stop ("Please provide a valid year. Year
                                              must be between 1961 and 2050")

  if (!is.element(var, c("gdp", "gni", "gdp_capita", "death_rate",
  "pop", "gni_capita"))) stop("Please provide a valid variable name")

  if (length(country_iso) > 1 & agg == "no") stop ("Multiple countries can be
                                                         selected only if 'agg' option
                                                         is set to either 'yes' or 'row'")

  # Downloading datasets from DBnomics

if (agg == "no") {

  code <- dplyr::case_when(
    var == "gdp_capita" ~ "NY.GDP.PCAP.PP.KD", # constant PPP
    var == "gni_capita" ~ "NY.GNP.PCAP.CD", # current
    var == "gdp" ~ "NY.GDP.MKTP.PP.KD", # constant PPP
    var == "gni" ~ "NY.GNP.ATLS.CD", # current
    var == "pop" ~ "SP.POP.TOTL",
    var == "death_rate" ~ "SP.DYN.CDRT.IN"
  )

} else if (agg %in% c("row", "yes")) {

  code <- dplyr::case_when(
    var == "gdp_capita" ~ c("NY.GDP.MKTP.PP.KD", "SP.POP.TOTL"),
    var == "gni_capita" ~ c("NY.GNP.ATLS.CD", "SP.POP.TOTL"),
    var == "gdp" ~ "NY.GDP.MKTP.PP.KD",
    var == "gni" ~ "NY.GNP.ATLS.CD",
    var == "pop" ~ "SP.POP.TOTL",
    var == "death_rate" ~ c("SP.DYN.CDRT.IN", "SP.POP.TOTL")
  )

}

  if (agg == "row") {

    countries <- c(country_iso, "WLD")

  } else {

    countries <- country_iso

  }

  # defining codes to download

  codes <- as.vector(sapply(countries,
                 function(x) paste0("WB/WDI/A-", code, "-", x
                 )
  )
  )

  codes <- unique(codes) # removing duplicate codes referring to the WLD

  # Downloading codes

  data_raw <- rdbnomics::rdb(codes)

  data_raw <- data_raw[, c("original_period", "series_code", "value")]

  data_raw$original_period <- as.numeric(data_raw$original_period)

  last_yr <- max(data_raw[!is.na(data_raw$value), ][["original_period"]]) # last year of available data

  data_raw$countries <- stringr::str_sub(data_raw$series_code, -3) # creating a variable with iso name



# Computing the variable of interest

  if (agg %in% c("row", "yes")) {

      if (agg == "row") {

        data_raw$value <- ifelse(data_raw$countries == "WLD",
                                 data_raw$value,
                                 data_raw$value * -1)
      }

      data_raw$series_code <- ifelse(grepl("POP", data_raw$series_code, fixed = TRUE),
                                     "denom",
                                     "nom") # identifying nominator and denominator variables


      if (var == "death_rate") {

        # in case death rate is chosen, first the total number of deaths must be calculated from death rates of single countries

        data_raw <- data_raw %>%
          tidyr::pivot_wider(names_from = series_code, values_from = value) %>%
          dplyr::mutate(nom = nom * denom) %>%
          tidyr::pivot_longer(names_to = "series_code", values_to = "value")

      }

      data_raw <- data_raw %>%
        dplyr::group_by(series_code, original_period) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = series_code, values_from = value)

      if (var %in% c("gdp", "pop", "gni")){

        data_raw$value <- data_raw$nom

      } else {

        data_raw$value <- data_raw$nom / data_raw$denom

      }

  }

  if(ref_yr <= last_yr) {

    value <- data_raw[data_raw$original_period == ref_yr, ]$value # value for the desired year

  } else {

    hist_period <- seq(last_yr - growth_rate_int + 1, last_yr, by = 1)

    data_raw <- data_raw[data_raw$original_period %in% hist_period, ]

    if (var %in% c("gni", "gni_capita")) {

      # Since gni and gni capita are in nominal terms, values must be corrected for inflation

      gdp_defl_us <- rdbnomics::rdb("WB/WDI/A-NY.GDP.DEFL.ZS-USA")[, c("original_period", "value")]

      names(gdp_defl_us) <- c("original_period", "defl")

      gdp_defl_us$original_period <- as.numeric(gdp_defl_us$original_period)

      data_raw <- merge(data_raw, gdp_defl_us)

      data_raw$value <- data_raw$value / data_raw$defl *
        data_raw[data_raw$original_period == last_yr, ]$defl

      data_raw$defl <- NULL

    }

    growth_rate <- compute_growth_rate(data_raw$value, avg = TRUE) # average growth rate of the variable

    fcast_h <- ref_yr - last_yr

    value <- data_raw[data_raw$original_period == last_yr, ]$value * (1 + growth_rate)^fcast_h # forecasted value


  }

  list(value = value, last_yr = last_yr)

}
