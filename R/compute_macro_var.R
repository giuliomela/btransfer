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
#'     `pop`, `gni_capita` and `death_rate`. GDP and GDP per capita are returned in
#'     constant international dollars (2017). GNI and GNI per capita are returned in
#'     constant USD. The base year is the same of the latest version of the WB income
#'     classification of countries, since GNI per capita is essentially used to compute
#'     the income elasticity of the marginal utility of consumption (`epsilon`).
#' @param agg A string. It can assume three values: `no`, the default, `yes`
#'     and `row`. Indicates whether the variable of interest must be computed for an
#'     aggregate. In case `row`, the ROW is built as the difference between the selected
#'     countries from the world total.
#' @param growth_rate_int a numeric value. The number of years to be considered to calculate
#'     average growth rates for the variable of interest in case `ref_yr` is set into the
#'     future.
#' @param avg A logical value. If `TRUE` the average value of the variable specified with
#'     `var` in the time window specified via `growth_rate_int` is returned. Default is set
#'     to `FALSE`. If `ref_yr` is in the future, the average value returned is always computed
#'     starting from historical data, up to the last available year.
#' @param growth_rt A logical value. If `TRUE` the average growth rate of the variable
#'     specified with `var` is computed over the time interval specified through `growth_rate_int`.
#'     If `ref_yr` is in the future, the average growth rate returned is always computed
#'     starting from historical data, up to the last available year.
#' @return A list with the value of the selected variable for the country and year
#'     of interest (`value`) and the latest year of available data used in the computation (`last_value`)
compute_macro_var <- function (country_iso, ref_yr, var = "gdp_capita", agg = "no",
                               growth_rate_int = 20, avg = FALSE, growth_rt = FALSE) {

  iso3c <- year <- series_code <- original_period <- nom <- denom <- twn_data <-
    income_classification <- defl <- NULL

  if(isTRUE(avg) & isTRUE(growth_rt))
    stop("Paramters 'avg' and 'growth_rt' cannot be both set to TRUE")

  if (ref_yr > 2050 | ref_yr < 1961) stop ("Please provide a valid year. Year
                                              must be between 1961 and 2050")

  if (!is.element(var, c("gdp", "gni", "gdp_capita", "death_rate",
  "pop", "gni_capita"))) stop("Please provide a valid variable name")

  if (length(country_iso) > 1 & agg == "no") stop ("Multiple countries can be
                                                         selected only if 'agg' option
                                                         is set to either 'yes' or 'row'")

  if ("TWN" %in% country_iso & agg == "no") {

    data_raw <- twn_data[twn_data$series_code == var, ]

    last_yr <- max(data_raw$original_period, na.rm = TRUE)

  } else {


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

    countries <- country_iso[country_iso != "TWN"] # removing Taiwan in case is included (variables added later)

    countries <- c(countries, "WLD")

  } else {

    countries <- country_iso[country_iso != "TWN"] # removing Taiwan in case is included (variables added later)


  }

  # defining codes to download

  codes <- as.vector(sapply(countries,
                 function(x) paste0("WB/WDI/A-", code, "-", x
                 )
  )
  )

  codes <- unique(codes) # removing duplicate codes referring to the WLD

  # Downloading codes

  # data_raw <- rdbnomics::rdb(codes)
  #
  # data_raw <- data_raw[, c("original_period", "series_code", "value")]


  # data_raw <- tryCatch(
  #   {
  #     rdbnomics::rdb(codes)
  #   },
  #   error = function(e) {
  #     warning(paste("Impossibile scaricare una o più serie:", paste(codes, collapse = ", ")), immediate. = TRUE)
  #     return(NULL)
  #   }
  # )

  countries_with_missing_data <- c()

  results <- lapply(codes, function(code) {
    tryCatch(
      {
        # ⚠️ Modifica: Usa capture.output per sopprimere ogni stampa su stdout
        # Combina anche suppressWarnings per catturare i warning standard,
        # sebbene l'output sia già stato soppresso.
        invisible(capture.output(
          suppressWarnings(
            data <- rdbnomics::rdb(code)
          )
        ))
        return(data)
      },
      error = function(e) {
        # Questo blocco cattura l'ERRORE e stampa il tuo warning personalizzato
        country_iso <- stringr::str_sub(code, -3)
        country_name <- tryCatch(
          countrycode::countrycode(country_iso, "iso3c", "country.name.en"),
          error = function(e) country_iso
        )

        countries_with_missing_data <<- c(countries_with_missing_data, country_iso)

        # Stampa il tuo warning personalizzato
        warning(paste("Data series not available:", country_name), call. = FALSE, immediate. = TRUE)
        return(NULL)
      }
    )
  })

  data_raw <- do.call(rbind, results[!sapply(results, is.null)])

  # Se data_raw è NULL, ritorna NA per value e last_yr

  if (is.null(data_raw) || nrow(data_raw) == 0) {
    return(list(value = NA, last_yr = NA))
  }

  data_raw$original_period <- as.numeric(data_raw$original_period)

  last_yr <- data_raw %>%
    dplyr::group_by(series_code) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::summarise(latest = max(original_period, na.rm = TRUE)) %>%
    dplyr::ungroup()

  last_yr <- min(last_yr$latest, na.rm = TRUE)  # last year of available data

  data_raw$countries <- stringr::str_sub(data_raw$series_code, -3) # creating a variable with iso name

  }

# Computing the variable of interest

  if (agg %in% c("row", "yes")) {

      if (agg == "row") {

        data_raw$value <- ifelse(data_raw$countries == "WLD",
                                 data_raw$value,
                                 data_raw$value * - 1)
      }

        data_raw$series_code <- ifelse(grepl("POP", data_raw$series_code, fixed = TRUE),
                                     "denom",
                                     "nom") # identifying nominator and denominator variable


      if (var == "death_rate") {

        # in case death rate is chosen, first the total number of deaths must be calculated from death rates of single countries

        data_raw <- data_raw %>%
          tidyr::pivot_wider(names_from = series_code, values_from = value) %>%
          dplyr::mutate(nom = nom / 1000 * denom) %>%
          tidyr::pivot_longer(!c(original_period, countries), names_to = "series_code", values_to = "value")

      }

        # adding Taiwan data (if present in the aggregate)

        if (is.element("TWN", country_iso)) {

          filtering_vars <- dplyr::case_when(
            stringr::str_detect(var, "gdp") ~ c("gdp", "pop"),
            stringr::str_detect(var, "gni") ~ c("gni", "pop"),
            stringr::str_detect(var, "death_rate") ~ c("deaths", "pop"),
            stringr::str_detect(var, "pop") ~ "pop"
          ) %>% unique()

          data_twn <- btransfer::twn_data[btransfer::twn_data$series_code %in% filtering_vars, ]

          data_twn$series_code <- ifelse(
            data_twn$series_code == "pop",
            "denom",
            "nom"
          )

          data_raw <- dplyr::bind_rows(data_raw, data_twn)

        }

      data_raw <- data_raw %>%
        dplyr::group_by(series_code, original_period) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = series_code, values_from = value)

      if (var %in% c("gdp", "gni")){

        data_raw$value <- data_raw$nom

      } else if (var == "pop") {

        data_raw$value <- data_raw$denom

      } else {

        data_raw$value <- data_raw$nom / data_raw$denom

      }

  }

  if (var %in% c("gni", "gni_capita")) {

    dfl_yr <- btransfer::income_classification$year[1]

    # Since gni and gni capita are in nominal terms, values must be corrected for inflation

    gdp_defl_us <- rdbnomics::rdb("WB/WDI/A-NY.GDP.DEFL.ZS-USA")[, c("original_period", "value")]

    gdp_defl_us <- gdp_defl_us %>%
      dplyr::rename(defl = value) %>%
      dplyr::mutate(original_period = as.numeric(original_period)) %>%
      dplyr::filter(!is.na(defl))

    nom_dfl <- gdp_defl_us %>%
      dplyr::filter(original_period == dfl_yr) %>%
      dplyr::pull(defl)

    data_raw <- data_raw %>%
      dplyr::ungroup() %>%
      dplyr::left_join(gdp_defl_us) %>%
      dplyr::mutate(value = value / defl * nom_dfl,
                    defl = NULL)

  }

  if(ref_yr <= last_yr) {

    value <- data_raw[data_raw$original_period == ref_yr, ]$value # value for the desired year

  } else {

    hist_period <- seq(last_yr - growth_rate_int, last_yr, by = 1)

    data_raw <- data_raw[data_raw$original_period %in% hist_period, ]

    growth_rate <- compute_growth_rate(data_raw$value, avg = TRUE) # average growth rate of the variable

    fcast_h <- ref_yr - last_yr

    value <- data_raw[data_raw$original_period == last_yr, ]$value * (1 + growth_rate)^fcast_h # forecasted value

  }

  if (avg == TRUE) {

    hist_period <- seq(last_yr - growth_rate_int, last_yr, by = 1)

    data_raw <- data_raw[data_raw$original_period %in% hist_period, ]

    avg_value <- mean(data_raw$value, na.rm = TRUE) # average value

    list(avg_value = avg_value, last_yr = last_yr)

  } else if (growth_rt == TRUE) {

    hist_period <- seq(last_yr - growth_rate_int, last_yr, by = 1)

    data_raw <- data_raw[data_raw$original_period %in% hist_period, ]

    growth_rate <- compute_growth_rate(data_raw$value, avg = TRUE) # average growth rate of the variable

    list(growth_rate = growth_rate, last_yr = last_yr, missing_countries = countries_with_missing_data)

  } else {

    list(value = value, last_yr = last_yr, missing_countries = countries_with_missing_data)

  }

}
