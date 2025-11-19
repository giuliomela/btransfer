#' Returns macro-economic variables for selected countries and years
#'
#' This optimized function extracts and returns macro-economic variables from the
#' World Bank database (via DbNomics). It handles single countries, aggregates,
#' and calculates the Rest of the World (ROW). It also includes forecasting
#' for future years based on historical growth rates.
#'
#' @param country_iso A string. The ISO code (3 digits) of the country(ies) of interest.
#' @param ref_yr A double. Year of interest (between 1961 and 2050).
#' @param var A string. The macro-economic variable to be extracted.
#' @param agg A string. 'no', 'yes' (aggregate), or 'row' (Rest of the World).
#' @param growth_rate_int a numeric value. Number of years to consider for growth rate calculations.
#' @param avg A logical value. If TRUE, returns the historical average value.
#' @param growth_rt A logical value. If TRUE, returns the historical average growth rate.
#' @return A list with the calculated value, the latest year of available data and missing countries.
compute_macro_var <- function (country_iso = "WLD", ref_yr = 2021, var = "gdp_capita", agg = "no",
                               growth_rate_int = 20, avg = FALSE, growth_rt = FALSE) {

  # Define variables to avoid 'no visible binding' notes
  iso3c <- year <- series_code <- original_period <- nom <- denom <- value <- countries <- latest <- defl <- nd_type <- NULL

  # --- 1. Initial Checks and Validation ---
  if(isTRUE(avg) && isTRUE(growth_rt)) stop("Parameters 'avg' and 'growth_rt' cannot be both set to TRUE.")
  if (ref_yr > 2050 || ref_yr < 1961) stop ("Please provide a valid year. Year must be between 1961 and 2050.")
  valid_vars <- c("gdp", "gni", "gdp_capita", "death_rate", "pop", "gni_capita")
  if (!is.element(var, valid_vars)) stop("Please provide a valid variable name.")
  if (length(country_iso) > 1 && agg == "no") stop ("Multiple countries can be selected only if 'agg' is set to either 'yes' or 'row'.")

  # --- 2. DBnomics Code Preparation ---
  # Map variable names to World Bank Development Indicators (WDI) codes
  wdi_codes <- dplyr::case_when(
    var == "gdp_capita" ~ c("NY.GDP.MKTP.PP.KD", "SP.POP.TOTL"),
    var == "gni_capita" ~ c("NY.GNP.ATLS.CD", "SP.POP.TOTL"),
    var == "gdp" ~ "NY.GDP.MKTP.PP.KD",
    var == "gni" ~ "NY.GNP.ATLS.CD",
    var == "pop" ~ "SP.POP.TOTL",
    var == "death_rate" ~ c("SP.DYN.CDRT.IN", "SP.POP.TOTL"),
    TRUE ~ "ERROR"
  ) |>  unique()

  countries_to_download <- country_iso

  if (agg == "row") { countries_to_download <- c(countries_to_download, "WLD") } # Add WLD for ROW calculation

  # Create full DBnomics codes
  codes <- as.vector(sapply(countries_to_download, function(x) paste0("WB/WDI/A-", wdi_codes, "-", x))) |> unique()


  # --- 3. Data Download and Preparation ---

  if ("TWN" %in% country_iso) {

    codes_with_taiwan <- codes[stringr::str_detect(codes, "TWN")]

    taiwan_data <- btransfer::twn_data

    taiwan_data <-
      taiwan_data |>
      dplyr::mutate(
        series_code = dplyr::case_when(
          .data$series_code == "gdp" ~ "WB/WDI/A-NY.GDP.MKTP.PP.KD-TWN",
          .data$series_code == "gni" ~ "WB/WDI/A-NY.GNP.ATLS.CD-TWN",
          .data$series_code == "pop" ~ "WB/WDI/A-SP.POP.TOTL-TWN",
          .data$series_code == "deaths" ~ "WB/WDI/A-SP.DYN.CDRT.IN-TWN"
        )
      ) |>
      dplyr::filter(!stringr::str_detect(.data$series_code, "capita|rate"),
                    .data$series_code %in% codes_with_taiwan) |>
      dplyr::mutate(series_code = stringr::str_remove(.data$series_code, "WB/WDI/"))

    if (length(country_iso) == 1) {

      data_raw <- taiwan_data

    } else {

      codes_no_taiwan <-
        codes[!stringr::str_detect(codes, "TWN")]

      data_raw <- download_from_dbnomics(codes_no_taiwan)

      data_raw <- dplyr::bind_rows(
        data_raw,
        taiwan_data
      )

    }

  } else {

  data_raw <- download_from_dbnomics(codes)

  }

  # Identify countries with missing data (for output)
  expected_codes <- stringr::str_remove(codes, "WB/WDI/")
  got_codes <- unique(data_raw$series_code)

  missing_codes <- setdiff(expected_codes, got_codes)

  iso_with_missing_data <- unique(sub(".*-([A-Z]{3})$", "\\1", missing_codes))

  countries_with_missing_data <-
    countrycode::countrycode(iso_with_missing_data,
                             "iso3c",
                             "country.name.en")

  countries_with_missing_data <-
    ifelse(length(countries_with_missing_data) == 0,
           "None",
           countries_with_missing_data)

  # Latest year of available data (minimum across all downloaded series)

  last_yr <- data_raw |>
    dplyr::filter(!is.na(value)) |>
    dplyr::group_by(series_code) |>
    dplyr::summarise(latest = max(original_period), .groups = "drop") |>
    dplyr::pull(latest) |>
    min(na.rm = TRUE)



  # preparing data according to the variable chosen

  if (var %in% c("gdp_capita", "gni_capita", "death_rate")) {

    # identifying denominator
    data_ag <- data_raw |>
      # 1. Identify Nom/Denom
      dplyr::mutate(
        nd_type = dplyr::case_when(
          grepl("POP", .data$series_code) ~ "denom",
          TRUE ~ "nom"
        )
      )

    # handling death rate convert rate (per 1000) to total deaths (Numerator)

    if (var == "death_rate") {

      data_deaths <- data_ag |>
        dplyr::filter(grepl("SP.DYN.CDRT.IN", .data$series_code)) |>
        dplyr::rename(rate = .data$value)

      data_pop <- data_ag |>
        dplyr::filter(grepl("SP.POP.TOTL", series_code)) |>
        dplyr::rename(pop = .data$value)

      data_ag <- data_deaths |>
        dplyr::inner_join(data_pop, by = c("original_period", "countries")) |>
        dplyr::transmute(
          .data$original_period, .data$countries,
          nom_value = .data$rate / 1000 * .data$pop, # Total Deaths (Nom.)
          denom_value = .data$pop # Total Population (Denom.)
        ) |>
        # Re-structure for aggregation
        tidyr::pivot_longer(
          cols = c(.data$nom_value, .data$denom_value),
          names_to = "nd_type",
          values_to = "value"
        ) |>
        dplyr::mutate(nd_type = sub("_value", "", .data$nd_type))

    }

    # computing variable of interest

    data_ag$series_code <- NULL # removing series code

    if (agg == "no") {

      data_ag <-
        data_ag |>
        tidyr::pivot_wider(
          names_from = .data$nd_type, values_from = .data$value
        ) |>
        dplyr::select(!.data$countries)

    } else if (agg == "yes") {

      data_ag <-
        data_ag |>
        dplyr::group_by(.data$original_period, .data$nd_type) |>
        dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_wider(
          names_from = .data$nd_type, values_from = .data$value
        )

    } else if (agg == "row") {

      data_ag <-
        data_ag |>
        dplyr::mutate(value = ifelse(.data$countries == "WLD", .data$value, -1 * .data$value)) |>
        dplyr::group_by(.data$original_period, .data$nd_type) |>
        dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_wider(
          names_from = .data$nd_type, values_from = .data$value
        ) |>
        dplyr::filter(dplyr::if_all(.cols = dplyr::everything(), .fns = ~ .x > 0)) # removes rows with negative values (usually last year, which has incomplete data)

    }

    data_raw <-
      data_ag |>
      dplyr::mutate(value = .data$nom / .data$denom, .keep = "unused")

  } else if (var %in% c("gdp", "gni", "pop")) {

    if (agg == "no") {

      data_raw <-
        data_raw |>
        dplyr::select(original_period, value)

    } else if (agg == "yes") {

      data_raw <-
        data_raw |>
        dplyr::group_by(.data$original_period) |>
        dplyr::summarise(value = sum(.data$value, na.rm = T), .groups = "drop")

    } else if (agg == "row") {

      data_raw <-
        data_raw |>
        dplyr::mutate(value = ifelse(.data$countries == "WLD", .data$value, -1 * .data$value)) |>
        dplyr::group_by(.data$original_period) |>
        dplyr::summarise(value = sum(.data$value, na.rm = T), .groups = "drop") |>
        dplyr::filter(dplyr::if_all(.cols = dplyr::everything(), .fns = ~ .x > 0)) # removes rows with negative values (usually last year, which has incomplete data)

    }

  }

  # Calculate Final Value, Average, or Growth Rate ---

  # Historical data range for calculation/forecasting
  hist_period_end <- last_yr
  hist_period_start <- hist_period_end - growth_rate_int

  # Data used for average, growth rate, or last observation
  data_hist <- data_raw |>
    dplyr::filter(original_period >= hist_period_start & original_period <= hist_period_end) |>
    dplyr::arrange(original_period)


  # --- 6.1. Calculate Average or Growth Rate (Immediate Return) ---
  if (isTRUE(avg)) {
    avg_value <- mean(data_hist$value, na.rm = TRUE)
    return(list(avg_value = avg_value, last_yr = last_yr, missing_countries = countries_with_missing_data))
  }

  if (isTRUE(growth_rt)) {
    # Assumes `compute_growth_rate` function is available in the package
    growth_rate <- compute_growth_rate(data_hist$value, avg = TRUE)
    return(list(growth_rate = growth_rate, last_yr = last_yr, missing_countries = countries_with_missing_data))
  }

  # Calculate Value

  value <- data_raw[data_raw$original_period == ref_yr, ]$value


  # if (ref_yr <= last_yr) {
  #   # Historical Value
  #   value <- data_raw |>
  #     dplyr::filter(original_period == ref_yr) |>
  #     dplyr::pull(value)
  #
  #   if (length(value) == 0) {
  #     value <- NA
  #     warning(paste("No data available for the reference year:", ref_yr))
  #   }
  #
  # } else {
  #
  #   # Forecast
  #   # Assumes `compute_growth_rate` function is available in the package
  #   growth_rate <- compute_growth_rate(data_hist$value, avg = TRUE)
  #
  #   last_value <- data_hist |>
  #     dplyr::filter(original_period == last_yr) |>
  #     dplyr::pull(value)
  #
  #   fcast_h <- ref_yr - last_yr
  #
  #   # Forecasted Value
  #   value <- last_value * (1 + growth_rate)^fcast_h
  # }

  return(list(value = value, last_yr = last_yr, missing_countries = countries_with_missing_data))

}
