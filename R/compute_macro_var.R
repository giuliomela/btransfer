#' Returns macro-economic variables for selected countries and years
#'
#' This function extracts and returns macro-economic variables from the
#' \code{wb_series} database, which are in turn retrieved from the
#' World Bank database. The user can specify the country and the year of interest.
#' In case the selected year is in the future, the function estimates future
#' values of the selected variable based on the average growth rates of the last
#' \code{n} years. Where \code{n} is the number of years between the selected
#' reference year and the latest year with available data. IN case such difference
#' is lower than 5 years, 5 years is used a window to compute the average.
#'
#' @param country_iso A string. The ISO code (3 digits) of the country of interest.
#' Codes \code{WLD} and \code{EUU} stand for the world and the European Union
#' respectively. In case \code{agg} is set to either \code{yes} or \code{row}
#' more than one iso code can be provided.
#' @param ref_year A double. Year of interest. Can be a year into the future up
#' to 2050. Years before 1961 are not accepted neither.
#' @param var A string. The name of the macro-economic variable to be extracted.
#' It can assume the following values: \code{gdp}, \code{gdp_capita}, \code{gni},
#' \code{pop}, \code{gni_capita} and \code{death_rate}.
#' @param agg A string. It can assume three values: \code{no}, the default, \code{yes}
#' and \code{row}. Indicates whether the variable of interest must be computed for an
#' aggregate. In case \code{agg = "row"}, the ROW is built substracting the selected
#' countries from the world total
#' @return A double with the value of the selected variable for the country and year
#' of interest.
#' @seealso For more information on variables see the \code{wb_series} documentation.
compute_macro_var <- function (country_iso, ref_year, var = "gdp_capita", agg = "no") {

  iso3c <- year <- NULL

  if (ref_year > 2050 | ref_year < 1961) stop("Please provide a valid year. Year
                                              must be between 1961 and 2050")

  if (!is.element(var, c("gdp", "gni", "gdp_capita", "death_rate",
  "pop", "gni_capita"))) stop("Please provide a valid variable name")

  if (length(country_iso) > 1 & agg == "no") stop ("Multiple countries can be
                                                         selected only if 'agg' option
                                                         is set to either 'yes' or 'row'")

  last_yr <- max(btransfer::wb_series$year)

if (ref_year <= last_yr) {

  if (agg == "no") {

  subset(btransfer::wb_series, iso3c == country_iso & year == ref_year)[[var]]

  } else {

    alt_var <- ifelse(var == "death_rate",
                      "deaths",
                      stringr::str_remove(var, "_capita"))

    alt_country_iso <- c("WLD", country_iso)

    alt_db <- subset(btransfer::wb_series,
                     iso3c %in% alt_country_iso & year == ref_year)[, c("iso3c", alt_var, "pop")]

    if (agg == "row") { # making selected countries variables negative to compute row

      for (i in c(alt_var, "pop")) {

        alt_db[[i]] <- ifelse(alt_db$iso3c != "WLD", alt_db[[i]] * -1,
                              alt_db[[i]])

      }

    }

    if (var %in% c("gdp_capita", "gni_capita", "death_rate")) {

      value <- sum(alt_db[[alt_var]], na.rm = TRUE) / sum(alt_db[["pop"]], na.rm = TRUE)

    } else {

      value <- sum(alt_db[[alt_var]], na.rm = TRUE)

    }

      value

    }

} else {

  if (var == "death_rate") stop ("It is not possible to estimate future death rates")

  hor <- ifelse(ref_year - last_yr < 5, 5, ref_year - last_yr)

  if (agg == "no") {

  latest_value <- subset(btransfer::wb_series, iso3c == country_iso & year == last_yr)[[var]]

  avg_growth_rate <- compute_avg(iso = country_iso, (last_yr - hor), last_yr, var,  "growth_rt")


  } else { # computing aggregate latest values

    alt_var <- stringr::str_remove(var, "_capita")

    alt_country_iso <- c("WLD", country_iso)

    all_values <- subset(btransfer::wb_series,
                         iso3c %in% alt_country_iso &
                           year %in% (last_yr - hor):last_yr)[, c("iso3c", "year", alt_var, "pop")]

    if (agg == "row") {

    for (i in c(alt_var, "pop")) {

      all_values[[i]] <- ifelse(all_values$iso3c != "WLD", all_values[[i]] * -1,
                                all_values[[i]])

    }

    } else {

      all_values <- all_values[all_values$iso3c != "WLD", ]

    }

    agg_values <- dplyr::tibble(year = unique(all_values$year))

    for (i in c(alt_var, "pop")) { # computes aggregate values over which computing growth rates

      agg_values[[i]] <- tapply(all_values[[i]], all_values$year,
                                function(x) sum(x, na.rm = T))

    }

    if (var %in% c("gdp_capita", "gni_capita")) {

      agg_values[[var]] <- agg_values[[alt_var]] / agg_values[["pop"]]

    }

    agg_values$growth_rate <- compute_growth_rate(agg_values[[var]])

    latest_value <- agg_values[agg_values$year == last_yr, ][[var]]

    avg_growth_rate <- mean(agg_values$growth_rate, na.rm = TRUE)


  }

  fcast_value <- latest_value * (1 + avg_growth_rate)^hor

  names(fcast_value) <- NULL

  fcast_value

}

}
