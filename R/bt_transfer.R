#' Computing transfer factos for unit value trasnfer with income adjustment
#'
#' \code{bt_transfer} returns a data frame with all parameters used to calculate
#' transfer factors which can be in turn used to perform value transfers in
#' environmental and health economics.
#'
#' This function allows the calculation of transfer factors to be used to perform
#' unit value transfers with income adjustments. Such factors are scalars that must be
#' multiplied by the original value. The function allows the user to select the
#' study site (country in which the original stusy that identified the value to be
#' transferred was carried out) and the policy sites. The latter can be a group of
#' countries or an aggregate of countries. Study and policy years can also be selected,
#' as well as whether the original estimate is expressed in US dollars or euro (default).
#' Finally, the function considers different values for income elasticity of the
#' willingness to pay for the environmental/health good to be transferred (epsilon)
#' according to the policy country's GNI per capita. More information on value
#' transfer techniques can be found in Navrud and Ready (2007).
#'
#' @param study_site A single string. The name of the country in which the original
#' study that identified the values to be transferred was carried out. The name can be
#' provided in any language (first letter always capitalized). The default value is \code{EUU}
#' which statnds for the European Union.
#' @param policy_site A string vector. Names of the countries to which the value must be
#' transferred. It can be either a single string or a vector. Names can be provided in
#' any language.
#' @param study_yr Numeric. The year in which the original values was estimated. Default
#' is 2016, year about which the estimates provided by the European Commission's Handbook
#' on the external cost of the transport sector refer to.
#' @param policy_yr Numeric. The ywar to which values must be transferred. Default is 2019.
#' @param aggregate A single string. It can assume three different values. If \code{no} transfer
#' factors are provided for each country selected via \code{policy_site}. If \code{yes} a transfer
#' factor is calculated for the aggregate made up by the countries selected via \code{policy_site}.
#' If \code{row} a transfer factor is calculated for the aggregate "rest of the world" (world
#' minus countries selected via \code{policy_site}).
#' @param currency A single string. It can assume two values: \code{EUR} (the default) or
#' \code{USD}. It refers to the currency in which the original values is expressed.
#' @return A data frame containing all parameters used and the transfer factor (bt_fct), which
#' is the scalar which the original value must be multiplied by to perform the transfer. Such
#' factor is already adjusted for inflation.
#' @examples
#' bt_transfer(policy_site = c("Italia", "Allemagne", "France", "España", "Polska"))
#' bt_transfer(study_site = "United States",
#' policy_site = c("Italia", "Allemagne", "France", "España", "Polska"),
#' currency = "USD")
#' bt_transfer(policy_site = c("Italia", "Allemagne", "France", "España", "Polska"), aggregate = "yes")
#' bt_transfer(policy_site = c("Italia", "Allemagne", "France", "España", "Polska"), aggregate = "row")
bt_transfer <- function (study_site = "EUU", policy_site, study_yr = 2016, policy_yr = 2019,
                         aggregate = "no", currency = "EUR") {

  # identifying iso3c codes of provided study and policy sites

  if (study_site == "EUU") {

    iso_study <- study_site

    iso_policy <- countrycode::countryname(policy_site, "iso3c")

  } else {

    iso_study <- countrycode::countryname(study_site, "iso3c")

    iso_policy <- countrycode::countryname(policy_site, "iso3c")

  }

  # income levels and epsilon

  epsilon <- income_class[, c("income_level_iso3c", "epsilon")]

  epsilon <- merge(income_lvs, epsilon)[, c("iso3c", "epsilon")]

  epsilon$epsilon <- as.numeric(epsilon$epsilon)

  # adding epsilon for the EUU

  epsilon[nrow(epsilon) + 1,] = c("EUU", 0.2)

  # Defining gdp per capita data of the study site

  study_site_gdp <- subset(wb_series, iso3c == iso_study & year == study_yr)$gdp_capita

  # Defining GDP deflator to be used

  if (currency == "EUR") {

    gdp_defl_study <- subset(gdp_defl_eurostat, eu_code == "EA" & year == study_yr)$gdp_defl

    # multiplying factor to be used to account for inflation from study to policy year
    gdp_defl_fct <- subset(gdp_defl_eurostat, eu_code == "EA" & year == policy_yr)$gdp_defl / gdp_defl_study

  } else if (currency == "USD") {

    gdp_defl_study <- subset(wb_series, iso3c == "USA" & year == study_yr)$gdp_defl

    gdp_defl_fct <- subset(wb_series, iso3c == "USA" & year == policy_yr)$gdp_defl / gdp_defl_study

    # USD-EUR policy year exchange rate. USD per EUR (values in dollars ,ust be multiplied for this factor)

    us_euro <- subset(wb_series, iso3c == "EMU" & year == policy_yr)$exc_rate

    }

  if (aggregate == "no") {

    gdp_capita <- subset(wb_series, iso3c %in% iso_policy & year == policy_yr,
                         select = c(iso3c, year, gdp_capita))

    # assigning epsilon according to income levels and computing transfer factor (adjusted for inflation)

    bt_fct <- merge(gdp_capita, epsilon, all.x = TRUE)

    bt_fct$epsilon <- as.numeric(bt_fct$epsilon)

    bt_fct <- within(bt_fct, { # value transfer factors adjusted for inflation

    bt_fct <- ((gdp_capita / study_site_gdp)^epsilon) * gdp_defl_fct

    })

 if (currency == "EUR") {

   bt_fct

 } else if (currency == "USD") {

   bt_fct$bt_fct <- bt_fct$bt_fct * us_euro

   bt_fct
 }


  } else if (aggregate == "yes") {

    # downloading gdp (PPP) and population data to compute aggregate's gdp per capita.
    # donwloading also GNI (current USD) to identify appropriate epsilon

    gdp_pop <- subset(wb_series,
                      iso3c %in% iso_policy & year == policy_yr,
                      select = c(iso3c, year, gdp, gni, pop))

    bt_fct <- data.frame(gdp_capita = sum(gdp_pop$gdp) / sum(gdp_pop$pop),
               gni_capita = sum(gdp_pop$gni) / sum(gdp_pop$pop))

    # calculating trasnfer factors

    # identifying epsilon on the basis of the latest income classification by the WB

    epsilon_agg <- income_class

    epsilon_agg$gni_agg <- ifelse(bt_fct$gni_capita < income_class$max & bt_fct$gni_capita >= income_class$min,
                                  TRUE, FALSE)

    bt_fct$epsilon <- subset(epsilon_agg, gni_agg == TRUE)$epsilon

    bt_fct <- within(bt_fct, { # value transfer factors adjusted for inflation

    bt_fct <- ((gdp_capita / study_site_gdp)^epsilon) * gdp_defl_fct

    })

    rownames(bt_fct) <- NULL

    if (currency == "EUR") {

      bt_fct

    } else if (currency == "USD") {

      bt_fct$bt_fct <- bt_fct$bt_fct* us_euro

      bt_fct
    }

  }else if (aggregate == "row"){

    gdp_pop_wld <- subset(wb_series, iso3c == "WLD" & year == policy_yr)

    gdp_pop <-  subset(wb_series, iso3c %in% iso_policy & year == policy_yr)

    gdp_pop <- data.frame(gdp = sum(gdp_pop$gdp),
               gni = sum(gdp_pop$gni),
               pop = sum(gdp_pop$pop))

    gdp_pop_row <- within(gdp_pop, {

      gdp_capita <- (gdp_pop_wld$gdp - gdp) / (gdp_pop_wld$pop - pop)
      gni_capita <- (gdp_pop_wld$gni - gni) / (gdp_pop_wld$pop - pop)
      gdp <- NULL
      gni <- NULL
      pop <- NULL

    })

    # identifying epsilon on the basis of GNI per capita (Atlas method)

    epsilon_row <- income_class

    epsilon_row$gni_row <- ifelse(gdp_pop_row$gni_capita < income_class$max & gdp_pop_row$gni_capita >= income_class$min,
                                   TRUE, FALSE)

    gdp_pop_row$epsilon <- subset(epsilon_row, gni_row == TRUE)$epsilon

    bt_fct <- within(gdp_pop_row, {

      bt_fct <- (gdp_capita / study_site_gdp)^epsilon * gdp_defl_fct

    })

    if (currency == "EUR") {

      bt_fct

    } else if (currency == "USD") {

      bt_fct$bt_fct <- bt_fct$bt_fct* us_euro

      bt_fct
    }


  }


}
