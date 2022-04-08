#' Computing transfer factos for unit value trasnfer with income adjustment
#'
#' \code{bt_transfer} returns a data frame with all parameters used to calculate
#' transfer factors which can be in turn used to perform value transfers in
#' environmental and health economics.
#'
#' This function allows the calculation of transfer factors to be used to perform
#' unit value transfers with income adjustments. Such factors are scalars that must be
#' multiplied by the original value. The function allows the user to select the
#' study site (country in which the original study that identified the value to be
#' transferred was carried out) and the policy sites. The latter can be a group of
#' countries or an aggregate of countries. Study and policy years can also be selected,
#' as well as whether the original estimate is expressed in US dollars or euro (default).
#' Finally, the function considers different values for income elasticity of the
#' willingness to pay for the environmental/health good to be transferred (epsilon)
#' according to the policy country's GNI per capita. More information on value
#' transfer techniques can be found in Navrud and Ready (2007).
#' The function works also in case policy year is set to a future year. Parameters like
#' GDP per capita and GNI per capita (Needed to identify the appropriate epsilon)
#' are calculated, for the policy year in the future, using average growth rates
#' of the last n-year period. In this case, n (the lenght of the time window over
#' which the growth averages are calculated) is equal to the time horizon defined
#' by the policy year (i.e. if the policy year is 2030 and the last year with
#' available data is 2020, the horizon is set to 10 years).
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
#' @param policy_yr Numeric. The ywar to which values must be transferred. Default is 2019. It
#' is possible to define also future years up to 2050.
#' @param aggregate A single string. It can assume three different values. If \code{no} transfer
#' factors are provided for each country selected via \code{policy_site}. If \code{yes} a transfer
#' factor is calculated for the aggregate made up by the countries selected via \code{policy_site}.
#' If \code{row} a transfer factor is calculated for the aggregate "rest of the world" (world
#' minus countries selected via \code{policy_site}).
#' @param currency A single string. It can assume three values: \code{EUR} (the default),
#' \code{USD} or \code{LCU}. It refers to the currency in which the original values is expressed.
#' In case \code{LCU} is chosen, the GDP deflator and the currency considered are those of the study site.
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

  # defining whether the value transfer is to be performed for a year in the future or not

  latest_av_yr <- max(wb_series$year) # latest year for which actual data are avilable

  # forecasting indicator values in case policy year is in the future

  if (policy_yr > latest_av_yr) {

    # computing average growth rates by country (average windows varies)

    h <- policy_yr - latest_av_yr

    growth_rates <- subset(wb_growth, year > latest_av_yr - h)

  }

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


    gdp_defl_fct <- subset(gdp_defl_eurostat, eu_code == "EA" &
                             year == ifelse(policy_yr > latest_av_yr,
                                            latest_av_yr, policy_yr))$gdp_defl / gdp_defl_study

  } else if (currency == "USD") {

    gdp_defl_study <- subset(wb_series, iso3c == "USA" & year == study_yr)$gdp_defl

    gdp_defl_fct <- subset(wb_series, iso3c == "USA" &
                             year == ifelse(policy_yr > latest_av_yr,
                                            latest_av_yr, policy_yr))$gdp_defl / gdp_defl_study

    # USD-EUR policy year exchange rate. USD per EUR (values in dollars ,ust be multiplied for this factor)

    us_euro <- subset(wb_series, iso3c == "EMU" &
                        year == ifelse(policy_yr > latest_av_yr,
                                       latest_av_yr, policy_yr))$exc_rate

  } else if (currency == "LCU") { # in case the currency of the primary estimate is in LCU

    gdp_defl_study <- subset(wb_series, iso3c == iso_study & year == study_yr)$gdp_defl

    gdp_defl_fct <- subset(wb_series, iso3c == iso_study &
                             year == ifelse(policy_yr > latest_av_yr,
                                            latest_av_yr, policy_yr))$gdp_defl / gdp_defl_study

    # converting LCU in USD and finally in EUR

    lcu_us <- subset(wb_series, iso3c == iso_study &
                       year == ifelse(policy_yr > latest_av_yr,
                                      latest_av_yr, policy_yr))$exc_rate

    us_euro <- subset(wb_series, iso3c == "EMU" &
                        year == ifelse(policy_yr > latest_av_yr,
                                       latest_av_yr, policy_yr))$exc_rate

    us_euro <- lcu_us * us_euro

    }

  if (aggregate == "no") {

    if (policy_yr > latest_av_yr) {

      growth_params <- subset(wb_series, iso3c %in% iso_policy & year == latest_av_yr,
             select = c(iso3c, year, gdp_capita, gni, pop))

      growth_selected <- subset(growth_rates, iso3c %in% iso_policy)

      growth_selected_l <- split(growth_selected, growth_selected$country)

      growth_selected_l <- lapply(growth_selected_l, function (x) {

        data.frame(iso3c = unique(x$iso3c),
                   gdp_capita_growth = mean(x$gdp_capita_growth, na.rm = T),
                   gni_growth = mean(x$gni_growth, na.rm = T),
                   pop_growth = mean(x$pop_growth, na.rm = T))

      })

      growth_selected <- do.call("rbind", growth_selected_l)

      rownames(growth_selected) <- NULL

      growth_params <- merge(growth_params, growth_selected)

      # computing the GDP per capita (PPP, constant USD) in the policy year
      bt_fct <- data.frame(iso3c = growth_params$iso3c,
                               year = policy_yr,
                               gdp_capita = growth_params$gdp_capita *
                                 (1 + growth_params$gdp_capita_growth / 100)^h,
                           gni_capita = (growth_params$gni *
                             (1 + growth_params$gni_growth / 100)^h) /
                           (growth_params$pop *
                             (1 + growth_params$pop_growth / 100)^h))

      # Defining epsilon values

      bt_fct_l <- split(bt_fct, bt_fct$iso3c)

      bt_fct_l <- lapply(bt_fct_l, function (x) {

        epsilon_agg <- income_class

        epsilon_agg$gni_agg <- ifelse(x$gni_capita < income_class$max & x$gni_capita >= income_class$min,
                                      TRUE, FALSE)

        bt_fct <- x

        bt_fct$epsilon <- subset(epsilon_agg, gni_agg == TRUE)$epsilon

        bt_fct

      })

      bt_fct <- do.call("rbind", bt_fct_l)

      rownames(bt_fct) <- NULL

    } else { # if the policy year is not in the future

    gdp_capita <- subset(wb_series, iso3c %in% iso_policy & year == policy_yr,
                         select = c(iso3c, year, gdp_capita))

    # assigning epsilon according to income levels and computing transfer factor (adjusted for inflation)

    bt_fct <- merge(gdp_capita, epsilon, all.x = TRUE)

    bt_fct$epsilon <- as.numeric(bt_fct$epsilon)

    bt_fct$year <- NULL

    }


  } else if (aggregate == "yes") { # if the BT must be performed towards an aggregate

    if (policy_yr > latest_av_yr) { # if the policy year is in the future

      # computing average gdp per capita growth of the aggregate

      gdp_pop <- subset(wb_series,
                        iso3c %in% iso_policy & year > latest_av_yr - h - 1,
                        select = c(iso3c, year, gdp, gni, pop))

      gdp_pop_l <- split(gdp_pop, gdp_pop$year)

      gdp_pop_l <- lapply(gdp_pop_l, function (x) {

        data.frame(year = unique(x$year),
                   gdp_capita = sum(x$gdp, na.rm = T) / sum(x$pop, na.rm = T),
                   gni_capita = sum(x$gni, na.rm = T) / sum(x$pop, na.rm = T))

      })

      gdp_gni_capita <- do.call("rbind", gdp_pop_l)

      rownames(gdp_gni_capita) <- NULL

      # computing the average GDP growth
      gdp_growth <- mean(c(NA, gdp_gni_capita$gdp_capita[-1] /
                             gdp_gni_capita$gdp_capita[-nrow(gdp_gni_capita)] - 1),
                         na.rm = TRUE)

      gni_growth <- mean(c(NA, gdp_gni_capita$gni_capita[-1] /
                             gdp_gni_capita$gni_capita[-nrow(gdp_gni_capita)] - 1),
                         na.rm = TRUE)

      latest_gdp <- subset(gdp_gni_capita, year == latest_av_yr)$gdp_capita

      latest_gni <- subset(gdp_gni_capita, year == latest_av_yr)$gni_capita

      bt_fct <- data.frame(gdp_capita = latest_gdp * (1 + gdp_growth)^h,
                           gni_capita = latest_gni * (1 + gni_growth)^h)

      # defining epsilon

      epsilon_agg <- income_class

      epsilon_agg$gni_agg <- ifelse(bt_fct$gni_capita < income_class$max & bt_fct$gni_capita >= income_class$min,
                                    TRUE, FALSE)

      bt_fct$epsilon <- subset(epsilon_agg, gni_agg == TRUE)$epsilon

      rownames(bt_fct) <- NULL


    } else { # if policy year is not in the future (usually latest year with avilable data)

    # subsetting data

    gdp_pop <- subset(wb_series,
                      iso3c %in% iso_policy & year == policy_yr,
                      select = c(iso3c, year, gdp, gni, pop))

    bt_fct <- data.frame(gdp_capita = sum(gdp_pop$gdp) / sum(gdp_pop$pop),
               gni_capita = sum(gdp_pop$gni) / sum(gdp_pop$pop))

    # calculating transfer factors

    # identifying epsilon on the basis of the latest income classification by the WB

    epsilon_agg <- income_class

    epsilon_agg$gni_agg <- ifelse(bt_fct$gni_capita < income_class$max & bt_fct$gni_capita >= income_class$min,
                                  TRUE, FALSE)

    bt_fct$epsilon <- subset(epsilon_agg, gni_agg == TRUE)$epsilon

    rownames(bt_fct) <- NULL

    }


  } else if (aggregate == "row"){ # if the policy sit is the ROW (world minus countries provided)

    if (policy_yr > latest_av_yr) { # if the policy year is in the future

      gdp_gni_pop <-  subset(wb_series, iso3c %in% c(iso_policy, "WLD") & year > latest_av_yr - h - 1,
                         select = c(iso3c, year, gdp, gni, pop))

      gdp_gni_pop <- within(gdp_gni_pop, {

        gdp <- ifelse(iso3c != "WLD", -gdp, gdp)
        gni <- ifelse(iso3c != "WLD", -gni, gni)
        pop <- ifelse(iso3c != "WLD", -pop, pop)

      })

      gdp_gni_pop_l <- split(gdp_gni_pop, gdp_gni_pop$year)

      gdp_gni_pop_l <- lapply(gdp_gni_pop_l, function(x){

        data.frame(year = unique(x$year),
                   gdp_capita = sum(x$gdp, na.rm = T) / sum(x$pop, na.rm = T),
                   gni_capita = sum(x$gni, na.rm = T) / sum(x$pop, na.rm = T))

      })

      gdp_gni_capita <- do.call("rbind", gdp_gni_pop_l)

      rownames(gdp_gni_capita) <- NULL

      # computing the average GDP growth
      gdp_growth <- mean(c(NA, gdp_gni_capita$gdp_capita[-1] /
                             gdp_gni_capita$gdp_capita[-nrow(gdp_gni_capita)] - 1),
                         na.rm = TRUE)

      gni_growth <- mean(c(NA, gdp_gni_capita$gni_capita[-1] /
                             gdp_gni_capita$gni_capita[-nrow(gdp_gni_capita)] - 1),
                         na.rm = TRUE)

      latest_gdp <- subset(gdp_gni_capita, year == latest_av_yr)$gdp_capita

      latest_gni <- subset(gdp_gni_capita, year == latest_av_yr)$gni_capita

      bt_fct <- data.frame(gdp_capita = latest_gdp * (1 + gdp_growth)^h,
                           gni_capita = latest_gni * (1 + gni_growth)^h)

      # defining epsilon

      epsilon_agg <- income_class

      epsilon_agg$gni_agg <- ifelse(bt_fct$gni_capita < income_class$max & bt_fct$gni_capita >= income_class$min,
                                    TRUE, FALSE)

      bt_fct$epsilon <- subset(epsilon_agg, gni_agg == TRUE)$epsilon

    } else { # if the policy year is NOT in the future (usually latest year with available data)

    gdp_pop <- subset(wb_series, iso3c %in% c(iso_policy, "WLD") & year == policy_yr)

    gdp_pop <- within(gdp_pop, {

      gdp <- ifelse(iso3c == "WLD", gdp, -gdp)
      gni <- ifelse(iso3c == "WLD", gni, -gni)
      pop <- ifelse(iso3c == "WLD", pop, -pop)

    })

    gdp_gni_row <- data.frame(gdp_capita = sum(gdp_pop$gdp) / sum(gdp_pop$pop),
               gni_capita = sum(gdp_pop$gni) / sum(gdp_pop$pop))

    # identifying epsilon on the basis of GNI per capita (Atlas method)

    epsilon_row <- income_class

    epsilon_row$gni_row <- ifelse(gdp_gni_row$gni_capita < income_class$max & gdp_gni_row$gni_capita >= income_class$min,
                                   TRUE, FALSE)

    gdp_gni_row$epsilon <- subset(epsilon_row, gni_row == TRUE)$epsilon

    bt_fct <- gdp_gni_row

    }

  }

  bt_fct <- within(bt_fct, { # value transfer factors adjusted for inflation

    bt_fct <- ((gdp_capita / study_site_gdp)^epsilon) * gdp_defl_fct
    year <- policy_yr

  })

  if (currency == "EUR") {

    bt_fct

  } else if (currency == "USD") {

    bt_fct$bt_fct <- bt_fct$bt_fct * us_euro

    bt_fct

  }


}
