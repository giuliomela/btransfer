#' Computing transfer factors for unit value transfer with income adjustment
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
#' of the last n-year period. In this case, n (the length of the time window over
#' which the growth averages are calculated) is equal to the time horizon defined
#' by the policy year (i.e. if the policy year is 2030 and the last year with
#' available data is 2020, the horizon is set to 10 years).
#'
#' @param study_site A single string. The name of the country in which the original
#' study that identified the values to be transferred was carried out. The name can be
#' provided in any language (first letter always capitalized). It is possible to choose
#' also two aggregates: the European Union (\code{EUU}) and the world as a whole (\code{WLD}).
#' The default value is \code{EUU}.
#' @param policy_site A string vector. Names of the countries to which the value must be
#' transferred. It can be either a single string or a vector. Names can be provided in
#' any language. If the policy site is the European Union or the world \code{EUU} and
#' \code{WLD} must be used respectively.
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
#' @param study_currency A single string. It can assume three values: \code{EUR} (the default),
#' \code{USD} or \code{LCU}. It refers to the currency in which the original values is expressed.
#' In case \code{LCU} is chosen, the GDP deflator and the currency considered are those of the study site.
#' In case \code{EUR} or \code{USD} are provided the Euro area and the US GDP deflators are used
#' respectively.
#' @param policy_currency A single string. It can assume three values: \code{EUR} (the default),
#' \code{USD} or \code{LCU}. It refers to the currency in which the final value must be expressed.
#' The option \code{LCU} can be chosen only if just one policy site in provided through
#' the paramter \code{policy_site}.
#' @param aggregate_name A single string. In case the value transfer must be perfomed
#' for an aggregate of countries, a name can be provided to be displayed in the
#' output.
#' @return A tibble containing all parameters used and the transfer factor (bt_fct), which
#' is the scalar which the original value must be multiplied by to perform the transfer. Such
#' factor is already adjusted for inflation.
#' @export
#'
#' @examples
#' bt_transfer(policy_site = c("Italia", "Allemagne", "France", "España", "Polska"))
#' bt_transfer(study_site = "United States", policy_site = "Italia", policy_currency = "USD")
#' bt_transfer(policy_site = c("Italia", "Allemagne", "France", "España", "Polska"),
#' aggregate = "yes")
#' bt_transfer(policy_site = c("Italia", "Allemagne", "France", "España", "Polska"),
#' aggregate = "row")
#' bt_transfer(policy_site = c("Italia", "Allemagne", "France", "España", "Polska"),
#' policy_yr = 2030, aggregate = "row")
#' bt_transfer(policy_site = c("Italia", "Allemagne"), study_currency = "USD",
#' policy_currency = "EUR")
#' bt_transfer(policy_site = "Mexico", study_currency = "USD", policy_currency = "LCU")
bt_transfer <- function (study_site = "European Union", policy_site, study_yr = 2016, policy_yr = 2019,
                         aggregate = "no", study_currency = "EUR",  policy_currency = "EUR",
                         aggregate_name = "none") {

  iso3c <- eu_code <- gdp_capita <- income_class <- gni_agg <- gni_row <- NULL

  # verifying the call is correct (error messages provided if not)

  if (length(policy_site) == 1) {

  if(policy_site == "WLD" & aggregate %in% c("yes", "row"))
    stop("If world is selected as policy site, aggregate must be set to no")

  if(policy_site == "EUU" & aggregate == "yes")
    stop("If the EU is selected as policy site, aggregate must be set to either no or row")

  }

  if (!is.element(study_currency, c("EUR", "USD", "LCU"))) stop("Please provide a valid currency unit: USD, EUR or LCU")

  if (!is.element(policy_currency, c("EUR", "USD", "LCU"))) stop("Please provide a valid currency unit: USD, EUR or LCU")

  if (length(policy_site) > 1 & policy_currency == "LCU") stop("The option policy currency can be set to LCU only if just one policy site is selected.
                                                               Please retry indicating just one policy site")

  if (aggregate %in% c("yes", "row") & policy_currency == "LCU") stop("For aggregates, the option policy currency can be set to either EUR or USD only")

  if(policy_yr > 2050) stop("Value transfer can be performed up to 2050 at most")

  # defining whether the value transfer is to be performed for a year in the future or not

  latest_av_yr <- max(btransfer::wb_series$year) # latest year for which actual data are available

  # Defining a reference year for GDP deflator and exchange rate (it is the policy year,
  #unless the policy year is in the future: then it becomes the latest year with available data)

  ref_yr <- ifelse(policy_yr > latest_av_yr,
                   latest_av_yr, policy_yr)

  # identifying iso3c codes of provided study and policy sites

  iso_study <- iso_codes(study_site)

  iso_policy <- sapply(policy_site, iso_codes)

  # Defining gdp per capita data of the study site

  study_site_gdp <- compute_macro_var(iso_study, study_yr)

  # Defining GDP deflator to be used


  if (study_currency == "USD") {

    gdp_dfl <- sapply(c(study_yr, ref_yr),
                      function (x) compute_gdp_dfl(country_iso = "USA", ref_year = x))

  } else if (study_currency == "LCU") { # in case the currency of the primary estimate is in LCU

    gdp_dfl <- sapply(c(study_yr, ref_yr),
                      function (x) compute_gdp_dfl(country_iso = iso_study, ref_year = x))

  } else if (study_currency == "EUR") {

    gdp_dfl <- sapply(c(study_yr, ref_yr),
                      function (x) compute_gdp_dfl(ref_year = x))

  }

  # calculating gdp deflator factor

  gdp_defl_fct <- gdp_dfl[2] / gdp_dfl[1]

  # defining exchange rates to be used

    cur_study <- dplyr::case_when(
      study_currency == "EUR" ~ "EMU",
      study_currency == "USD" ~ "USA",
      TRUE ~ ifelse(iso_study == "EUU", "EMU", iso_study)
    )

    if (length(iso_policy) > 1){

      cur_policy <- ifelse(policy_currency == "EUR", "EMU", "USA")

    } else {

  cur_policy <- dplyr::case_when(
    policy_currency == "EUR" ~ "EMU",
    policy_currency == "USD" ~ "USA",
    policy_currency == "LCU" ~ ifelse(iso_policy == "EUU", "EMU", iso_policy)
  )

    }

    # computing the exchange rate to be incorporated in the value transfer factor
    exc_rate_fct <- compute_exc_rate(cur_study, cur_policy, ref_yr)

    if (aggregate == "no") {

      bt_fct <- dplyr::tibble(iso3c = iso_policy,
                              year = policy_yr)

      # adding gdp ang gni per capita

      for (i in c("gdp_capita", "gni_capita")) {
        col_name <- i
        bt_fct[[col_name]] <- sapply(bt_fct$iso3c, function (x){
          compute_macro_var(x, ref_year = policy_yr, var = i)
        })
      }

  } else { # if the BT must be performed towards an aggregate

    bt_fct <- dplyr::tibble(year = policy_yr)

    # adding gdp ang gni per capita

    for (i in c("gdp_capita", "gni_capita")) {
      bt_fct[[i]] <- compute_macro_var(iso_policy, ref_year = policy_yr, var = i, agg = aggregate)
    }

  }

    # adding epsilon

    bt_fct$epsilon <- sapply(bt_fct$gni_capita, compute_epsilon)

# computing the value transfer factor, including adjustment for inflation

    bt_fct$bt_fct <- (bt_fct$gdp_capita / study_site_gdp)^bt_fct$epsilon * gdp_defl_fct

  # including country names

  if (aggregate == "no") {

    bt_fct$country <- sapply(bt_fct$iso3c, from_iso_to_name)

  } else if ( aggregate == "yes") {

    bt_fct$iso3c <- NA_character_
    bt_fct$country <- ifelse(aggregate_name == "none", "Aggregate",
                             aggregate_name)

  } else if (aggregate == "row") {

    bt_fct$iso3c <- NA_character_
    bt_fct$country <- "ROW"

  }

  # converting currency if needed

    bt_fct$bt_fct <- bt_fct$bt_fct * exc_rate_fct

  # selecting and reordering columns

    bt_fct <- bt_fct[, c("iso3c", "country", "year", "gdp_capita", "epsilon", "bt_fct")]

    dplyr::as_tibble(bt_fct)
}

