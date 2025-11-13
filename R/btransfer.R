#' Computing transfer factors for unit value transfer with income adjustment
#'
#' `btransfer` returns value factors to perform value transfers from a study to a policy site, in
#' environmental and health economics.
#'
#' This function allows the calculation of transfer factors to be used to perform
#' unit value transfers with income adjustment. Such factors are scalars that must be
#' multiplied by the original value. The function allows the user to select the
#' study site (country in which the original study that identified the value to be
#' transferred was carried out) and the policy sites. Study and policy years can also be selected,
#' as well as whether the original estimate is expressed in US dollars or euro (default).
#' Finally, the function considers different values for income elasticity of the
#' willingness to pay for the environmental/health good to be transferred (epsilon)
#' according to the policy country's GNI per capita. More information on value
#' transfer techniques can be found in Navrud and Ready (2007).
#' The function works also in case policy year is set to a future year. Parameters like
#' GDP per capita and GNI per capita (Needed to identify the appropriate epsilon)
#' are calculated, for the policy year in the future, using average growth rates
#' of the last n-year period, which can be defined by the user.
#'
#' @param study_site A single string. The name of the country in which the original
#'     study that identified the values to be transferred was carried out. The name can be
#'     provided in any language (first letter always capitalized). It is possible to choose
#'     also some aggregates: the European Union, the world as a whole and a few maritime aggregates present in
#'     the 2019 version of the EU's Handbook on the External Costs of Transport:
#'     - Mediterranean sea (`med`)
#'     - Black sea (`blk`)
#'     - North sea (`nor`)
#'     - Atlantic ocean (`atl`), European countries only
#'     - Baltic sea (`bal`)
#'     Single country names can
#'     be provided in English, French, Italian, Spanish, German, Dutch and Portuguese.
#'     The default value is `European Union`.
#' @param policy_site A string vector. Names of the country to which the value must be
#'     transferred. It is possible to choose amongst some aggregates: the Eu, the world and the maritime aggregates
#'     (see `study_site`. If a vector of names is provided and the `agg_policy` argument
#'     is set to `yes` or `row`, such vector is used to compute a value transfer factor to the
#'     aggregate made up by the countries specified or the Rest of the world (world total minus the
#'     countries specified). If one of the maritime aggregates is chosen, `agg_policy` must be set to `yes`.
#'     Names can be provided in English, French, Spanish, Italian, German, Dutch and Portuguese.
#' @param study_yr Numeric. The year in which the original values was estimated. Default
#'     is 2016, year about which the estimates provided by the European Commission's Handbook
#'     on the external cost of the transport sector refer to.
#' @param policy_yr Numeric. The year to which values must be transferred. Default is 2019. It
#'     is possible to define also future years up to 2050.
#' @param ref_yr Numeric. The base year to be used as a reference for price levels. For example,
#'     if `ref_yr = 2019`, the transfer factor is modified in a way to convert transferred values to the
#'     `2019` price levels.
#' @param agg_policy A character string. It can assume three different values. If `no` transfer
#'     factors are provided a single country selected via `policy_site`. If `yes` a transfer
#'     factor is calculated for the aggregate made up by the countries selected via `policy_site`.
#'     If `row` a transfer factor is calculated for the aggregate "rest of the world" (world
#'     minus countries selected via `policy_site`). In case one of the maritime aggregates is chosen as
#'     `policy_site`, this parameter must be set to `yes` to have the aggregate value, otherwise an error is returned.
#' @param study_currency A character string. It can be specified using the ISO code of the country
#'     of interest. For example, US dollar is specified using `USA`, while the GB Pound is specified
#'     with `GBR`. The euro is denoted with `EMU`. The GDP deflator used to take inflation into account
#'     (when `study_yr` and `policy_yr` are not the same) depends on the `study_currency`. If
#'     `study_currency = "USA` then the US GDP deflator is used, for example. If the ISO code is not known it
#'     can be retrieved using the [iso_codes] function.
#' @param policy_currency A single string. A character string. It can be specified using the ISO code of the country
#'     of interest. See `study_currency`.
#' @param avg_h A numeric value. Number of years to be considered to compute average growth rates for
#'     GDP and GNI per capita in case `policy_yr` is in the future.
#' @return A numeric value (scalar) representing the transfer factor (bt_fct), which
#'     is the number to which the original value must be multiplied by to perform the transfer. Such
#'     factor is already adjusted for inflation and expressed in the `policy_yr` price levels. If `policy_yr`
#'     is a year into the future, price levels corresponds to the last year for which data are available.
#' @export
#'
#' @examples
#' btransfer(policy_site = "Brazil", policy_yr = 2020, ref_yr = 2020, policy_currency = "BRA")
#' btransfer(study_site = "blk", policy_site = "med", policy_yr = 2020, ref_yr = 2020,
#' agg_policy = "yes", policy_currency = "EMU")
#' btransfer(study_site = "Italia", policy_site = c("Francia", "Germany", "Portugal"),
#' policy_yr = 2020, ref_yr = 2020, agg_policy = "row", policy_currency = "USA")
btransfer <- function(study_site = "European Union", policy_site, study_yr = 2016, policy_yr,
                      ref_yr, agg_policy = "no",
                      study_currency = "EMU", policy_currency, avg_h = 20) {

  agg_countries <- unique(btransfer::agg_composition$code)

  # identifying ISO codes of both study and policy sites

  study_iso <- iso_codes(study_site) # study site can only be a single country or world/EU/maritime aggregate

  policy_iso <- sapply(policy_site,
                       function(x) iso_codes(x))
   # policy site can be either a single country or world/EU/maritime aggregate or a custom aggregate

  # Defining whether study and policy sites are aggregates or not

  agg_study <- dplyr::case_when(
    study_site %in% agg_countries ~ "yes",
    sum(study_iso %in% c("WLD", "EUU")) == 0 ~ "no",
    TRUE ~ "no"
  )

  # Computing GDP per capita of study and policy sites in the study and policy years



    gdp_study <- compute_macro_var(study_iso,
                                 ref_yr = study_yr,
                                 agg = agg_study,
                                 var = "gdp_capita",
                                 growth_rate_int = avg_h)


    gdp_policy <- compute_macro_var(policy_iso,
                                  ref_yr = policy_yr,
                                  agg = agg_policy,
                                  var = "gdp_capita",
                                  growth_rate_int = avg_h)


  # Defining GNI per capita of the policy site to compute epsilon

  gni_policy <- compute_macro_var(policy_iso,
                                  ref_yr = policy_yr,
                                  agg = agg_policy,
                                  var = "gni_capita",
                                  growth_rate_int = avg_h)

  #countries with missing data
  all_missing_countries <- unique(c(
    gdp_study$missing_countries,
    gdp_policy$missing_countries,
    gni_policy$missing_countries
  ))

  if (length(all_missing_countries) > 0) {

    # converting ISO codes in country names
    missing_country_names <- countrycode::countrycode(all_missing_countries,
                                                      "iso3c", "country.name.en")

    # Removing NAs is translation fails
    missing_country_names <- na.omit(missing_country_names)

    warning_message <- paste0(
      "The aggregate has been calculated exluding the following countries due to missing data:\n",
      paste(missing_country_names, collapse = ", ")
    )

    warning(warning_message, call. = FALSE, immediate. = TRUE)
  }

  # Removing missing countries elements to simplify the code
  gdp_study$missing_countries <- NULL
  gdp_policy$missing_countries <- NULL
  gni_policy$missing_countries <- NULL

  #computing epsilon

  epsilon_base_yr <- btransfer::income_classification$year[1]

  gni_current_yr <- policy_yr

  dfl_gni_fct <- # US GDP deflator to convert nominal GNI into policy year price levels
    compute_gdp_dfl(
      study_currency = "USD",
      base_yr = gni_current_yr,
      ref_yr = epsilon_base_yr
    )

  # adjusting GNI per capita

  adjusted_gni_policy <-
    gni_policy[["value"]] * dfl_gni_fct

  epsilon <- compute_epsilon(adjusted_gni_policy)

  last_yr <- gdp_study[["last_yr"]]

  #return(last_yr)

  # computing GDP deflator according to study currency

  # reference year for inflation and currency adjustments

  #if (ref_yr > last_yr) stop ("Parameter 'ref_yr' cannot be a year into the future")

  dfl_fct <- compute_gdp_dfl(study_currency,
                             base_yr = study_yr,
                             ref_yr = ref_yr)

  # computing exchange rate

  exc_rate_fct <- compute_exc_rate(cur_from = study_currency,
                                   cur_to = policy_currency,
                                   ref_yr = ref_yr)

  # computing benefit transfer factor, combining together VT proper and inflation and
  # exchange rate adjustment

  vt_fct <- (gdp_policy[["value"]] / gdp_study[["value"]])^epsilon * dfl_fct * exc_rate_fct

  vt_fct

}
