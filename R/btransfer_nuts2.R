#' Computing transfer factors for unit value transfer with income adjustment at NUTS2 level
#'
#' This function is a simplified version of `btransfer` which computes transfer factors
#' between any European country belonging to the EU (or the Eu itself) and any of the Italian regions.
#' In this simplified version it is not possible to transfer values to the future.
#' Epsilon, that is the income elasticity of the willingness to pay for
#' environmental/health goods, can be defined by the user.
#'
#' @param study_site A string. The name (in any language) of the EU country from which the value has to be
#'     transferred. the European Union as a whole can spe specified in English and Italian only. Only
#'     one `study_site` can be specified at once.
#' @param policy_site A character vector. A vector of Italian NUTS2 names to perform the transfer to. Names must
#'     be in Italian. See the full list checking the documentation of `nuts2_codes`.
#' @param study_yr A numeric value. The study year.
#' @param policy_yr A numeric value. The policy year.
#' @param ref_yr A numeric value. The year at which prices must be expressed.
#' @param epsilon A numeric value. The income elasticity of the willingness to pay for
#'     environmental/health goods. Default value is `0.2`.
#' @param simplify A logical value. If set to `TRUE` the function returns the transfer factor only. If set to
#'     `FALSE` it returns a tibble with information on the selected NUTS.
#' @return Either a single value (transfer factor) or A tibble containing transfer factors for the selected NUTS2 regions.
#' @export
#'
#' @examples
#'
#' btransfer_nuts2(policy_site = "Umbria", policy_yr = 2019, ref_yr = 2019)
btransfer_nuts2 <- function (study_site = "Italia", policy_site, study_yr = 2016, policy_yr,
                                     ref_yr, epsilon = 0.2, simplify = TRUE) {

  original_period <- value <- defl_fct <- NULL

  if (study_site %in% c("Unione Europea", "European Union")) {

    study_code <- "EU27_2020"

  } else {

    study_code <- countrycode::countryname(study_site, "iso2c")

  }

  if (!any(is.element(policy_site, btransfer::nuts2_codes$label)))
    stop("Please provide a valid 'study_site' name. See 'nuts2codes' documentation for the full list")

  policy_code <- btransfer::nuts2_codes[btransfer::nuts2_codes$label %in% policy_site, ]$code

  # downloading GDP per capita (current values) and GDP deflator (Italy). Pointless to use GDP per capita expressed
  # PPS since PPS are not available at regional level.

  gdp_codes <- sapply(c(study_code, policy_code), function(x) {

    paste0("Eurostat/nama_10r_2gdp/A.EUR_HAB.", x)


  })

  gdp_codes <- unique(gdp_codes) # removes duplicate codes in case study_site = policy_site

  gdp_data <- rdbnomics::rdb(ids = gdp_codes)[, c("geo", "original_period", "value")]

  study_gdp <- gdp_data[gdp_data$original_period %in% study_yr &
                          gdp_data$geo == study_code, ]

  study_gdp <- study_gdp %>%
    dplyr::rowwise() %>%
    dplyr::mutate(defl_fct = compute_gdp_dfl(base_yr = original_period, ref_yr = ref_yr),
                  value = value * defl_fct) %>%
    dplyr::pull(value) # gdp of the study country adjusted for inflation

  policy_gdp <- gdp_data[gdp_data$original_period %in% policy_yr &
                           gdp_data$geo %in% policy_code, ]

  policy_gdp <- policy_gdp %>%
    dplyr::rowwise() %>%
    dplyr::mutate(defl_fct = compute_gdp_dfl(base_yr = original_period, ref_yr = ref_yr),
                  value = value * defl_fct,
                  defl_fct = NULL) %>%
    dplyr::ungroup()

  # Computing transfer factors

  trans_fct <- policy_gdp %>%
    dplyr::mutate(trans_fct = (value / study_gdp)^epsilon,
                  value = NULL)

  if (simplify == TRUE) {

    trans_fct$trans_fct

  } else {

    trans_fct

  }



}
