btransfer <- function(study_site = "European Union", policy_site, study_yr = 2016, policy_yr, agg_policy,
                      study_currency = "EMU", policy_currency) {

  agg_countries <- unique(agg_composition$code)

  # identifying ISO codes of both study and policy sites

  study_iso <- iso_codes(study_site) # study site can only be a single country or world/EU/maritime aggregate

  policy_iso <- iso_codes(policy_site) # policy site can be either a single country or world/EU/maritime aggregate or a custom aggregate

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
                                 var = "gdp_capita")

  gdp_policy <- compute_macro_var(policy_iso,
                                  ref_yr = policy_yr,
                                  agg = agg_policy,
                                  var = "gdp_capita")

  # Defining GNI per capita of the policy site to compute epsilon

  gni_policy <- compute_macro_var(policy_iso,
                                  ref_yr = policy_yr,
                                  agg = agg_policy,
                                  var = "gni_capita")


  epsilon <- compute_epsilon(gni_policy[["value"]])

  last_yr <- gdp_study[["last_yr"]]

  # computing GDP deflator according to study currency

  # reference year for inflation and currency adjustments

  ref_yr <- ifelse(policy_yr > last_yr,
                   last_yr,
                   policy_yr)

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
