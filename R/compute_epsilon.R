#' Computes the income elasticity of the willingness to pay
#'
#' This function computes the income elasticity of the willingness to pay for
#' environmental/health goods (epsilon) based on GNI per capita and the latest
#' version of the World Bank's income classification of countries.
#'
#' @param gni_capita A double. The gross national income per capita of the
#' country for which epsilon must be estimated. Expressed in constant USD, at the
#' prices of the year in which the WB performed the income classification. See
#' \url{https://blogs.worldbank.org/opendata/new-world-bank-country-classifications-income-level-2021-2022}
#' @return A double. The income elasticity of the willingness to pay
#' @seealso \code{\link{income_class}}
#'
#' @examples compute_epsilon(30000)
compute_epsilon <- function (gni_capita) {

  epsilon_db <- btransfer::income_class

  epsilon_db$true_false <- ifelse(gni_capita < epsilon_db$max &
                                    gni_capita >= epsilon_db$min,
         TRUE, FALSE)

  epsilon_db[epsilon_db$true_false == TRUE, ]$epsilon

}
