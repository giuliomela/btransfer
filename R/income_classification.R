#' World Bank country classification based in GNI per capita (Atlas method)
#'
#' This dataset contains the latest available World Bank's income classification based
#' on gross national income (GNI) per capita (Atlas method). This classification is useful
#' to identify the most appropriate income elasticity of the willingness to pay for
#' the environmental/health good to be transferred (epsilon). Values are defined
#' on the basis of Navrud (2009) and email exchanges with Prof Navrud himself.
#'
#' @format A data frame
#' \describe{
#' \item{income_level}{Country income classification according to the WB}
#' \item{income_level_iso3c}{Income classification code}
#' \item{min}{Lower bound of income classification (international USD)}
#' \item{max}{Upper bound of income classification (international USD)}
#' \item{epsilon}{income elasticity of the willingness to pay of the corresponding income group}
#' \item{year}{Reference year of the income classification}
#' }
#' @source \url{https://blogs.worldbank.org/opendata/new-world-bank-country-classifications-income-level-2021-2022}
"income_classification"
