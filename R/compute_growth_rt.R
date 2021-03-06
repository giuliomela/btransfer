#' Compute growth rates of a vector of values
#'
#' This function computes growth rates along a vector of values.
#'
#' @param x A vector of numeric values. It can handle \code{NA_real_}.
#' @return A vector of growth rates
#'
compute_growth_rate <- function (x) {

  c(NA, x[-1] /
      x[-length(x)] - 1)

}
