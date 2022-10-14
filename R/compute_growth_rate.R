#' Compute growth rates of a vector of values
#'
#' This function computes growth rates along a vector of values.
#'
#' @param x A vector of numeric values. It can handle \code{NA_real_} values.
#' @param avg A logical value. If `TRUE` (the default), the average growth rate is
#'     compute. If `FALSE`, the whole vector of growth rates is returned.
#' @return A vector of growth rates or their mean value
#'
compute_growth_rate <- function (x, avg = TRUE) {

  output <- c(NA, x[-1] /
      x[-length(x)] - 1)

  if (isTRUE(avg)) {

    mean(output, na.rm = TRUE)

  } else {

    output

  }

}
