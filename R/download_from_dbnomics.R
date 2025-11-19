#' Downloads data from the DBnomics database
#'
#' This function is a wrapper around the `rdbnomics::rdb()` function which downloads the data and manipulates the original data table
#' to make a tibble with only a subset of the original columns. This is convenient for further use in the `btransfer` package.
#'
#' @param codes_list A character vector of the DBnomics codes of the series to download.
#' Default is the world `WLD`.
#' @returns A tibble of three columns
#' @source \url{https://db.nomics.world/}
#'
download_from_dbnomics <- function(codes_list) {

  tryCatch(
    output <-
      rdbnomics::rdb(codes_list) |>
      tibble::as_tibble() |>
      dplyr::select(.data$original_period, .data$series_code, .data$value),
    error = function(e) {
      warning(paste("Error in download:", e$message))
      return(NULL)
    }
  )

  output <-
    output |>
    dplyr::mutate(
      original_period = as.integer(.data$original_period),
      countries = sub(".*-([A-Z]{3})$", "\\1", .data$series_code)
    )

  return(output)

}
