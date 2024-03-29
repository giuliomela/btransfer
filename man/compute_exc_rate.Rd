% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_exc_rate.R
\name{compute_exc_rate}
\alias{compute_exc_rate}
\title{Compute exchange rates between two currencies in given year}
\source{
\url{https://data.worldbank.org/}
}
\usage{
compute_exc_rate(cur_from, cur_to, ref_yr)
}
\arguments{
\item{cur_from}{A string. Country code (ISO, 3 digits) to which the origin
currency belongs.}

\item{cur_to}{A string. Country code (ISO, 3 digits) to which the destination
currency belongs.}

\item{ref_yr}{A double. Year for which the exchange rate must be calculated}
}
\value{
A double. Exchange rate for the currency pair and the year selected.
}
\description{
This function computes nominal exchange rates between two custom currencies for
a given year. Exchange rate data are from the World Bank database. Euro is
denoted with the code `EMU`. For US dollar use `USA`.
}
