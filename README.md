
<!-- README.md is generated from README.Rmd. Please edit that file -->

# btransfer

<!-- badges: start -->
<!-- badges: end -->

The goal of `btransfer` is to provide useful functions to perform unit
value transfer with income adjustment between countries or country
aggregates. It also provides a function to compute the social discount
rate for any given country or country aggregates.

## Installation

You can install the development version of `btransfer` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("giuliomela/btransfer")
```

## Examples

The main function of the package is `bt_transfer` which computes
so-called transfer factors to perform unit value transfer from a
user-defined study site to a policy site. The user can choose the place
and year in which the original study was carried out (with `study_site`
and `study_yr` options) as well as the place and year to which the
underlying value has to be transferred (`policy_site` and `policy_yr`).
The function also considers inflation and exchange rates when performing
the computation. The user can define both study and policy currencies
(`study_currency` and `policy_currency`), while the adjustment for
inflation is always carried out using the GDP deflator of the
`policy_currency`. Transfer factors can be computed also when the policy
site is an aggregate. The user can control this option with the argument
`aggregate`. The transfer factor can be computed also for a year in the
future up to 2050.

``` r
library(btransfer)

study_site <- "United Kingdom"

policy_site <- c("Italia", "France", "Deutschland", "Polska", "United States")

# Value transfer to the EU to some individual countries. No currency conversion.

bt_transfer(study_site = "Unione Europea", policy_site = policy_site, 
            study_yr = 2016, policy_yr = 2019, policy_currency = "EUR",
            study_currency = "EUR")

# The primary value is expressed in USD, but must be converted to euro

bt_transfer(study_site = "Australia", policy_site = policy_site, 
            study_yr = 2014, policy_yr = 2020, study_currency = "USD",
            policy_currency = "EUR")

# The primary value is expressed in the study country's currency and must be
# converted to the currency of the policy country

bt_transfer(study_site = "Mexico", policy_site = "India", 
            study_yr = 2014, policy_yr = 2020, study_currency = "LCU",
            policy_currency = "LCU")

# The study site is the world as a whole. Original estimate in USD, but value
# must be converted to euro.

# defining the countries that make up the aggregate
nos <- c("France", "United Kingdom", "Belgium", "Netherlands",
         "Denmark", "Norway", "Germany") # North Sea countries

bt_transfer(study_site = "World", policy_site = policy_site, 
            study_yr = 2014, policy_yr = 2020, study_currency = "USD",
            policy_currency = "EUR", aggregate = "yes",
            aggregate_name = "North Sea")

# The policy year is in the future

bt_transfer(study_site = "World", policy_site = policy_site, 
            study_yr = 2014, policy_yr = 2030, study_currency = "USD",
            policy_currency = "EUR", aggregate = "no")
```

Another useful function provided by the package is `compute_sdr` which
calculates the social discount rate for selected countries or country
aggregates using the Ramsey???s rule. This parameter is useful in economic
Cost Benefit Analysis or other economic evaluations in which monetary
values must be discounted back from the future to present in order to
perform the assessment.

``` r
# The SDR calculated for a single country

compute_sdr(countries = "Italy", policy_yr = 2019, h = 10)

# SDR calculated for the World as a whole

compute_sdr(countries = "World", policy_yr = 2019, h = 10)

# SDR calculated for an aggregate of countries

compute_sdr(countries = nos, policy_yr = 2019, h = 10,
            aggregate = "yes", aggregate_name = "North Sea")


# SDR calculated for the rest of the world, given an aggregate of countries

compute_sdr(countries = nos, policy_yr = 2019, h = 10,
            aggregate = "row")
```
