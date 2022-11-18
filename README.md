
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epiCo

*epiCo* provides functions for clustering, regression, now casting, and sup reporting analyses vector-borne diseases in Colombia.

**R** from common *health information systems*.

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/epiverse-trace/readepi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/readepi/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/readepi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/readepi?branch=main)
[![lifecycle-concept](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-concept.svg)](https://www.reconverse.org/lifecycle.html#concept)
<!-- badges: end -->

## Installation

You can install the development version of *epiCo* from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("epiverse-trace/epiCo")
```

## Example

These examples illustrate some of the current functionalities:

``` r
library(epiCo)
library(incidence)
library(qcc)

## Occupation labels

isco_codes <- c(7321, 2411, 4121, 3439, 3431)
isco_labels <- get_occupationLabels(isco_codes, output_level = "unit_label")

## Incidence rates estimation

data("dengue_orinoquia_2017")
incidence_object <- incidence(dengue_orinoquia_2017$FEC_NOT, groups = dengue_orinoquia_2017$COD_MUN_O)
incidenceRate_object <- estimate_incidenceRate(incidence_object, level = 2)

## Outbreaks detection

# EWMA method

incidence_arauca <- incidence_object$counts[,"81001"]
outbreaks_object <- detect_outbreaks_EWMA(incidence_arauca, lambda = 0.2, nsigmas = 2)

```

## Development

### Lifecycle

This package is currently a *concept*, as defined by the [RECON software
lifecycle](https://www.reconverse.org/lifecycle.html). This means that
essential features and mechanisms are still being developed, and the
package is not ready for use outside of the development team.

### Contributions

Contributions are welcome via [pull
requests](https://github.com/epiverse-trace/epiCo/pulls).

Contributors to the project include:

  - Juan D. Umaña (author)
  - Samuel Torres (contributor)
  - Mauricio Santos-Vega (contributor)

### Code of Conduct

Please note that the epiCo project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
