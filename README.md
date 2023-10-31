
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epiCo <img src="man/figures/logo.svg" align="right" width="120"/>

epiCo provides statistical tools for the analysis of demographic trends, spatiotemporal behavior, and outbreak characterization of vector-borne diseases in Colombia. 

<!-- badges: start -->
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/epiverse-trace/epico/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/epico/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/epico/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/epico?branch=main)
[![lifecycle-experimental](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-experimental.svg)](https://www.reconverse.org/lifecycle.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of *epiCo* from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("epiverse-trace/epiCo")
```

## Motivation

When reviewing the current epidemiological bulletins published by the local Secretariats of Health in Colombia, we identified an opportunity to create a tool to:
- Include spatial and demographic risk assessments into their routine epidemiological reports to better identify population groups for potential interventions.
- Facilitate the understanding of the different epidemiological profiles within a region in Colombia regarding the onset, duration, magnitude, and frequency of the outbreaks.
- Strengthen the transparency of the methods used for the outbreak analysis.
- Provide more informative context by performing correlation and regression analysis with local socioeconomic and climate data.

The package allows for interoperable analyses of linelist data from
[SIVIGILA](https://www.ins.gov.co/Direcciones/Vigilancia/Paginas/SIVIGILA.aspx) (accessible using Epiverse-TRACE package [sivirep](https://github.com/epiverse-trace/sivirep)) with spatial, socioeconomic, and climate data (accessible using Epiverse-TRACE package [ColOpenData](). 

*epiCo* can be used to perform the following main tasks at the municipality, departmental, or national level:

1) To identify demographic vulnerabilities from linelist data and the socioeconomic census, including risk assessment based on age, gender, occupation, and ethnicity.
2) To assess hot-spot analyses (as Local Moran's index) based on real travel distances in Colombia estimated from [Bravo-Vega C., Santos-Vega M., & Cordovez J.M. (2022)](https://doi.org/10.1371/journal.pntd.0010270).
3) To generate automated outbreak characterization (onset, duration, magnitude, and frequency) using traditional methods as the [endemic channel](https://iris.paho.org/handle/10665.2/8562) and poisson tests for unusual behavior, or by statistical process control methods as ARIMA Control Chart for outlier detection.
4) To estimate the probability of the effective reproductive number (R<sub>t</sub>) being higher than one (i.e., outbreak onset) based on work by [Parag, K.V., & Donnelly, C.A. (2022)](https://doi.org/10.1371/journal.pcbi.1010004) & [Codeço, C.T., Villela, D.A., & Coelho, F.C. (2018)](https://doi.org/10.1016/j.epidem.2018.05.011).
5) To perform correlation analyses between categorical socioeconomic data, climate time series and epidemiological data to generate a report of potential drivers and trends of VBDs outbreaks.
6) *Future features will include nowcasting assessment, underreport estimation, and short-term forecasting.*

All functionalities are performed automatically from epidemiological, demographic, spatial, and socioeconomic data published by Colombian institutions, but methods can also be customized as well as input data, so hypothetical information can be tested within the package.

## Example

These examples illustrate some of the current functionalities:

``` r
library(epiCo)

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

  - [Juan D. Umaña](https://github.com/juan-umana) (author)
  - [Juan Montenegro](https://github.com/Juanmontenegro99) (author)
  - [Julian Otero](https://github.com/jd-otero) (author)
  - [Samuel Torres](https://github.com/samueltof) (contributor)
  - [Mauricio Santos-Vega](https://github.com/mauricio110785) (author)

### Code of Conduct

Please note that the epiCo project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
