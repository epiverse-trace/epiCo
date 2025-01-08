# epiCo 1.0.1

### Break changes
-   epiCo now displays the plots in English or Spanish automatically according to the  local system configuration

### New features
-   endemic_channel plot can display less than 52 current observations
-   age_risk now accepts ages over 100 years old

### Internal changes
-   Replace RColorBrewer by base R equivalent
-   Deleted unexpected errors from tests
-   Add geometric_sd tests
-   expect_message() display an error when there are unused arguments

### Minor changes
-   Fix misleading error message in geometric_mean function

### Documentation
-   CRAN Installation instructions
-   DOI information
-   Fix color specification in Endemic Channel vignette

### Bug fixes
-   endemic_channel function now accepts the input of NULL or NA values
-   Fix bug on ‘weighted’ method in geometric_mean function
-   Fix bug on ‘positive’ method in geometric_mean function
-   Fix bug on ‘positive’ method in geometric_sd function

# epiCo 1.0.0

-   This release of epiCo package includes fixed bugs and feature requests identified after three user testings (v0.1.0 to v0.3.0), co-design meetings with local Secretariats of Health, and a first shiny app testing. This release is the most recent and stable version of the package to be submitted to CRAN and used in Epiverse-TRACE web/shiny-app developments.

# epiCo 0.3.0

-   Last user testing version
