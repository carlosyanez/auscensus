auscensus
================
  <!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
  [![R-CMD-check](https://github.com/carlosyanez/auscensus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/carlosyanez/auscensus/actions/workflows/R-CMD-check.yaml)
  [![lintr](https://github.com/carlosyanez/auscensus/actions/workflows/lintr.yml/badge.svg)](https://github.com/carlosyanez/auscensus/actions/workflows/lintr.yml)
  [![auscensus status badge](https://carlosyanez.r-universe.dev/badges/auscensus)](https://carlosyanez.r-universe.dev)
  <!-- badges: end -->
  
<img src="https://github.com/carlosyanez/auscensus/raw/main/img/hexSticker.png" width = "175" height = "200" align="right" />

**auscensus** provides a way read data from the Australian Bureau of Statistics Data Packs. The motivation behind the package is to be able to easily being to pull out data points for multiple geographic structures for different censuses. Currently, this package works well with ABS data packs from 2011 to 2021.


## Installation and pre-requisites

The current version of this package (0.0.1.0000) is not yet in CRAN. To  install the package, you can download it from GitHub:

```
devtools::install_github("carlosyanez/auscensus")
```
Alternatively, install from r-universe:

```
# Enable this universe
options(repos = c(
    carlosyanez = 'https://carlosyanez.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('auscensus')
```

## How to use.

This package provides a series of functions to access them from different censuses. For reference on how to check the [articles](articles/index.html).

## ATTENTION - BATTERIES NOT INCLUDED!

Please note this package is made to interact with the Census Data Packs but does not include them. To get started, please follow this [article](articles/setup.html).

## Issues? bugs? Ideas?

If you find something that is not quite right, please post an issue. If you have any ideas, or requests, or if you want to contribute, [please let me know](https://twitter.com/messages/25712933-3805104374?recipient_id=25712933&text=Hello%20world)!


## Credits

-   Data has been sourced directly from the [Australian Bureau of Statistics](https://www.abs.gov.au/). Historical results for all Commonwealth elections are published on the AEC's [Tally Room archive](https://results.aec.gov.au/).
-  Some functions use code written for the [{tigris} package](https://github.com/walkerke/tigris), maintained by [Kyle Walker](https://github.com/walkerke).
-  <a href="https://www.flaticon.com/free-icons/census" title="census icons">Census icons created by noomtah - Flaticon</a>.

## Acknowledgment of Country

The author of this package acknowledges the Boonwurrung/Bunurong and Wurrundjeri Woi Wurrung peoples of the Eastern Kulin Nation as Traditional Owners and Custodians of the land where this package has been created, and pays respect to their Elders past, present and emerging.



