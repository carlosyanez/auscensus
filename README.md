auscensus
================
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/carlosyanez/auscensus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/carlosyanez/auscensus/actions/workflows/R-CMD-check.yaml)
  [![auscensus status badge](https://carlosyanez.r-universe.dev/badges/auscensus)](https://carlosyanez.r-universe.dev)
  <!-- badges: end -->
  
<img src="https://github.com/carlosyanez/auscensus/raw/main/img/hexSticker.png" width = "175" height = "200" align="right" />

**auscensus** provides result data for the Australian Federal Elections, from 2004 onwards, direclty from R.
Along with functions to access the data, this package also provides functions to provide summaries tables and plots.
The current version provides election data for the House of Representatives.

All the data is contained as a release ([here](https://github.com/carlosyanez/auscensus/releases/tag/data)) in [Apache Parquet format](https://arrow.apache.org/docs/r/index.html), so it can be used directly in any programming language/tool/platform that supports Parquet.


## Installation and pre-requisites

The current version of this package (0.0.1.0000) is not yet in CRAN. To  install the package, you can download it from Github:

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

This package provides a series of functions to access the from different censuses. For reference on how to check the [articles](articles/index.html).

## ATTENTION - BATTERIES NOT INCLUDED!

Please note this package is made to interact with the Census Data Packs but does not include them. Please refer to the articles to see how to import the relevant data.

## Issues? bugs? Ideas?

If you find something that is not quite right, please post an issue. If
you have any ideas,requests, or if you want to contribute, [please let me know](https://twitter.com/messages/25712933-3805104374?recipient_id=25712933&text=Hello%20world)!

## To Do

WORK IN PROGRESS
## Credits

-   Data has been sourced directly from the [Australian Bureau of Statistics](https://www.abs.gov.au/). Historical results for all Commonwealth elections are published on the AEC's [Tally Room archive](https://results.aec.gov.au/).
-  Some functions use code written for the [{tigris} package](https://github.com/walkerke/tigris), maintained by [Kyle Walker](https://github.com/walkerke).
-  <a href="https://www.flaticon.com/free-icons/census" title="census icons">Census icons created by noomtah - Flaticon</a>.

## Acknowledgment of Country

The author of this package acknowledges the Boonwurrung/Bunurong and Wurrundjeri Woi Wurrung peoples of the Eastern Kulin Nation as Traditional Owners and Custodians of the land where this package has been created, and pays respect to their Elders past, present and emerging.


