### This file intends to be a shortcut to install and use {trackdown}, an R package to use Google Docs to edit the "narrative" part of a RMD file
### {trackdown} can be downloaded from CRAN
### This file is not intended to be run as one piece!


## Settings #####
gdrive_path <- "trackdown"              # the folder on google drive


# Executive Summary ####
## Commands to run #####

#upload for first time

trackdown::upload_file(here::here("precompile","setup.Rmd"), gpath="auscensus",hide_code=TRUE)
trackdown::upload_file(here::here("precompile","getting_data.Rmd"), gpath="auscensus",hide_code=TRUE)

#sync RMD back with google
trackdown::download_file(here::here("precompile","setup.Rmd"),gpath="auscensus")
trackdown::download_file(here::here("precompile","getting_data.Rmd"),gpath="auscensus")




