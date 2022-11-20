# Based on https://github.com/walkerke/auscensus/blob/master/R/helpers.R , released under MIT licence.
# Forked from {auspol}, with changes in downloading and importing files

#' Set the cache directory to store parquet files with auscensus
#'
#' @description By default, auscensus uses the rappdirs package to determine a suitable location to store shapefiles
#' on the user's computer.  However, it is possible that the user would want to store shapefiles in a custom
#' location.  This function allows users to set the cache directory, and stores the result in the user's
#' .Renviron so that auscensus will remember the location.
#'
#' Windows users: please note that you'll need to use double-backslashes or forward slashes
#' when specifying your cache directory's path in R.
#'
#' @param path The full path to the desired cache directory
#' @importFrom utils read.table write.table
#' @noRd
#' @examples \dontrun{
#' # Set the cache directory
#' cache_dir('PATH TO MY NEW CACHE DIRECTORY')
#'
#' # Check to see if it has been set correctly
#' Sys.getenv('auscensus_cache_dir')
#' }
manage_cache_dir <- function(path) {
  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")

  if(!dir.exists(path)){
    dir.create(path,recursive = TRUE)
  }


  if (!file.exists(renv)) {
    file.create(renv)
  }

  check <- readLines(renv)

  if (isTRUE(any(grepl('auscensus_cache_dir', check)))) {
    oldenv <- read.table(renv,sep="=",stringsAsFactors = FALSE)
    newenv <- oldenv[!grepl('auscensus_cache_dir', oldenv$V1), ]
    write.table(newenv, renv, quote = FALSE, sep = "=",
                col.names = FALSE, row.names = FALSE)
  }

  var <- paste0("auscensus_cache_dir=", "'", path, "'")

  write(var, renv, sep = "\n", append = TRUE)
  message(sprintf("Your new cache directory is %s. \nTo use now, restart R or run `readRenviron('~/.Renviron')`", path))

}

#' Helper function to download  data
#' @importFrom tibble tribble
#' @returns nothing
#' @export
#' @keywords helpers
census_datapacks <- function(){
  tribble(~ Census, ~ url,~type,
          2021,"https://www.abs.gov.au/census/find-census-data/datapacks/download/2021_GCP_all_for_AUS_short-header.zip","GCP",
          2016,"https://www.abs.gov.au/census/find-census-data/datapacks/download/2016_GCP_all_for_AUS_short-header.zip","GCP",
          2011,"https://www.abs.gov.au/census/find-census-data/datapacks/download/2011_BCP_all_for_AUST_short-header.zip","BCP",
          2006,"https://www.abs.gov.au/AUSSTATS/abs@archive.nsf/LookupAttach/2006CensusDataPack_BCPPublication04.11.200/$file/census06bcp.zip","BCP"
  )

}

#' Helper function to download  data
#' @param download_dir full path of the directory to donwload the files
#' @importFrom fs file_exists dir_create path
#' @importFrom utils download.file
#' @importFrom dplyr filter if_any
#' @param download_dir Full path where to download census files (required)
#' @param census_year census year to download (default to all)
#' @param download_method method to pass to download.file() ("wget" as default)
#' @returns nothing
#' @export
#' @keywords helpers
#'
data_census_download <- function(download_dir,census_year=NULL,download_method="wget"){

  dir_create(download_dir)
  censuses <- census_datapacks()

  if(!is.null(census_year))
    censuses <- censuses |>
                filter(if_any(c("Census"), ~ .x %in% census_year))

  for(i in 1:nrow(censuses)){
    source       <- censuses[i,]$url
    destination  <- path(download_dir,paste0(censuses[i,]$Census,".zip"))

    if(!file_exists(destination))
      download.file(source,destination,method=download_method)

  }
}


#' Helper function to update/download  data
#' @importFrom fs dir_info
#' @returns nothing
#' @export
#' @keywords helpers
data_census_info <- function(){
  cache_dir <- Sys.getenv('auscensus_cache_dir')
  dir_info(cache_dir)
}

#' Helper function to update/download  data
#' @importFrom fs dir_ls file_delete
#' @returns nothing
#' @param file to delete - defaults to all of them (provide full path, can obtain from data_census_info)
#' @export
#' @keywords helpers
data_census_delete <- function(file=NULL){
  if(is.null(file)){
    file <- data_census_info()$path
  }

  file_delete(file)
}

#' Helper function to update/download  data
#' @importFrom  fs file_copy
#' @returns nothing
#' @importFrom stringr str_extract
#' @param file file to import to the cache
#' @export
#' @keywords helpers
data_census_import <- function(file){
  cache_dir <- Sys.getenv('auscensus_cache_dir')

  filename <- str_extract(file,"([+-]?(?=\\.\\d|\\d)(?:\\d+)?(?:\\.?\\d*))(?:[eE]([+-]?\\d+))?[a-zA-Z]+")

  file_copy(file,path(cache_dir,filename),TRUE)

}

#' Helper function to find cache folder
#' @returns nothing
#' @export
#' @keywords helpers
find_census_cache<- function(){
  cache_dir <- Sys.getenv('auscensus_cache_dir')
  return(cache_dir)

}
