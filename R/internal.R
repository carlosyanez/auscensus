#############################################################
### Internal functions ####
#############################################################



#' Obtain get data from filename
#' @return data frame with data from file, filtered by division and election year
#' @importFrom dplyr filter if_any
#' @importFrom rlang .data
#' @importFrom methods is
#' @param filename where the file is (parquet or zip containing parquet file)
#' @param filters *(optional)* list() with filters in the form list(Column="Value")
#' @noRd
#' @export
get_auscensus_metadata<- function(filename,filters=NULL){

  data <- load_auscensus(filename)


  if(is(filters,"list")){
    for(i in 1:length(filters)){

      data <- data |> filter(if_any(names(filters)[i], ~ .x %in% filters[[i]]))

    }
  }
  return(data)

}


## Based on https://github.com/walkerke/auscensus/blob/master/R/helpers.R , released under MIT licence.


#' Helper function to download  data
#'
#' @importFrom  piggyback pb_download_url
#' @importFrom  arrow read_parquet
#' @importFrom  stringr str_remove str_c str_detect
#' @importFrom utils download.file
#' @importFrom  zip unzip
#' @importFrom fs path
#' @param auscensus_file name of the file to download.
#' @param refresh Whether to re-download shapefiles if cached. Defaults to value of the global
#' option "auscensus_refresh" if that option is, and FALSE if not. This will override the behavior
#' set in "auscensus_refresh" option if a value (TRUE or FALSE) is provided.
#' @param auscensus_type Added as an attribute to return object (used internally).
#' @return dataframe
#' @noRd
load_auscensus <- function(auscensus_file,
                        refresh=getOption("auscensus_refresh", FALSE),
                        auscensus_type=NULL,force=FALSE) {




  obj <- NULL


  if (Sys.getenv("auscensus_cache_dir") != "") {
    cache_dir <- Sys.getenv("auscensus_cache_dir")
    cache_dir <- path.expand(cache_dir)
  } else {
    cache_dir <- manage_cache_dir(path("auscensus"))
  }

  file_loc <- file.path(cache_dir, auscensus_file)

  if(str_detect(file_loc,"zip")){
    file_detect <- str_c(str_remove(file_loc,"zip"),"parquet")
  }else{
    file_detect <- file_loc
  }

  if (!file.exists(file_detect)|force) {


    if(!str_detect(auscensus_file,"https")){
      url  <- pb_download_url(auscensus_file,
                              repo = "carlosyanez/auscensus",
                              tag = "aux_files")
    }else{
      url <- auscensus_file
    }

    download.file(url,file_loc)

    if(str_detect(file_loc,"zip")){
      unzip(file_loc,exdir = cache_dir)
      file.remove(file_loc)
    }

  }


  obj <- read_parquet(file_detect)
  return(obj)

}

#' Helper function to find census years with imported files (i.e. available)
#' @importFrom  stringr str_remove_all str_detect str_c
#' @importFrom dplyr filter if_any pull
#' @return string vector
#' @noRd
available_years <- function(){

  years <- data_census_info() |>
    filter(if_any(c("path"), ~str_detect(.x,"zip$"))) |>
    pull(.data$path)

  cache_dir <- find_census_cache()
  years <- str_remove_all(years,str_c(cache_dir,"/"))
  years <- str_remove_all(years,"\\.zip")

}




