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



#' Internal function to read data from files and add to cache. Written to be run inside get_census_data()
#' Inherits variables from get_census_data()
#' @return data frame with data from file, filtered by division and election year
#' @importFrom dplyr left_join select any_of mutate if_any filter across collect pull distinct rename
#' @importFrom stringr str_remove str_extract str_c str_replace_all str_squish str_remove_all fixed
#' @importFrom tidyr pivot_longer
#' @importFrom fs path
#' @importFrom zip  unzip
#' @importFrom arrow open_dataset write_dataset
#' @param content_stubs content_stubs
#' @param i i
#' @param geo_struct geo structure
#' @param attr attr
#' @param avail_years available years
#' @noRd
import_data <- function(content_stubs,i, geo_struct,attr,avail_years){

  content_i <- get_auscensus_metadata("content.zip") |>
    left_join(content_stubs |>
                select(-any_of(c("flag","cached_file","identifier"))) |>
                mutate(c_flag=TRUE),
              by=c("Year","element","geo")) |>
    filter(if_any(c("c_flag"), ~ .x==TRUE)) |>
    select(-any_of("c_flag"))               |>
    select(-any_of(c("flag"))) |>
    left_join(content_stubs[i,], by=c("geo","Year","element")) |>
    filter(if_any("flag", ~ .x==TRUE))


  if(nrow(content_i)==0){
    message("no data for year ",content_stubs[i,]$Year)
    data_i <- NULL
  }else{

    descriptors <- get_auscensus_metadata("descriptors.zip") |>
      mutate(across(c("Profiletable"), ~ str_remove(.x,"[a-zA-Z]$"))) |>
      left_join(content_stubs,
                by=c("Year","Profiletable"="element")) |>
      filter(if_any(c("flag"),~.x==TRUE))

    if(!is.null(attr)){
      attr <- str_squish(attr)
      attr <- str_remove_all(attr, "[^A-z|0-9|[:punct:]|\\s]")
      attr <- str_remove_all(attr, ":")
      attr <- str_remove_all(attr, "/")
      attr <- str_remove_all(attr, fixed("\\"))

      descriptors <- descriptors |>
        mutate(across(any_of(c("Long")), ~ str_squish(.x))) |>
        mutate(across(any_of(c("Long")), ~ str_remove_all(.x, "[^A-z|0-9|[:punct:]|\\s]"))) |>
        mutate(across(any_of(c("Long")), ~ str_remove_all(.x,":"))) |>
        mutate(across(any_of(c("Long")), ~ str_remove_all(.x,"/"))) |>
        mutate(across(any_of(c("Long")), ~ str_remove_all(.x,fixed("\\")))) |>
        mutate(across(any_of(c("Long")), ~ str_remove_all(.x,fixed('"'))))  |>
        filter(if_any(c("Long"), ~ .x %in% attr))
    }

    geo_decode <- get_auscensus_metadata("geo_reverse.zip") |>
      filter(if_any(c("ASGS_Structure"), ~ .x %in% geo_struct)) |>
      pivot_longer(-any_of(c("ASGS_Structure","Census_Code")), names_to="Year",values_to="Unit") |>
      filter(if_any(c("Year"), ~ .x %in% avail_years)) |>
      filter(if_any(c("ASGS_Structure"),~ .x== geo_struct)) |>
      filter(if_any(c("Year"), ~ .x==content_stubs[i,]$Year)) |>
      select(-any_of(c("ASGS_Structure","Year")))

    cache_dir <- find_census_cache()
    zip_file <- path(cache_dir,content_i$zip) |> unique()
    filename <- content_i$filename
    temp_file <- path(cache_dir,str_extract(filename,"[^/\\\\&\\?]+\\.\\w{3,4}(?=([\\?&].*$|$))"))

    tryCatch(unzip(zipfile = zip_file,files=filename,junkpaths = TRUE,exdir = cache_dir),
             error=function(e){cat("ERROR :",conditionMessage(e), "\n")},
             finally=str_c("zip: ",zip_file," \n file: ",filename)
    )


    data_j <- open_dataset(temp_file,
                           format="csv",
                           unify_schemas=TRUE)

    key_col <- data_j$schema$names[1]


    units <- data_j |>
      select(any_of(key_col)) |>
      collect()

    geo_decode_u  <- geo_decode |> filter(if_any(any_of(c("Census_Code")), ~ .x %in% (units |> select(key_col) |> pull())))
    decode_key <- as.vector("Census_Code")
    names(decode_key) <- key_col
    parquet_file <- content_i[1,]$cached_file

    message(str_c(content_stubs[i,]$Year,": Extracting ",nrow(descriptors), " attributes."))


    for(column in descriptors$Short){


      data_u <- data_j |>
        select(any_of(c(key_col,column))) |>
        collect() |>
        pivot_longer(-any_of(c(key_col,"split")),
                     names_to="Short",
                     values_to = "Value") |>
        filter(if_any(any_of("Value"), ~ !is.na(.x))) |>
        left_join(descriptors ,by="Short") |>
        mutate(across(c(key_col), as.character)) |>
        left_join(geo_decode_u,by=decode_key) |>
        mutate(Year = content_i[1,]$Year) |>
        select(any_of(c("Year",key_col,"Unit","Long","Attribute","Value"))) |>
        rename("Census_Code"=key_col) |>
        rename("Attribute"="Long")    |>
        distinct()                    |>
        mutate(across(c("Attribute"), ~ str_replace_all(.x,":","-")))

      write_dataset(data_u,
                    parquet_file,
                    format="parquet",
                    existing_data_behavior="delete_matching",
                    partitioning="Attribute")

    }



    tryCatch(file_delete(temp_file),
             error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

    data_i <- open_dataset(parquet_file,
                           format="parquet",
                           unify_schemas=TRUE)

  }

  return(data_i)
}


