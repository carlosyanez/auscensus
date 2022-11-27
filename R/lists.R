#' Get Census years.
#' @description Very simple function listing the Census years included in this package, for which data pack has been imported.
#' @returns vector with years
#' @importFrom  rlang .data
#' @importFrom dplyr filter if_any mutate across pull
#' @importFrom stringr str_detect str_remove_all str_c
#' @param mode Either "listed" or "available
#' @export
#' @keywords lists
#' @include internal.R
#' @examples \dontrun{
#' # Get list of all divisions
#' list_census_years()
#'  }
list_census_years <- function(mode="available"){

  data <- get_auscensus_metadata("geo_key.zip")
  data  <- colnames(data)
  data  <- data[!(data %in% c("ASGS_Structure","Census_Name"))]

  if(mode=="available"){
    cache_dir <- find_census_cache()

  #check metadata years against
    files <- data_census_info() |>
             filter(if_any(c("path"), ~str_detect(.x,"zip$"))) |>
             pull(.data$path)

    files <- str_remove_all(files,str_c(cache_dir,"/"))
    files <- str_remove_all(files,"\\.zip")

    data <- data[data %in% files]
  }


  return(data)
}

#' Get Geography types.
#' @description Very simple function listing geography types (e.g. SAx, CED, etc.), for which data pack has been imported.
#' @returns tibble, showing the geotype, available for each year
#' @importFrom  rlang .data
#' @importFrom dplyr select any_of mutate across filter distinct arrange if_any if_else
#' @importFrom tidyr pivot_longer pivot_wider
#' @export
#' @keywords lists
#' @include internal.R
#' @examples \dontrun{
#' # Get list of all divisions
#' list_census_geo_types()
#'  }
list_census_geo_types <- function(){

  #load data
  data <- get_auscensus_metadata("geo_key.zip") |>
          select(-any_of(c("Census_Name"))) |>
          pivot_longer(-c("ASGS_Structure"),names_to="year",values_to = "value") |>
          mutate(across(c("value"), ~ if_else(is.na(.x),FALSE,TRUE))) |>
          filter(if_any(c("value"), ~ .x == TRUE)) |>
          distinct()

  #find available years
  years <- list_census_years()

  #filter by year, reduce to t
  data <- data |>
          filter(if_any(c("year"), ~ .x %in% years)) |>
          pivot_wider(names_from = "year",values_from = "value") |>
          arrange(.data$ASGS_Structure)


  return(data)
}


#' Get census geographies, filterable
#' @description Get list of available geopgrahies, filterable by type and name.
#' @returns tibble, showing the geo type, available for each year
#' @importFrom  rlang .data
#' @importFrom dplyr filter if_any mutate across if_else arrange all_of
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer pivot_wider
#' @param geo_types vector containing one or more geography types (i.e. "STE","CED","SA1" ). NULL by default.
#' @param geo_names vector containing one or more geography names (i.e. "Melbourne", "Yarra","Stonnington" for LGAs). NULL by default.
#' @param geo_name_regex string with a regular expression to filter geograhpy names (i.e. for all elements starting with M : "$M")
#' @export
#' @keywords lists
#' @include internal.R
#' @examples \dontrun{
#' # Get list of all Commonwealth electoral divisions and Local Government Areas that start with "Mel"
#' list_census_geo(geo_types=c("CED","LGA"),geo_name_regex="^Mel")
#'  }
list_census_geo <- function(geo_types=NULL,
                            geo_names = NULL,
                            geo_name_regex=NULL){

  #load data
  data <- get_auscensus_metadata("geo_key.zip")

  if(!is.null(geo_types)){
    data <- data  |>
            filter(if_any(c("ASGS_Structure"), ~ .x %in% geo_types))
  }

  if(!is.null(geo_names)){
    data <- data  |>
            filter(if_any(c("Census_Name"), ~ .x %in% geo_names))
  }

  if(!is.null(geo_name_regex)){
    data <- data |>
            filter(if_any(c("Census_Name"),
                          ~ str_detect(.x,geo_name_regex)))
  }


  #find available years
  years <- list_census_years()

  #filter by year, reduce to t
  data <- data |>
    pivot_longer(-c("ASGS_Structure","Census_Name"),
                 names_to = "year",
                 values_to= "value") |>
    filter(if_any(c("year"), ~ .x %in% years)) |>
    mutate(across(c("value"), ~ if_else(is.na(.x),FALSE,TRUE))) |>
    filter(if_any(c("value"), ~ .x ==TRUE)) |>
    pivot_wider(names_from = "year",values_from = "value") |>
    arrange(.data$ASGS_Structure,.data$Census_Name)


  return(data)
}



#' Get census geographies, filterable
#' @description Get list of available geopgrahies, filterable by type and name.
#' @returns tibble, showing the geo type, available for each year
#' @importFrom  rlang .data
#' @importFrom dplyr filter if_any mutate across if_else arrange
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer pivot_wider
#' @param number vector containing one or more table numbers
#' @param table_name_regex string with a regular expression to filter table names (i.e. for all elements containing with Country : "Country")
#' @export
#' @keywords lists
#' @include internal.R
#' @examples \dontrun{
#' # Get list of all divisions
#' list_census_geo()
#'  }
list_census_tables <- function(number=NULL, table_name_regex=NULL){

  #load data
  data <- get_auscensus_metadata("tables.zip")

  if(!is.null(number)){
    data <- data  |>
      filter(if_any(c("Number"), ~ .x %in% number))
  }

  if(!is.null(table_name_regex)){
    data <- data |>
      filter(if_any(c("Table Name"),
                    ~ str_detect(.x,table_name_regex)))
  }


  #find available years
  years <- list_census_years()

  #nominal years
  nominal_years <- list_census_years(mode="all")
  non_year_cols <- colnames(data)[!(colnames(data) %in% nominal_years)]

  data <- data |>
    pivot_longer(-all_of(non_year_cols),
                 names_to = "year",
                 values_to= "value") |>
    filter(if_any(c("year"), ~ .x %in% years)) |>
    mutate(across(c("value"), ~ if_else(is.na(.x),FALSE,TRUE))) |>
    filter(if_any(c("value"), ~ .x ==TRUE)) |>
    pivot_wider(names_from = "year",values_from = "value") |>
    arrange(as.numeric(.data$Number))


  return(data)
}

#' Get names of attributes for a given census tables, across all time
#' @description Get list of available geopgrahies, filterable by type and name.
#' @returns tibble, showing the geo type, available for each year
#' @importFrom dplyr filter if_any mutate across rename distinct
#' @importFrom stringr str_detect str_remove str_to_title
#' @importFrom tidyr  pivot_wider
#' @param number vector containing one or more table numbers
#' @param table_name_regex string with a regular expression to filter table names (i.e. for all elements containing with Country : "Country")
#' @export
#' @keywords lists
#' @include internal.R
#' @examples \dontrun{
#' # Get list of all divisions
#' list_census_attributes()
#'  }
list_census_attributes <- function(number=NULL, table_name_regex=NULL){

  data <- get_auscensus_metadata("descriptors.zip")
  avail_years <- list_census_years()

 data <-  data |>
    mutate(across(c("Long"), ~ str_to_title(.x))) |>
    mutate(across(c("Profiletable"), ~ str_remove(.x,"[a-zA-Z]$"))) |>
    filter(if_any(c("Profiletable"),~str_detect(.x,number)))


 if(!is.null(number)){
   data  <- data |>
    mutate(Profiletable=number)
 }

  data <- data |>
    filter(if_any(c("Year"), ~ .x %in% avail_years)) |>
    select(c("Profiletable","Long","Year"))|>
    mutate(dummy=TRUE) |>
    distinct() |>
    pivot_wider(any_of(c("Profiletable","Long")),
                values_from = "dummy",names_from = "Year") |>
    rename("Table"="Profiletable","Attribute"="Long")


  if(!is.null(table_name_regex)){
    data <- data |>
      filter(if_any(c("Attribute"), ~ str_detect(.x,table_name_regex)))

  }

 return(data)


}



