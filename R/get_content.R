

#' Obtain get data from filename
#' @return data frame with data from file, filtered by division and election year
#' @importFrom dplyr filter if_any left_join relocate mutate all_of across distinct pull
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom stringr str_to_title str_c str_remove
#' @importFrom zip unzip
#' @importFrom fs path file_delete file_exists
#' @importFrom tidyr pivot_longer pivot_wider crossing
#' @importFrom arrow write_parquet read_parquet
#' @param census_table list of tables, in the format of the output of list_census_tables()
#' @param geo_structure vector with strings of geo structures (e.g. SA1,LGA,CED)
#' @param selected_years  years to filter
#' @param cache if TRUE, will save the query in the cache (in parquet file) for later use
#' @param ignore_cache If TRUE, it will ignore cached files
#' @importFrom rlang .data
#' @include internal.R
#' @keywords getdata
#' @export
get_census_data <- function(census_table,geo_structure,selected_years=list_census_years(),cache=TRUE,ignore_cache=FALSE){

  #basic data
  avail_years <-  list_census_years()
  all_years   <-  list_census_years(mode="all")
  cache_dir    <- find_census_cache()
  geos         <- list_census_geo(geo_types = geo_structure)

  #get tables to explore and filter (table number, year)
  table_non_year_cols <- colnames(census_table)[!(colnames(census_table) %in% avail_years)]

  census_table <- census_table |>
    pivot_longer(all_of(avail_years), names_to = "Year",values_to = "flag") |>
    filter(if_any(c("Year"), ~ .x %in% selected_years))

  table_stubs  <- get_auscensus_metadata("tables.zip") |>
    pivot_longer(all_of(all_years), names_to = "Year",values_to = "initial") |>
    filter(if_any(c("Year"), ~ .x %in% selected_years)) |>
    left_join(census_table,by=c(table_non_year_cols,"Year"))  |>
    filter(if_any(c("initial"), ~ !is.na(.x)))       |>
    filter(if_any(c("flag"), ~ !is.na(.x)))          |>
    mutate(element=str_c(.data$initial,.data$Number),
           flag=TRUE,
           Year=as.numeric(.data$Year)
    ) |>
    select(any_of(c("Year","element","flag")))


  #geo types to filter
  geo_non_year_cols <- colnames(geos)[!(colnames(geos) %in% avail_years)]
  geo_list    <- geos |>
    pivot_longer(all_of(avail_years), names_to = "Year",values_to = "flag") |>
    distinct(.data$ASGS_Structure) |>
    pull()


  #create content stubs
  content_stubs <- crossing(tibble(geo=geo_structure),table_stubs) |>
    mutate(identifier =str_c(.data$Year,"_",.data$geo,"_",.data$element),
           cached_file=path(cache_dir,str_c(.data$identifier,".parquet")),
           cache_exists=file_exists(.data$cached_file))

  #clean up so far
  rm(list=c("geo_list","geo_non_year_cols",
            "table_non_year_cols","census_table","table_stubs"))


  data <-list()
  #try to load files from cache, check if they are still current
  for(i in 1:nrow(content_stubs)){
    if(content_stubs[i,]$cache_exists&!ignore_cache){

        data_i <- read_parquet(content_stubs[i,]$cached_file)
        data[[i]]  <- data_i
        names(data)[i] <- content_stubs[i,]$identifier
    }else{

      if(!exists("content")){
        content <- get_auscensus_metadata("content.zip") |>
          left_join(content_stubs |> select(-any_of(c("flag","cached_file","identifier"))),
                    by=c("Year","element","geo")) |>
          filter(if_any(c("cache_exists"), ~ .x==FALSE))
      }
      if(!exists("geo_decode")){
        geo_decode    <- get_auscensus_metadata("geo_reverse.zip") |>
          filter(if_any(c("ASGS_Structure"), ~ .x %in% geo_structure)) |>
          pivot_longer(-any_of(c("ASGS_Structure","Census_Code")), names_to="Year",values_to="Unit") |>
          filter(if_any(c("Year"), ~ .x %in% avail_years))
      }
      if(!exists("descriptors")){
        descriptors <- get_auscensus_metadata("descriptors.zip") |>
          mutate(across(c("Profiletable"), ~ str_remove(.x,"[a-zA-Z]$"))) |>
          left_join(content_stubs,
                    by=c("Year","Profiletable"="element")) |>
          filter(if_any(c("flag"),~.x==TRUE))
      }

    content_i <- content |>
      select(-any_of("flag")) |>
      left_join(content_stubs[i,], by=c("geo","Year","element")) |>
      filter(if_any("flag", ~ .x==TRUE))

    geo_decode_i <- geo_decode |>
      filter(if_any(c("ASGS_Structure"),~ .x==content_stubs[i,]$geo)) |>
      filter(if_any(c("Year"), ~ .x==content_stubs[i,]$Year)) |>
      select(-any_of(c("ASGS_Structure","Year")))

    data_i <- NULL

    for(j in 1:nrow(content_i)){
      #read file

      zip_file <- path(cache_dir,content_i[j,]$zip)
      filename <- content_i[j,]$filename
      temp_file <- path(cache_dir,str_extract(filename,"[^/\\\\&\\?]+\\.\\w{3,4}(?=([\\?&].*$|$))"))
      unzip(zipfile = zip_file,files=filename,junkpaths = TRUE,exdir = cache_dir)
      data_j       <- read_csv(temp_file,col_types = "c")
      file_delete(temp_file)

      data_j_col1 <- colnames(data_j)[1]
      data_j      <- data_j |>
        pivot_longer(-any_of(c(data_j_col1)), names_to = "Short",values_to = "Value") |>
        left_join(descriptors |> select(any_of(c("Short","Long"))),
                  by="Short") |>
        select(-any_of("Short")) |>
        mutate(Long=str_to_title(.data$Long)) |>
        distinct() |>
        pivot_wider(names_from = "Long",values_from = "Value")


      if(is.null(data_i)){
        data_i <- data_j
      }else{
        data_i_key <- colnames(data_i)[1]
        colnames(data_j)[1] <- data_i_key

        data_i <- data_i |>
          left_join(data_j,by=data_i_key)

      }
    }

    colnames(data_i)[1] <- "Census_Code"

    data_i <- data_i |>
      left_join(geo_decode_i, by="Census_Code") |>
      relocate("Unit",.before=1) |>
      mutate(Year=content_stubs[i,]$Year,.before=1)

    if(cache){
      write_parquet(data_i,path(cache_dir,content_stubs[i,]$identifier,ext = "parquet"))
    }

    data[[i]] <- data_i
    names(data)[i] <- content_stubs[i,]$identifier

    }
  }

  return(data)
}


#' Obtain get data from filename
#' @return data frame with data from file, filtered by division and election year
#' @importFrom dplyr select any_of starts_with filter if_any rename left_join mutate bind_rows
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @param table_number number of selected table
#' @param geo_structure vector with strings of geo structures (e.g. SA1,LGA,CED)
#' @param attributes attribure
#' @param geo_units geo units
#' @param selected_years  years to filter
#' @param reference_total reference total
#' @param cache if TRUE, will save the query in the cache (in parquet file) for later use
#' @param ignore_cache If TRUE, it will ignore cached files
#' @importFrom rlang .data
#' @include internal.R
#' @keywords getdata
#' @export
get_census_summary <- function(table_number,
                               geo_structure,
                               attributes,
                               geo_units=NULL,
                               selected_years=list_census_years(),
                               reference_total=NULL,
                               cache=TRUE,
                               ignore_cache=FALSE){


  data <- tibble()

  census_table <- list_census_tables(number=table_number)

  data_source <- get_census_data(census_table,geo_structure,selected_years,cache,ignore_cache)

  for(i in 1:length(data_source)){

    data_i <-data_source[[i]] |>
      select(-any_of(starts_with("Census")))

    if(!is.null(geo_units)){
      data_i <- data_i |>
        filter(if_any(c("Unit"), ~ .x %in% geo_units))
    }

    data_i <- data_i |>
      pivot_longer(-any_of(c("Year","Unit")),names_to="Attribute",values_to="Value")

    if(!is.null(attributes)){
      data_i <- data_i |>
        filter(if_any(c("Attribute"),~ .x %in% attributes ))

    }

    if(!is.null(reference_total)){
      total <-  data_i |>
        filter(if_any(c("Attribute"), ~ .x==reference_total))  |>
        select(-any_of("Attribute")) |>
        rename("Total"="Value")

      data_i <- data_i |>
        filter(if_any(c("Attribute"), ~ .x %in% attributes)) |>
        left_join(total,by=c("Year","Unit")) |>
        mutate(Percentage=100*.data$Value/.data$Total)

      colnames(data_i)[which(colnames(data_i)=="Total")] <- reference_total


    }
    data <- bind_rows(data,data_i)
  }

  return(data)
}
