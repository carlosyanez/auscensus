

#' Get census data.
#' @description  This function extracts table files from each data pack (given tables and geo structure), and will collate them together into a list(),
#'  which it will return. By default it will save the processed tables in the cache folder (in parquet files), which it will use on subsquent calls.
#' @return data frame with data from file, filtered by division and election year
#' @importFrom dplyr filter if_any left_join relocate mutate all_of across distinct pull collect
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom stringr str_to_title str_c str_remove str_length
#' @importFrom zip unzip
#' @importFrom stats setNames
#' @importFrom fs path file_exists
#' @importFrom tidyr pivot_longer pivot_wider crossing
#' @importFrom arrow write_parquet read_parquet open_dataset
#' @param census_table list of tables, in the format of the output of list_census_tables()
#' @param geo_structure vector with strings of geo structures (e.g. SA1,LGA,CED)
#' @param selected_years  years to filter
#' @param ignore_cache If TRUE, it will ignore cached files
#' @param attributes If geo_structure, is SA1 or SA2, please provide a list of attributes (columns) - otherwise the function won't load the data.
#' @param collect_data if TRUE (default), will return data, if FALSE , it will return {arrow} bindings to cached files
#' @importFrom rlang .data
#' @include internal.R
#' @keywords getdata
#' @examples \dontrun{
#' data <- get_census_data(census_table = list_census_tables("04"),
#'                         geo_structure = "LGA")
#'
#' names(data)
#' }
#' @export
get_census_data <- function(census_table,
                            geo_structure,
                            selected_years=list_census_years(),
                            ignore_cache=FALSE,
                            attributes=NULL,
                            collect_data=TRUE){

  to_filter <- c("SA1","SA2")

    if((geo_structure %in% to_filter) &is.null(attributes)) stop("Please provide attribute list if geo_structure is SA1 or SA2")
  if(geo_structure %in% to_filter){
    file_suffix <- str_c(attributes,collapse = "_")
  }else{
    file_suffix <- ""
  }

  #basic data
  avail_years  <- list_census_years()
  all_years    <- list_census_years(mode="all")
  cache_dir    <- find_census_cache()
  geos         <- list_census_geo(geo_types = geo_structure)

  #get tables to explore and filter (table number, year)
  table_non_year_cols <- colnames(census_table)[!(colnames(census_table) %in% avail_years)]

  ##print(table_non_year_cols)
  census_table <- census_table |>
    pivot_longer(-all_of(table_non_year_cols), names_to = "Year",values_to = "flag") |>
    filter(if_any(c("Year"), ~ .x %in% selected_years))

  #print(1)
  table_stubs  <- get_auscensus_metadata("tables.zip")
  #print(2)
  stubs_non_year_cols <- colnames(table_stubs)[!(colnames(table_stubs) %in% avail_years)]
  #print(stubs_non_year_cols)
  table_stubs <- table_stubs |>
    pivot_longer(-all_of(stubs_non_year_cols), names_to = "Year",values_to = "initial") |>
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
  #print(geo_non_year_cols)
  geo_list    <- geos |>
    pivot_longer(-all_of(geo_non_year_cols), names_to = "Year",values_to = "flag") |>
    distinct(.data$ASGS_Structure) |>
    pull()


  #create content stubs
  content_stubs <- crossing(tibble(geo=geo_structure),table_stubs) |>
    mutate(identifier =str_c(.data$Year,"_",.data$geo,"_",.data$element),
           cached_file=path(cache_dir,str_c(.data$identifier,"_",file_suffix,".parquet")),
           cache_exists=file_exists(.data$cached_file))


  #clean up so far
  rm(list=c("geo_list","geo_non_year_cols",
            "table_non_year_cols","census_table","table_stubs"))


  data <-list()
  #try to load files from cache, check if they are still current
  for(i in 1:nrow(content_stubs)){
    if(content_stubs[i,]$cache_exists&!ignore_cache){
        data_i <- open_dataset(content_stubs[i,]$cached_file,
                               format="parquet")
        data_index <- length(data) +1

        if(collect_data){
          data[[data_index]] <- data_i |> collect()
        }else{
          data[[data_index]] <- data_i
        }
        names(data)[[data_index]] <- content_stubs[i,]$identifier

    }else{
      if(!exists("content_data")){
        content_data <- get_auscensus_metadata("content.zip") |>
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

    content_i <- content_data |>
      select(-any_of(c("flag"))) |>
      left_join(content_stubs[i,], by=c("geo","Year","element")) |>
      filter(if_any("flag", ~ .x==TRUE))

    geo_decode_i <- geo_decode |>
      filter(if_any(c("ASGS_Structure"),~ .x==content_stubs[i,]$geo)) |>
      filter(if_any(c("Year"), ~ .x==content_stubs[i,]$Year)) |>
      select(-any_of(c("ASGS_Structure","Year")))

    data_i <- NULL

    content_sub_elements <- unique(content_i$sub_element)

    attributes_short <- descriptors |>
                        filter(if_any("Long") %in% attributes)
    attributes_short <- attributes_short$Short


    if(nrow(content_i)>0){
    #rm(data_j,data_k,j,k)
    for(j in 1:length(content_sub_elements)){

      content_j <- content_i |> filter(if_any(c("sub_element"),~.x==content_sub_elements[j]))
      data_j <- NULL
      if(nrow(content_j)>0){

        zip_file <- path(cache_dir,content_j$zip) |> unique()
        filename <- content_j$filename
        temp_file <- path(cache_dir,str_extract(filename,"[^/\\\\&\\?]+\\.\\w{3,4}(?=([\\?&].*$|$))"))
        unzip(zipfile = zip_file,files=filename,junkpaths = TRUE,exdir = cache_dir)

        for(k in 1:nrow(content_j)){

        data_k <- open_dataset(
          sources = temp_file[k],
          format = "csv")

        key_col <- data_k$schema$names[1]

        if(geo_structure %in% to_filter){
          data_k_colnames <- attributes_short
          data_k <- data_k |> select(any_of(c(key_col,data_k_colnames)))
        }else{
        data_k_colnames <- data_k$schema$names
        data_k_colnames <- data_k_colnames[data_k_colnames!=key_col]
        }

        data_k_col <- tibble("Short"=data_k_colnames) |>
          left_join(descriptors |> select(any_of(c("Short","Long"))),
                    by="Short") |>
          mutate(across(c("Long"), ~str_to_title(.x)))

         if(is.null(data_j)){
            data_j <- data_k
            data_j_colnames <- data_k_colnames
          }else{
            data_j <- left_join(data_j,data_k,by=key_col)
            data_j_colnames <- unique(c(data_j_colnames,data_k_colnames))
              }
      }

        descriptors_j <- descriptors |>
                        filter(if_any(c("Short"), ~ .x %in% data_j_colnames)) |>
                        select(any_of(c("Short","Long"))) |>
                        bind_rows(tibble("Short"=key_col,"Long"="Census_Code"))

        naming_key <- setNames(object = descriptors_j$Short, nm = descriptors_j$Long)
        data_j <- data_j |>
                  rename(any_of(naming_key)) |>
                  mutate(across(c("Census_Code"),~as.character(.x)))

        data_j <- data_j |>
          left_join(geo_decode_i,by="Census_Code") |>
          mutate(Year =content_j[1,]$Year )


        parquet_file <- content_j[1,]$cached_file

        write_parquet(data_j,parquet_file)
        #file_delete(temp_file)

        data_index <- length(data) +1

        data_j <- open_dataset(parquet_file,
                               format="parquet"
                               )

        if(collect){
          data[[data_index]] <- data_j |> collect()
        }else{
          data[[data_index]] <- data_j
        }
        names(data)[data_index] <- content_j[1,]$identifier


      }
      }

    }




    }
    }


  return(data)
}


#' Get a particular data point across census
#' @description Extracts a particular data point statistics from census data packs, filterable by particular geographical units.
#' Provides and option to express values as percentage of another data point.
#' @return data frame with data from file, filtered by division and election year
#' @importFrom dplyr select any_of starts_with filter if_any rename left_join mutate bind_rows if_else collect
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom methods is
#' @param table_number number of selected table
#' @param geo_structure vector with strings of geo structures (e.g. SA1,LGA,CED)
#' @param attributes attribute
#' @param geo_unit_names geo unit names
#' @param geo_unit_codes geo unit codes
#' @param selected_years  years to filter
#' @param reference_total reference total
#' @param percentage_scale 1 if percentage to be presented in scale 0-1, or 100 to be shown as 0%-100%
#' @param ignore_cache If TRUE, it will ignore cached files
#' @param data_source result of get_census_data (will ignore other parameters if this is provided)
#' @param data_collected TRUE if data_source is a dataset, FALSE if is  a DB,arrow binding
#' @param census_table output of list_census_table()
#' @importFrom rlang .data
#' @include internal.R
#' @keywords getdata
#' @examples \dontrun{
#' get_census_summary(table_number = "04",
#'     attributes = list("60 year old male"=c("Age_years_60_males","Age (Years): 60_males"),
#'                       "60 year old female"=c("Age_years_60_males","Age (Years): 60_females")),
#'     geo_unit_names = c("Melbourne","Stonnington","Yarra"),
#'     reference_total = list("Total"=c("Total_persons")),
#      geo_structure = "LGA")
#'
#' }
#' @export
get_census_summary <- function(table_number=NULL,
                               geo_structure=NULL,
                               attributes,
                               geo_unit_names=NULL,
                               geo_unit_codes=NULL,
                               selected_years=list_census_years(),
                               reference_total=NULL,
                               percentage_scale = 1,
                               ignore_cache=FALSE,
                               data_source=NULL,
                               data_collected=FALSE,
                               census_table=NULL){


  data <- tibble()

  if(is(attributes,"list")){
    attributes_filter <- NULL
    for(i in 1:length(attributes)){
    attributes_filter <- c(attributes_filter,attributes[[i]])
    }
  }else{
    attributes_filter <- attributes
  }
  reference_total_filter <- NULL
  if(is(reference_total,"list")){
    reference_total_filter <- NULL
    for(i in 1:length(reference_total)){
      reference_total_filter <- c(reference_total_filter,reference_total[[i]])
    }
  }else{
    reference_total_filter <- NULL
    reference_total        <- NULL
  }

  if(is.null(data_source)){

    if(is.null(census_table)){
      census_table <- list_census_tables(number=table_number)
    }

    attr <- unique(c(attributes_filter,reference_total_filter))

    data_source <- get_census_data(census_table,
                                   geo_structure,
                                   selected_years,
                                   ignore_cache,
                                   attributes = attr,
                                   collect_data =  FALSE)
    data_collected <- FALSE
  }

  if(length(data_source)==0)  stop("no data found")
  for(i in 1:length(data_source)){
  if(!is.null(data_source[[i]])){
    data_i <- data_source[[i]] |>
                select(any_of(c("Census_Code","Unit","Year",attr)))

    if(!is.null(geo_unit_names)){
      data_i <- data_i |>
        filter(if_any(c("Unit"), ~ .x %in% geo_unit_names))
    }

    if(!is.null(geo_unit_codes)){
      data_i <- data_i |>
        filter(if_any(c("Census_Code"), ~ .x %in% geo_unit_codes))
    }

    if(!data_collected){
      data_i <- data_i |> collect()
      }

    data_i <- data_i |>
      pivot_longer(-any_of(c("Year","Unit","Census_Code")),names_to="Attribute",values_to="Value")  |>
      filter(if_any(c("Value"), ~ !is.na(.x)))


    if(is(attributes,"list")){
      for(i in 1:length(attributes)){

        data_i <- data_i |>
          mutate(across(c("Attribute"), ~ if_else(.x %in% attributes[[i]], names(attributes)[i],.x)))

      }
    }


    if(!is.null(reference_total)){
      if(!(percentage_scale %in% c(1,100))) stop("percentage scale must by 1 or 100 (as in 100%)")

      total <-  data_i |>
        filter(if_any(c("Attribute"), ~ .x %in% reference_total_filter))  |>
        select(-any_of("Attribute")) |>
        rename("Total"="Value")

      data_i <- data_i |>
        filter(if_any(c("Attribute"), ~ .x %in% names(attributes))) |>
        left_join(total,by=c("Year","Unit","Census_Code")) |>
        mutate(Percentage=percentage_scale*.data$Value/.data$Total)


      colnames(data_i)[which(colnames(data_i)=="Total")] <- names(reference_total)[1]


    }
    data <- bind_rows(data,data_i)
  }
  }

  return(data)
}

