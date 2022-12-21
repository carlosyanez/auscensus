

#' Get census data.
#' @description  This function extracts table files from each data pack (given tables and geo structure), and will collate them together into a list(),
#'  which it will return. By default it will save the processed tables in the cache folder (in parquet files), which it will use on subsquent calls.
#' @return data frame with data from file, filtered by division and election year
#' @importFrom dplyr filter if_any left_join relocate mutate all_of across distinct pull collect across ntile row_number
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom stringr str_to_title str_c str_remove str_length
#' @importFrom zip unzip
#' @importFrom stats setNames
#' @importFrom fs path file_exists
#' @importFrom tidyr pivot_longer pivot_wider crossing
#' @importFrom arrow read_parquet open_dataset write_dataset
#' @param census_table list of tables, in the format of the output of list_census_tables()
#' @param geo_structure vector with strings of geo structures (e.g. SA1,LGA,CED)
#' @param selected_years  years to filter
#' @param ignore_cache If TRUE, it will ignore cached files
#' @param collect_data if TRUE  will return data. if FALSE (default) , it will return {arrow} bindings to cached files
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
                            #attribute=NULL,
                            collect_data=FALSE){

  tryCatch(remove_census_cache_csv(),
           error=function(e){cat("ERROR :",conditionMessage(e), "\n")})


  if(str_detect(geo_structure,"_")){
    geo_struct <- str_extract(geo_structure,"(.+?)(?=_)")
  }else{
    geo_struct <- geo_structure
  }


  #basic data
  avail_years  <- list_census_years()
  all_years    <- list_census_years(mode="all")
  cache_dir    <- find_census_cache()
  geos         <- list_census_geo(geo_types = geo_struct)

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
           cached_file=path(cache_dir,str_c(.data$identifier)),
           cache_exists=file_exists(.data$cached_file))


  #clean up so far
  rm(list=c("geo_list","geo_non_year_cols",
            "table_non_year_cols","census_table","table_stubs"))


  data <-list()
  #try to load files from cache, check if they are still current
  for(i in 1:nrow(content_stubs)){
    if(content_stubs[i,]$cache_exists&!ignore_cache){
        data_i <- open_dataset(content_stubs[i,]$cached_file,
                               format="parquet",
                               unify_schemas=TRUE) |>
                              select(-any_of(c("split")))

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
          left_join(content_stubs |> select(-any_of(c("flag","cached_file","identifier"))) |> mutate(c_flag=TRUE),
                    by=c("Year","element","geo")) |>
          filter(if_any(c("c_flag"), ~ .x==TRUE)) |>
          select(-any_of("c_flag"))

      }
      if(!exists("geo_decode")){
        geo_decode    <- get_auscensus_metadata("geo_reverse.zip") |>
          filter(if_any(c("ASGS_Structure"), ~ .x %in% geo_struct)) |>
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
      filter(if_any(c("ASGS_Structure"),~ .x== geo_struct)) |>
      filter(if_any(c("Year"), ~ .x==content_stubs[i,]$Year)) |>
      select(-any_of(c("ASGS_Structure","Year")))

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

    n_cols <- length(data_j$schema$names)
    n_rows <- nrow(units)

    chunk_size <- 5*10^5
    split <- ceiling(n_rows*n_cols/chunk_size)
    message(str_c(n_rows," rows and ",n_cols," columns. Splitting in ",split," chunks."))
    units <- units |>
              mutate(split=ntile(x = row_number(), split)) |>
              mutate(across(any_of(c(key_col)), ~ as.character(.x)))

    iterations <- unique(units$split)

    geo_decode_u  <- geo_decode_i |> filter(if_any(any_of("Census_Code"), ~ .x %in% (units |> select(key_col) |> pull())))
    decode_key <- as.vector("Census_Code")
    names(decode_key) <- key_col
    parquet_file <- content_i[1,]$cached_file

    for(iter in iterations){

      data_u <- data_j |>
        mutate(across(c(key_col), ~ as.character(.x))) |>
        left_join(units,by=key_col) |>
        filter(if_any(any_of(c("split")),~ .x==iter)) |>
        collect() |>
        pivot_longer(-any_of(c(key_col,"split")),
                     names_to="Short",
                     values_to = "Value") |>
        filter(if_any(any_of("Value"), ~ !is.na(.x)))

      descriptors_u <- descriptors |>
                      filter(if_any(any_of("Short"), ~ .x %in% data_u$Short)) |>
                      select(any_of(c("Short","Long"))) |>
                      distinct()

      data_u <- data_u |>
        left_join(descriptors_u ,by="Short") |>
        left_join(geo_decode_u,by=decode_key) |>
        mutate(Year = content_i[1,]$Year) |>
        select(any_of(c("Year",key_col,"Unit","Long","Value","split"))) |>
        rename("Census_Code"=key_col) |>
        distinct() |>
        pivot_wider(names_from = "Long", values_from="Value")

      write_dataset(data_u,
                    parquet_file,
                    format="parquet",
                    existing_data_behavior="delete_matching",
                    partitioning="split")
    }

    tryCatch(file_delete(temp_file),
              error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

    data_index <- length(data) +1

    data_i <- open_dataset(parquet_file,
                           format="parquet",
                           unify_schemas=TRUE
                           ) |>
      select(-any_of(c("split")))

    if(collect_data){
          data[[data_index]] <- data_i |> collect()
        }else{
          data[[data_index]] <- data_i
        }
    names(data)[data_index] <- content_i[1,]$identifier


      }
  }

  return(data)
}


#' Get a particular data point across census
#' @description Extracts a particular data point statistics from census data packs, filterable by particular geographical units.
#' Provides and option to express values as percentage of another data point.
#' @return data frame with data from file, filtered by division and election year
#' @importFrom dplyr select any_of starts_with filter if_any rename left_join mutate bind_rows if_else collect group_by summarise
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom methods is
#' @param table_number number of selected table
#' @param geo_structure vector with strings of geo structures (e.g. SA1,LGA,CED)
#' @param attribute attribute
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
#'     attribute = list("60 year old male"=c("Age_years_60_males","Age (Years): 60_males"),
#'                       "60 year old female"=c("Age_years_60_males","Age (Years): 60_females")),
#'     geo_unit_names = c("Melbourne","Stonnington","Yarra"),
#'     reference_total = list("Total"=c("Total_persons")),
#      geo_structure = "LGA")
#'
#' }
#' @export
get_census_summary <- function(table_number=NULL,
                               geo_structure=NULL,
                               attribute,
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

  if(is(attribute,"list")){
    attribute_filter <- NULL
    for(i in 1:length(attribute)){
    attribute_filter <- c(attribute_filter,attribute[[i]])
    }
  }else{
    attribute_filter <- attribute
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

    attr <- unique(c(attribute_filter,reference_total_filter))

    data_source <- get_census_data(census_table,
                                   geo_structure,
                                   selected_years,
                                   ignore_cache,
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


    if(is(attribute,"list")){
      for(j in 1:length(attribute)){

        data_i <- data_i |>
          mutate(across(c("Attribute"), ~ if_else(.x %in% attribute[[j]], names(attribute)[j],.x)))

      }
    }

    # aggregate by same attribute, year, unit, census_code

    data_i <- data_i |>
      group_by(across(c("Census_Code","Unit","Year","Attribute"))) |>
      summarise(across(c("Value"), ~ sum(.x, na.rm=TRUE)),.groups="drop")


    if(!is.null(reference_total)){
      if(!(percentage_scale %in% c(1,100))) stop("percentage scale must by 1 or 100 (as in 100%)")

      total <-  data_i |>
        filter(if_any(c("Attribute"), ~ .x %in% reference_total_filter))  |>
        select(-any_of("Attribute")) |>
        rename("Total"="Value")

      data_i <- data_i |>
        filter(if_any(c("Attribute"), ~ .x %in% names(attribute))) |>
        left_join(total,by=c("Year","Unit","Census_Code")) |>
        mutate(Percentage=percentage_scale*.data$Value/.data$Total)


      colnames(data_i)[which(colnames(data_i)=="Total")] <- names(reference_total)[1]


    }
    data <- bind_rows(data,data_i)
  }
  }

  return(data)
}

