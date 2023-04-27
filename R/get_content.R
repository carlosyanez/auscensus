

#' Get census data.
#' @description  This function extracts table files from each data pack (given tables and geo structure), and will collate them together into a list(),
#'  which it will return. By default it will save the processed tables in the cache folder (in parquet files), which it will use on subsquent calls.
#' @return data frame with data from file, filtered by division and election year
#' @importFrom dplyr filter if_any left_join relocate mutate all_of across distinct pull collect across bind_rows case_when
#' @importFrom tibble tibble
#' @importFrom stringr  str_c str_remove str_length str_replace_all str_squish str_remove_all fixed
#' @importFrom zip unzip
#' @importFrom stats setNames
#' @importFrom fs path file_exists
#' @importFrom tidyr pivot_longer pivot_wider crossing replace_na
#' @importFrom arrow read_parquet open_dataset write_dataset
#' @importFrom progressr with_progress
#' @importFrom methods is
#' @param census_table list of tables, in the format of the output of list_census_tables()
#' @param geo_structure vector with strings of geo structures (e.g. SA1,LGA,CED)
#' @param selected_years  years to filter
#' @param ignore_cache If TRUE, it will ignore cached files
#' @param collect_data if TRUE  will return data. if FALSE (default) , it will return {arrow} bindings to cached files
#' @param attr attributes to filter on, presented as a character vector (e.g c("Age_years_60_males","Age_years_60_females"))
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
                            collect_data=FALSE,
                            attr=NULL){

  tryCatch(remove_census_cache_csv(),
           error=function(e){cat("ERROR :",conditionMessage(e), "\n")})


  if(str_detect(geo_structure,"_")){
    geo_struct <- str_extract(geo_structure,"(.+?)(?=_)")
  }else{
    geo_struct <- geo_structure
  }

  if(is.null(attr)){
      tables_to_search <- unique(census_table$Number)
      attr <- tibble()
      for(i in 1:length(tables_to_search)){
        attr_n <- list_census_attributes(tables_to_search[i]) |>
                  pivot_longer(-any_of(c("Table","Attribute")),
                               names_to="Year",
                               values_to="flag")


        attr <- bind_rows(attr,attr_n) |> distinct()


      }
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
    filter(if_any(c("Year"), ~ .x %in% selected_years)) |>
    filter(if_any(c("flag"), ~ .x==TRUE))

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
           cache_exists=file_exists(.data$cached_file)) |>
    replace_na(list(cache_exists=FALSE))


  #clean up so far
  rm(list=c("geo_list","geo_non_year_cols",
            "table_non_year_cols","census_table","table_stubs"))


  data <-list()
  #try to load files from cache, check if they are still current
  for(i in 1:nrow(content_stubs)){

    if(is(attr,"tbl")){
      attr_i <- attr |>
                filter(if_any(any_of(c("Year")), ~.x %in% content_stubs[i,]$Year)) |>
                select(any_of(c("Attribute"))) |>
                pull()
    }else{
      attr_i <- attr
    }

    data_index <- length(data) + 1


    cache_exists_i <- case_when(
      is.na(content_stubs[i,]$cache_exists) ~ FALSE,
      is.null(content_stubs[i,]$cache_exists) ~ FALSE,
      TRUE ~ content_stubs[i,]$cache_exists,
    )

    if(cache_exists_i&!ignore_cache){

        message("Getting cached data")

        data_i <- open_dataset(content_stubs[i,]$cached_file,
                               format="parquet",
                               unify_schemas=TRUE)

        existing_attr <- data_i |>
                        select(any_of(c("Attribute"))) |>
                        distinct() |>
                        collect() |>
                        pull()

        remaining_attr <- attr_i[!(attr_i %in% existing_attr)]
        if(length(remaining_attr)!=0){
          data_i <- import_data(content_stubs,i,geo_struct,remaining_attr,unique(content_stubs$Year))
        }

    }else{
      message("Getting data from source file")
      data_i <- import_data(content_stubs,i, geo_struct,attr_i,unique(content_stubs$Year))


    }

    attr_i <- str_squish(attr_i)
    attr_i <- str_remove_all(attr_i, "[^A-z|0-9|[:punct:]|\\s]")
    attr_i <- str_remove_all(attr_i, ":")
    attr_i <- str_remove_all(attr_i, "/")
    attr_i <- str_remove_all(attr_i, fixed("\\"))

    data_i <- data_i |>
              filter(if_any(any_of(c("Attribute")), ~ .x %in% !!attr_i))

    if(collect_data){
      data[[data_index]] <- data_i |>
                            collect() |>
                            pivot_wider(names_from = "Attribute",values_from = "Value")
    }else{
      data[[data_index]] <- data_i
    }
    names(data)[[data_index]] <- content_stubs[i,]$identifier
  }

  return(data)
}





#' Get a summary of one or a collection of statistics across Censuses
#' @description This function allows to produce a summary of one or many statistics across censuses. Results are presented in a simple summary table.
#' The function allows to present individual statistics or an aggregation of several statistics (e.g. aggregate number of births by country to present
#'a continental total). If the name statistic containing totals is provided, the function has an option to calculate percentages (presented either in 0-1 or 0-100 scale).
#' @return data frame with data from file, filtered by division and election year
#' @importFrom dplyr select any_of starts_with filter if_any rename left_join mutate bind_rows if_else collect group_by summarise
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom methods is
#' @importFrom stringr str_replace_all
#' @importFrom rlang .data
#' @param table_number number of selected table
#' @param geo_structure character presenting the geographical structure to present stats (e.g. SA1,LGA,CED)
#' @param attribute list with vectors of statistics to be summarise. Each vector element will be aggregated and presented under the item's name, e.g.
#'  list("60 year old male"=c("Age_years_60_males","Age (Years): 60_males"),
#   "60 year old female"=c("Age_years_60_males","Age (Years): 60_females"))
#' @param geo_unit_names vector with names of the geographic structures to present. They need to correspond with geo_structure, e.g. if geo_structure="LGA",
#' acceptable values could be  c("Melbourne","Stonnington","Yarra"). If both this and geo_unit_codes are null, it will present all avaialable elements.
#' @param geo_unit_codes vector with ABS codes of the geo structures to present. Similar to geo_units_names.
#' @param selected_years  vector with selected years to display.
#' @param reference_total Optional. List containing the names of all statistics representing totals, e.g. list("Total"=c("Total_persons")
#' @param percentage_scale 1 if percentage to be presented in scale 0-1, or 100 to be shown as 0%-100%
#' @param ignore_cache If TRUE, it will ignore cached files
#' @param data_source result of get_census_data (will ignore other parameters if this is provided)
#' @param data_collected TRUE if data_source is a dataset, FALSE if is  a DB,arrow binding
#' @param census_table Instead of using a table number, this allows for a more complex filter table, e.g. containing different table numbers.
#'  Expected format matches the output of list_census_table().
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

  if(is(attribute,"data.frame")){
    attribute <- attribute_tibble_to_list(attribute)
  }
  if(is(reference_total,"data.frame")){
    reference_total <- attribute_tibble_to_list(reference_total)
  }


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
                                   collect_data =  FALSE,
                                   attr = attr)

    data_collected <- FALSE
  }

  if(length(data_source)==0)  stop("no data found")
  for(i in 1:length(data_source)){
  if(!is.null(data_source[[i]])){

    data_i <- data_source[[i]]

    #check if units are empty, SA1, CD
    units_count <- data_i |> select(any_of("Unit")) |> collect()

    if(nrow(units_count)!=0){
      units_count <- units_count |> pull()
      units_count <- sum(is.na(units_count)) / length(units_count)

      if(units_count==1){
        data_i <- data_i |>
        mutate(Unit=.data$Census_Code)
      }
    }


    if(!is.null(geo_unit_names)){
      data_i <- data_i |>
        filter(if_any(c("Unit"), ~ .x %in% geo_unit_names))
    }

    if(!is.null(geo_unit_codes)){
      data_i <- data_i |>
        filter(if_any(c("Census_Code"), ~ .x %in% geo_unit_codes))
    }

    if(!data_collected){
      data_i <-  data_i |> collect()
    }else{
      data_i <- data_i |>
        pivot_longer(-any_of(c("Year","Unit","Census_Code")),names_to="Attribute",values_to="Value")  |>
        filter(if_any(c("Value"), ~ !is.na(.x)))

    }


    if(is(attribute,"list")){
      for(j in 1:length(attribute)){

        attr_j <- attribute[[j]]

        attr_j <- str_squish(attr_j)
        attr_j <- str_remove_all(attr_j, "[^A-z|0-9|[:punct:]|\\s]")
        attr_j <- str_remove_all(attr_j, ":")
        attr_j <- str_remove_all(attr_j, "/")
        attr_j <- str_remove_all(attr_j, fixed("\\"))

        data_i <- data_i |>
          mutate(across(c("Attribute"), ~ if_else(.x %in% str_replace_all(attr_j,":","-"), names(attribute)[j],.x)))

      }
    }

    if(is(reference_total,"list")){
      for(j in 1:length(reference_total)){

        data_i <- data_i |>
          mutate(across(c("Attribute"), ~ if_else(.x %in% str_replace_all(reference_total[[j]],":","-"), names(reference_total)[j],.x)))

      }
    }

    # aggregate by same attribute, year, unit, census_code

    data_i <- data_i |>
      group_by(across(c("Census_Code","Unit","Year","Attribute"))) |>
      summarise(across(c("Value"), ~ sum(.x, na.rm=TRUE)),.groups="drop")


    if(!is.null(reference_total)){


      data_i  <- data_i |>
                calculate_percentage(key_col="Attribute",
                                     value_col="Value",
                                     key_value=names(reference_total)[1],
                                     percentage_scale=percentage_scale)


      colnames(data_i)[which(colnames(data_i)=="Total")] <- names(reference_total)[1]


    }
    data <- bind_rows(data,data_i)
  }
  }

  return(data)
}

