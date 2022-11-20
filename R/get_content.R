

#my_tables <- list_census_tables(number="09")
#geo_type <- "CED"
#geos <- list_census_geo(geo_types = geo_type, geo_names = c("Goldstein","Wills"))

#selected_years <- list_census_years()
#cache <- TRUE


get_census_data <- function(my_tables,geo_types,geo_names,selected_years,cache){

  #basic data
  avail_years <-  list_census_years()
  all_years   <-  list_census_years(mode="all")
  cache_dir     <- find_census_cache()


  #get tables to explore and filter (table number, year)
  table_non_year_cols <- colnames(my_tables)[!(colnames(my_tables) %in% avail_years)]

  my_tables <- my_tables |>
    pivot_longer(all_of(avail_years), names_to = "Year",values_to = "flag") |>
    filter(if_any(c("Year"), ~ .x %in% selected_years))

  table_stubs  <- get_auscensus_metadata("tables.zip") |>
    pivot_longer(all_of(all_years), names_to = "Year",values_to = "initial") |>
    filter(if_any(c("Year"), ~ .x %in% selected_years)) |>
    left_join(my_tables,by=c(table_non_year_cols,"Year"))  |>
    filter(if_any(c("initial"), ~ !is.na(.x)))       |>
    filter(if_any(c("flag"), ~ !is.na(.x)))          |>
    mutate(element=str_c(.data$initial,.data$Number),
           flag=TRUE,
           Year=as.numeric(.data$Year)
    ) |>
    select(Year,element,flag)


  #geo types to filter
  geo_non_year_cols <- colnames(geos)[!(colnames(geos) %in% avail_years)]
  geo_list    <- geos |>
    pivot_longer(all_of(avail_years), names_to = "Year",values_to = "flag") |>
    distinct(.data$ASGS_Structure) |>
    pull()


  #create content stubs
  content_stubs <- crossing(tibble(geo=geo_type),table_stubs) |>
    mutate(identifier =str_c(.data$Year,"_",.data$geo,"_",.data$element),
           cached_file=path(cache_dir,str_c(identifier,".parquet")),
           cache_exists=file_exists(cached_file))

  #clean up so far
  rm(list=c("geo_list","geo_non_year_cols","geos",
            "table_non_year_cols","my_tables","table_stubs"))

  #only load extra datasets if there is a need

  if(!all(content_stubs$cache_exists)){

    content <- get_auscensus_metadata("content.zip") |>
      left_join(content_stubs %>% select(-any_of(c("flag","cached_file","identifier"))),
                by=c("Year","element","geo")) |>
      filter(if_any(c("cache_exists"), ~ .x==FALSE))

    geo_decode    <- get_auscensus_metadata("geo_reverse.zip") |>
      filter(if_any(c("ASGS_Structure"), ~ .x %in% geo_type)) |>
      pivot_longer(-any_of(c("ASGS_Structure","Census_Code")), names_to="Year",values_to="Unit") |>
      filter(if_any(c("Year"), ~ .x %in% avail_years))

  descriptors <- get_auscensus_metadata("descriptors.zip") |>
    mutate(across(c("Profiletable"), ~ str_remove(.x,"[a-zA-Z]$"))) |>
    left_join(content_stubs,
              by=c("Year","Profiletable"="element")) |>
    filter(if_any(c("flag"),~.x==TRUE))

  }


  data <- list()
  for(i in 1:nrow(content_stubs)){

    if(content_stubs[i,]$cache_exists){

      data_i <- read_parquet(content_stubs[i,]$cached_file)

    }else{


    content_i <- content |>
      select(-any_of("flag")) |>
      left_join(content_stubs[i,], by=c("geo","Year","element")) |>
      filter(if_any("flag", ~ .x==TRUE))

    data_i <- NULL

    for(j in 1:nrow(content_i)){
      #read file

      zip_file <- path(cache_dir,content_i[j,]$zip)
      filename <- content_i[j,]$filename
      temp_file <- path(cache_dir,str_extract(filename,"[^/\\\\&\\?]+\\.\\w{3,4}(?=([\\?&].*$|$))"))
      unzip(zipfile = zip_file,files=filename,junkpaths = TRUE,exdir = cache_dir)
      data_j       <- read_csv(temp_file)
      rm(temp_file)

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
          left_join(data_j,by=data_j_key)

      }
    }


    geo_decode_i <- geo_decode |>
      filter(if_any(c("ASGS_Structure"),~ .x==content_stubs[i,]$geo)) |>
      filter(if_any(c("Year"), ~ .x==content_stubs[i,]$Year)) |>
      select(-any_of(c("ASGS_Structure","Year")))

    colnames(data_i)[1] <- "Census_Code"

    data_i <- data_i |>
      left_join(geo_decode_i, by="Census_Code") |>
      relocate("Unit",.before=1) |>
      mutate(Year=content_stubs[i,]$Year,.before=1)

    if(cache){
      write_parquet(data_i,path(cache_dir,filename))

    }

    }

    data[[content_stubs[i,]$identifier]] <- data_i

  }

  return(data)
}
