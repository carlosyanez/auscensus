library(fs)
library(tidyverse)
library(arrow)


here()
geo_structure <- "SA4"
table_number <- "12"
type <- "LGA"
geo_units <- c("Melbourne","Sydney")

attr <- list_census_attributes("12")

attributes <-"Total_dependent_children_female_parent_speaks_english_only_male_parent_language_and_proficiency_in_english_not_stated"
attributes<- list("Men"=c("Total_males","Total,Males"),"Women"=c("Total_females","Total,Females"))
reference_total <- list("Total"=c("Total_persons","Total,Persons"))

a <- list_census_geo("CED",geo_name_regex = "Melb")






selected_years <- c("2021","2016")
cache <- TRUE

get_census_summary(table_number,geo_structure,attributes)

census_table <- list_census_tables(table_number)
a<-get_census_data(census_table,geo_structure)
is.null(a[[1]])


content <- load_auscensus("content.zip") |>
  filter(str_detect(element,table_number)) |>
  filter(str_detect(geo,geo_structure))
