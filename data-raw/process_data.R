#This file looks at the census files to :
# 1. extract metadata files and record variables, translators, table names
# 2. create a directory for all the files with the actual data, so they can be used in the package

# Setup ----


library(httr)
library(tidyverse)
library(here)
library(fs)
library(readxl)
library(arrow)
library(zip)

raw_files_dir <- here("data-raw","files")
dir_create(raw_files_dir)
processed_files_dir <- here("data-raw","processed")
dir_create(processed_files_dir)


# aux functions ----
save_zip_parquet <- function(df,name,dest_dir){

  zipname     <- str_c(name,".zip")
  parquetname <- str_c(name,".parquet")

  write_parquet(df,path(dest_dir,parquetname),compression="brotli")
  zip::zip(zipfile=path(dest_dir,zipname),
           files=path(dest_dir,parquetname),
           mode = "cherry-pick")
  fs::file_delete(path(dest_dir,parquetname))

}

# Folders and files ----

census_year     <- c(2021,2016,2011) #,
                     #2006)
census_strings  <- c("2021 Census GCP All Geographies for AUS",
                     "2016 Census GCP All Geographies for AUS",
                     "2011 Census BCP All Geographies for AUST") #,
                 #    "Basic Community Profile")

metadata <- tibble()
content  <- tibble()



for(i in 1:length(census_year)){
  zip_file    <- path(raw_files_dir,str_c(census_year[i],".zip"))
  zip_content <- zip_list(zip_file)

  #metadata and content files
  metadata <- zip_content %>%
    filter(str_detect(filename,"Metadata")) %>%
    mutate(year=census_year[i])

  content_i  <- zip_content %>%
    filter(str_detect(filename,census_strings[i])) %>%
    select(filename) %>%
    mutate(Year=census_year[i],
           zip = str_c(census_year[i],".zip")) %>%
    mutate(geo = str_remove(filename,str_c(census_strings[i],"/")),
           geo = str_extract(geo,"^[^/]+"),
           element = str_extract(filename,"[a-zA-Z]{1}[0-9]{2,}")
    )

  content <- bind_rows(content,content_i)


  #extract metadata files
  census_folder <- path(raw_files_dir,census_year[i])
  dir_create(census_folder)
  zip::unzip(zip_file,metadata$filename,exdir = census_folder,junkpaths=TRUE)

}
rm(zip_file,zip_content,metadata,i,census_strings,content_i)


# Get metadata and decoders ----

geo         <- tibble()
tables      <- tibble()
descriptors <- tibble()

keep_vars <- ls()

# Census 2011 to 2016 -----



tables_string <- c("Table Number, Name, Population","Table number, name, population","Table number, name population")
tables_skip   <- c(8,9,2)

descriptors_string <- c("Cell Descriptors Information","Cell descriptors information","Cell descriptors information")
descriptors_skip  <- c(10,10,3)


for(i in 1:length(census_year[1:3])){
  print(census_year[i])
  metadata       <- fs::dir_ls(path(raw_files_dir,census_year[i]))
  geo_file       <- metadata[str_detect(metadata,"geo")]
  metadata_file  <- metadata[str_detect(metadata,"Metadata")]

  geo_sheets <- excel_sheets(geo_file)

  geo_i   <- map_dfr(excel_sheets(geo_file),
                        function(x){
                               print(x)
                               read_xlsx(geo_file,sheet = x)
                        }) %>%
              mutate(Year=census_year[i])

  colnames(geo_i) <- str_remove(colnames(geo_i),str_c("_",census_year[i]))

  #geo_i <- geo_i %>% select(-any_of(c("AGSS_Code")))

  if(colnames(geo_i)[1]=="Level"){
    geo_i <- geo_i %>%
             rename("ASGS_Structure"="Level",
                    "Census_Code"="Code",
                    "Census_Name"="Label"
                    ) %>%
              mutate(AGSS_Code=Census_Code)

  }

  geo_i <- geo_i %>% select(ASGS_Structure,Census_Code,AGSS_Code,Census_Name,Year)

  geo <- bind_rows(geo,geo_i) %>% distinct()

  tables_i      <- read_xlsx(metadata_file,sheet=tables_string[i],skip=tables_skip[i]) %>%
                      mutate(Year=census_year[i])

  colnames(tables_i) <- str_to_title(colnames(tables_i))

  tables <- bind_rows(tables,tables_i) %>% distinct()

  descriptors_i <- read_xlsx(metadata_file,sheet=descriptors_string[i],skip=descriptors_skip[i]) %>%
                      mutate(Year=census_year[i])

  colnames(descriptors_i) <- str_remove_all(colnames(descriptors_i)," ")

  descriptors_i <- descriptors_i %>% select(-Sequential)

  descriptors <- bind_rows(descriptors,descriptors_i) %>% distinct()

}

rm(list=ls()[!(ls() %in% keep_vars)])
keep_vars <- ls()


# Clean up data frames ----

geo <- geo %>%
  select(-AGSS_Code) %>%
  distinct() %>%
  mutate(ASGS_Structure = if_else(str_starts(ASGS_Structure,"SA"), ASGS_Structure, str_remove_all(ASGS_Structure,"\\d+")),
         Census_Name=case_when(
           ASGS_Structure=="CED" & (str_detect(str_to_lower(Census_Name),"no usual") |
                                    str_detect(str_to_lower(Census_Name),"offshore")) ~ Census_Name,
           ASGS_Structure=="CED" ~ str_remove_all(Census_Name,"\\((.*?)\\)") %>%
             str_remove_all(.,"\\(") %>%
             str_remove_all(.,"\\)") %>%
             str_remove_all(.,",(.*?)$"),
           ASGS_Structure=="POA" & (str_detect(str_to_lower(Census_Name),"no usual") |
                                    str_detect(str_to_lower(Census_Name),"offshore") |
                                      str_detect(str_to_lower(Census_Name),"unclassified")) ~ Census_Name,
           ASGS_Structure=="POA" ~ str_remove_all(Census_Name,"[^0-9]+"),
           TRUE ~ str_remove_all(Census_Name,"\\([A-Z]\\)")
         ) %>% str_to_title(.) %>% str_squish(.)
         ) %>%
  #remove older statistical units
  filter(!(ASGS_Structure %in% c("CCD","SLA","SD")))


#geo_n <- geo %>%count(ASGS_Structure,Census_Name,Year)

geo_key <- geo %>%
  distinct() %>%
  pivot_wider( names_from = Year,values_from = Census_Code )


geo_reverse <- geo %>%
  distinct() %>%
  pivot_wider(names_from = Year,values_from = Census_Name )

# tables
tables <- tables %>%
          mutate(Initial=str_sub(`Table Number`,1,1),
                 Number = str_remove(`Table Number`,Initial)) %>%
          replace_na(list("Table Population"="NA")) %>%
          select(-`Table Number`) %>%
          pivot_wider(names_from = Year,values_from = Initial) %>%
          relocate(Number, .before=1)

  tables <- tables %>%
          filter(!is.na(Number)) %>%
          arrange(as.numeric(Number))


#saving descriptors as-is
save_zip_parquet(geo_key,"geo_key",processed_files_dir)
save_zip_parquet(tables,"tables",processed_files_dir)
save_zip_parquet(descriptors,"descriptors",processed_files_dir)
save_zip_parquet(content,"content",processed_files_dir)




## Census 2006 does not conform to present format - more time required to make it conform  ----

