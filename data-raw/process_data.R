# Setup ----

library(httr)
library(tidyverse)
library(here)
library(fs)
library(readxl)
library(arrow)

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

# Census 2021 ----
# Folders and files ----
census_year <-2021
census_folder <- path(raw_files_dir,census_year)
data_folder   <- path(census_folder,"2021 Census GCP All Geographies for AUS")

metadata_file <- path(census_folder,"Metadata","Metadata_2021_GCP_DataPacks_R1_R2.xlsx")
geo_file      <- path(census_folder,"Metadata","2021Census_geog_desc_1st_and_2nd_release.xlsx")

dest_folder   <- path(processed_files_dir,census_year)
dir_create(dest_folder)

# Get metadata and decoders ----

geo_sheets <- excel_sheets(geo_file)
geo_2021   <- map_dfr(excel_sheets(geo_file),  function(x){
  read_xlsx(geo_file,sheet = x)
})

tables_2021      <- read_xlsx(metadata_file,sheet="Table Number, Name, Population",skip=8)
descriptors_2021 <- read_xlsx(metadata_file,sheet="Cell Descriptors Information",skip=10)

#filter by existing geos
geo_list <- dir_ls(data_folder) %>% str_remove(.,str_c(data_folder,"/"))
geo_2021 <- geo_2021 %>% filter(ASGS_Structure %in% geo_list)

# Convert data files ----

for(geo in geo_list){

  data_subfolder      <- path(data_folder,geo)
  geo_all_files       <- dir_ls(data_subfolder)

  if(length(geo_all_files)==1){
    data_subfolder      <- path(data_folder,geo,"AUS")
    geo_all_files <-  dir_ls(data_subfolder)
  }

  print(geo)

  for(table in tables_2021$`Table Number`){

    #print(table)

    data_files <- geo_all_files[str_detect(geo_all_files,table)]
    data <- tibble()
    for(data_file in data_files){
      #print(data_file)
      data_i <- read_csv(data_file,col_types =  cols(.default = "c"))
      data <- bind_rows(data,
                        data_i)

    }

    data <- data %>%
      rename("Code"=str_c(geo,"_CODE_2021")) %>%
      pivot_longer(-c("Code"),names_to = "Attribute", values_to = "Value") %>%
      left_join(descriptors_2021 %>%
                  filter(DataPackfile==table) %>%
                  select(Short,Long),
                by=c("Attribute"="Short")) %>%
      left_join(geo_2021 %>%
                  filter(ASGS_Structure==geo) %>%
                  select(Label="Census_Name_2021","Census_Code_2021"),
                by=c("Code"="Census_Code_2021")) %>%
      mutate(Code=str_remove(Code,geo),
             Table_code=table,
             Table=  tables_2021[j,]$`Table Name`,
             Geo=geo)  %>%
      mutate(Value=str_remove_all(Value,"[^0-9]"),
             Value=as.numeric(Value)) %>%
      select(Geo,Geo_code=Code,Geo_name=Label,Table_code,Table_name=Table,Value)

    dest_filename <- str_c(census_year,geo,table,sep="_")

    save_zip_parquet(data,dest_filename,dest_folder)
    print(dest_filename)

  }
}



