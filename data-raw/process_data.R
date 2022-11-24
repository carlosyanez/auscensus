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
                     "2016 Census GCP All Geographies for AUST",
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




# Census 2006 ----

geo_2006  <- read_xls(path(raw_files_dir,"2006","Census2006_geog_desc.xls"),sheet = "Sheet1", col_types  ="text") %>%
  rename("ASGS_Structure"="Level",
         "Census_Code"="Code",
         "Census_Name"="Label"
  ) %>%
  mutate(AGSS_Code=Census_Code) %>%
  select(ASGS_Structure,Census_Code,AGSS_Code,Census_Name) %>%
  mutate(Year="2006")


tables_2006 <- read_xls(path(raw_files_dir,"2006","BCP2006_table_desc.xls"),sheet = "Sheet1", col_types  ="text") %>%
  mutate(Year="2006")
colnames(tables_2006) <- colnames(tables)

descriptors_2006 <-  read_xls(path(raw_files_dir,"2006","BCP2006_cell_desc_dp.xls"),sheet = "BCP2006_cell_desc", col_types  ="text") %>%
  mutate(Year="2006")

#content

zip_file    <- path(raw_files_dir,str_c("2006",".zip"))
zip_content <- zip_list(zip_file)

zip_2 <- zip_content %>%
          filter(str_detect(filename,"\\.zip")) %>%
          mutate(just_file= str_extract(filename,"[^/\\\\&\\?]+\\.\\w{3,4}(?=([\\?&].*$|$))"),
                 new_file=str_c("2006_",just_file),
                 unzipped= str_c(raw_files_dir,"/",new_file))


unzip(zip_file,zip_2$filename,junkpaths = TRUE,exdir = raw_files_dir)

fs::file_move(path(raw_files_dir,zip_2$just_file),path(raw_files_dir,zip_2$new_file))

for(i in 1:nrow(zip_2)){
  content_i <- zip_list(zip_2[i,]$unzipped) %>% mutate(zipped=zip_2[i,]$new_file)

  if(!exists("content_2006_i")){
    content_2006_i<- content_i   # %>% filter(!str_detect(filename,"\\.csv"))
  }else{
    content_2006_i <- bind_rows(content_2006_i,content_i)
  }

}

zip_content <- zip_content %>% mutate(zipped="")

#colnames(zip_content)
#colnames(content_2006)


content_2006 <- bind_rows(zip_content %>% select(filename,zipped),
                          content_2006_i %>% select(filename,zipped)) %>%
                mutate(Year="2006",
                      zip = if_else(zipped=="","2006.zip",zipped)) %>%
                mutate(stub=case_when(
                  zip=="2006.zip" ~ "Basic Community Profile",
                  zip=="2006_BCP_ASGC_06_R2.1.zip" ~ "BCP_ASGC_06_R2.1",
                  zip=="2006_BCP_CGIA_06_R2.0.zip" ~ "BCP_CGIA_06_R2.0"
                )) %>%
  mutate(geo = str_remove(filename,str_c(stub,"/")),
         geo = str_extract(geo,"^[^/]+"),
         element = str_extract(filename,"[a-zA-Z]{1}[0-9]{2,}")
  ) %>%
  filter(!is.na(element)) %>%
  select(-any_of(c("zipped","stub"))) %>%
  mutate(Year=as.numeric(Year)) %>%
  mutate(sub_element = str_extract(filename,"[a-zA-Z]{1}[0-9]{2,}(_)?[a-zA-Z]{1}"))


#merge into main elements -----
colnames(content)
colnames(content_2006)

content <- bind_rows(content,content_2006) %>%
            distinct() %>%
            mutate(sub_element=if_else(is.na(sub_element),element,sub_element))


#descriptors

descriptors_2006 <- descriptors_2006 %>% rename("Short"="Cell_Id",
                            "Long"="Cell_desc_long",
                            "Columnheadingdescriptioninprofile"="Column_label_Desc",
                            "Profiletable"="Table") %>%
                    mutate(DataPackfile=str_c(Profiletable,str_to_upper(Split_Table_DataPack))) %>%
                    select(-Split_Table_DataPack ,-Split_Table_Profile ,-Cell_desc_short ) %>%
                    mutate(Year=as.numeric(Year))

colnames(descriptors)
colnames(descriptors_2006)

descriptors <- bind_rows(descriptors,descriptors_2006) %>% distinct()


#geo

colnames(geo)
colnames(geo_2006)

geo <- bind_rows(geo, geo_2006 %>% mutate(Year=as.numeric(Year))) %>%distinct()


#tables
colnames(tables)
colnames(tables_2006)

tables <- bind_rows(tables, tables_2006 %>% mutate(Year=as.numeric(Year))) %>%
          distinct() %>%
          filter(!is.na(`Table Number`))


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
  pivot_wider( names_from = Year,
               values_from = Census_Code,
               values_fn = ~ str_c(.x,collapse=","))


geo_reverse <- geo %>%
  distinct() %>%
  pivot_wider(names_from = Year,
              values_from = Census_Name,
              values_fn = ~ str_c(.x,collapse=","))

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


descriptors <- descriptors %>%
        mutate(DataPackfile=if_else(is.na(DataPackfile),Profiletable,DataPackfile)) %>%
        distinct()


a <-content %>% distinct(geo,element,Year) %>%
  mutate(initial=TRUE,
         table   = str_sub(element,2)) %>%
  select(-element) %>%
  distinct() %>%
  pivot_wider(c(table,geo),values_from = initial,names_from = "Year")


a  %>% filter

#saving descriptors as-is ----
save_zip_parquet(geo_key,"geo_key",processed_files_dir)
save_zip_parquet(geo_reverse,"geo_reverse",processed_files_dir)
save_zip_parquet(tables,"tables",processed_files_dir)
save_zip_parquet(descriptors,"descriptors",processed_files_dir)
save_zip_parquet(content,"content",processed_files_dir)





