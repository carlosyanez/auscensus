# Census 2006 ----

# Geo : Merge
geo_2006  <- read_xls(path(raw_files_dir,"2006","Census2006_geog_desc.xls"),sheet = "Sheet1", col_types  ="text") %>%
  rename("ASGS_Structure"="Level",
         "Census_Code"="Code",
         "Census_Name"="Label"
  ) %>%
  mutate(AGSS_Code=Census_Code) %>%
  select(ASGS_Structure,Census_Code,AGSS_Code,Census_Name) %>%
  mutate(Year="2006")

geo <- bind_rows(geo,geo_2006) %>% distinct()
rm(geo_2006)
# Geo: make uniform
# cleanup






tables_2006 <- read_xls(path(raw_files_dir,"2006","BCP2006_table_desc.xls"),sheet = "Sheet1", col_types  ="text") %>%
  mutate(Year="2006")

colnames(tables_2006) <- colnames(tables)
tables <- bind_rows(tables,tables_2006) %>% distinct()



descriptors_2006 <-  read_xls(path(raw_files_dir,"2006","BCP2006_cell_desc_dp.xls"),sheet = "BCP2006_cell_desc", col_types  ="text") %>%
  mutate(Year="2006")



zip_file    <- path(raw_files_dir,str_c("2006",".zip"))
zip_content <- zip_list(zip_file)


zip_2 <- zip_content %>% filter(str_detect(filename,"\\.zip"))
  unzip(zip_file,zip_2$filename,junkpaths = TRUE,exdir = raw_files_dir)

zip_2 <- zip_2 %>% mutate(unzipped= str_c(raw_files_dir,"/",str_extract(filename,"[^/\\\\&\\?]+\\.\\w{3,4}(?=([\\?&].*$|$))")))

for(i in 1:nrow(zip_2)){
  content_i <- zip_list(zip_2[i,]$unzipped) %>% mutate(zipped=zip_2[i,]$filename)

  if(!exists("content_2006")){
      content_2006<- content_i %>% filter(!str_detect(filename,"\\.csv"))
  }else{
    content_2006 <- bind_rows(content_2006,content_i)
  }

}



#metadata and content files
metadata <- zip_content %>%
  filter(str_detect(filename,"Metadata")) %>%
  mutate(year="2006")




content_i  <- zip_content %>%
  filter(str_detect(filename,census_strings[i])) %>%
  select(filename) %>%
  mutate(Year="2006",
         zip = str_c("2006",".zip")) %>%
  mutate(geo = str_remove(filename,str_c(census_strings[i],"/")),
         geo = str_extract(geo,"^[^/]+"),
         element = str_extract(filename,"[a-zA-Z]{1}[0-9]{2,}")
  )

content_2006 <-

