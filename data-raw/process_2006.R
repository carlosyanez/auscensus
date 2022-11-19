# Census 2006 ----

# Geo : Merge
geo_2006  <- read_xls(path(raw_files_dir,census_year[4],"Census2006_geog_desc.xls"),sheet = "Sheet1", col_types  ="text") %>%
  rename("ASGS_Structure"="Level",
         "Census_Code"="Code",
         "Census_Name"="Label"
  ) %>%
  mutate(AGSS_Code=Census_Code) %>%
  select(ASGS_Structure,Census_Code,AGSS_Code,Census_Name) %>%
  mutate(Year=census_year[4])

geo <- bind_rows(geo,geo_2006) %>% distinct()
rm(geo_2006)
# Geo: make uniform
# cleanup

geo <- geo %>%
  mutate(ASGS_Structure = str_remove_all(ASGS_Structure,"\\d+"),
         Census_Name=case_when(
           ASGS_Structure=="CED" ~ str_remove_all(Census_Name,"\\((.*?)\\)") %>%
             str_remove_all(.,"\\(") %>%
             str_remove_all(.,"\\)") %>%
             str_remove_all(.,",(.*?)$") %>%
             str_squish(.),
           TRUE ~ Census_Name
         ))

geo2 <- geo %>%
  select(-AGSS_Code) %>%
  distinct() %>%
  pivot_wider( names_from = Year,values_from = Census_Code )






tables_2006 <- read_xls(path(raw_files_dir,census_year[4],"BCP2006_table_desc.xls"),sheet = "Sheet1", col_types  ="text") %>%
  mutate(Year=census_year[4])

colnames(tables_2006) <- colnames(tables)
tables <- bind_rows(tables,tables_2006) %>% distinct()







descriptors_2006 <-  read_xls(path(raw_files_dir,census_year[4],"BCP2006_cell_desc_dp.xls"),sheet = "BCP2006_cell_desc", col_types  ="text") %>%
  mutate(Year=census_year[4])

