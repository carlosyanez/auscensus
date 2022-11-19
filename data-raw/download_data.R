library(httr)
library(tidyverse)
library(here)
library(fs)

raw_files_dir <- here("data-raw","files")
dir_create(raw_files_dir)

# Get List ----

censuses <- tribble(~ Census, ~ url,~type,
                    2021,"https://www.abs.gov.au/census/find-census-data/datapacks/download/2021_GCP_all_for_AUS_short-header.zip","GCP",
                    2016,"https://www.abs.gov.au/census/find-census-data/datapacks/download/2016_GCP_all_for_AUS_short-header.zip","GCP",
                    2011,"https://www.abs.gov.au/census/find-census-data/datapacks/download/2011_BCP_all_for_AUST_short-header.zip","BCP",
                    2006,"https://www.abs.gov.au/AUSSTATS/abs@archive.nsf/LookupAttach/2006CensusDataPack_BCPPublication04.11.200/$file/census06bcp.zip","BCP"
                    )

locator <- str_locate_all(censuses$url,"/")
#rm(locations)
for(i in 1:length(locator)){
  pos_i <- max(locator[[i]][,1])
  if(!exists("locations")){
    locations <- pos_i
  }else{
    locations <- c(locations, pos_i)

  }

}

censuses$location <- locations

censuses <- censuses %>%
            mutate(filename = str_sub(url,location+1,str_length(url)))

# download -----

for(i in 1:nrow(censuses)){
  source       <- censuses[i,]$url
  destination  <- path(raw_files_dir,censuses[i,]$filename)

  if(!file_exists(destination))
    download.file(source,destination,method="wget")

}

# rename ----
for(i in 1:nrow(censuses)){
  file_move(path(raw_files_dir,censuses[i,]$filename),
                path(raw_files_dir,str_c(censuses[i,]$Census,".zip")))

}


