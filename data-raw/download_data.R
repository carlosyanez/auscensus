library(httr)
library(tidyverse)
library(here)
library(fs)

raw_files_dir <- here("data-raw","files")
dir_create(raw_files_dir)

# Get List ----

censuses <- tribble(~ Census, ~ file,~type,
                    2021,"https://www.abs.gov.au/census/find-census-data/datapacks/download/2021_GCP_all_for_AUS_short-header.zip","GCP",
                    2016,"https://www.abs.gov.au/census/find-census-data/datapacks/download/2016_GCP_all_for_AUS_short-header.zip","GCP",
                    2011,"https://www.abs.gov.au/census/find-census-data/datapacks/download/2011_BCP_all_for_AUST_short-header.zip","BCP",
                    2006,"https://www.abs.gov.au/AUSSTATS/abs@archive.nsf/LookupAttach/2006CensusDataPack_BCPPublication04.11.200/$file/census06bcp.zip","BCP"
                    )


censuses <- censuses %>%
  mutate(zip=str_extract(file,"/*\\.zip"))

#for(i in  1:nrow(censuses)){
#  dest_file <-str_c("data_pack",censuses[i,]$Census,".zip")
#  download.file(censuses[i,]$file,str_c("data_pack",dest_file))
#}

dest_files <- dir_ls(raw_files_dir)
dest_files <- dest_files[str_detect(dest_files,"zip$")]

for(i in  1:nrow(censuses)){
  zip::unzip(dest_files[i])
}
# Download all files ----


for(i in 1:nrow(to_download)){
  source       <- to_download[i,]$link
  destination  <- path(raw_files_dir,to_download[i,]$file)

  if(!file_exists(destination))
    download.file(source,destination)

}

write_csv(to_download,file=path(raw_files_dir,"sources.csv"))
