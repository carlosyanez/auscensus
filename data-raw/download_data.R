library(httr)
library(tidyverse)
library(here)
library(fs)

raw_files_dir <- here("data-raw","files")
dir_create(raw_files_dir)

# Get List ----

censuses <- tribble(~ Census, ~ repo,
                    2021,"HughParsonage/Census2021.DataPack/contents/data",
                    2016,"HughParsonage/Census2016.DataPack/contents/data",
                    2011,"HughParsonage/Census2011.DataPack/contents/data"
                    )

to_download <- tibble()

for(i in  1:nrow(censuses)){

req <- GET(paste0("https://api.github.com/repos/",
                  censuses[i,]$repo))

file_list <- content(req)
filenames <- sapply(file_list, function(x) x$name)

file_list <- file_list[grepl("rda$", filenames)]

to_download_i <- tibble(file = sapply(file_list, function(x) x$name),
                        link = sapply(file_list, function(x) x$download_url)) %>%
                 mutate(file=str_c(censuses[i,]$Census,"_",file),
                        census =censuses[i,]$Census)

to_download <- bind_rows(to_download,to_download_i)
}

# Download all files ----


for(i in 1:nrow(to_download)){
  source       <- to_download[i,]$link
  destination  <- path(raw_files_dir,to_download[i,]$file)

  if(!file_exists(destination))
    download.file(source,destination)

}

write_csv(to_download,file=path(raw_files_dir,"sources.csv"))
