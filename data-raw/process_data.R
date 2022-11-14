library(httr)
library(tidyverse)
library(here)
library(fs)
library(arrow)

processed_files_dir <- here("data-raw","processed")
raw_files_dir <- here("data-raw","files")

dir_create(processed_files_dir)

sources <- read_csv(path(raw_files_dir,"sources.csv"))


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

# transform ----

for(i in 1:nrow(sources)){

  file <- path(raw_files_dir,sources[i,]$file)
  object <- load(file)
  assign("obj",get(object))
  rm(list=c(object,"object"))

  save_zip_parquet(obj,str_remove(sources[i,]$file,".rda"),processed_files_dir)


}
