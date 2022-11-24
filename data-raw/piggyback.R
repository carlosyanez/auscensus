library(piggyback)
library(here)
library(fs)
library(tidyverse)


files_dir      <- here("data-raw","processed")
repo           <- "carlosyanez/auscensus"
version       <- "aux_files"

#pb_new__release(repo,version)

files_list <- tibble(file=dir_ls(files_dir),
                     Year=str_remove(file,str_c(files_dir,"/")) %>% str_extract(.,"^[0-9]+"))

files_list %>% filter(str_detect(file,"2021")) %>% pull(file) %>% file_delete()

#create new release
pb_new_release(repo,version)


# upload catalogue items ---
pb_upload(file=files_list$file,repo,version)



