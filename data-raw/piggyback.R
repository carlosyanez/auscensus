library(piggyback)
library(here)
library(fs)

files_dir      <- here("data-raw","processed")
repo           <- "carlosyanez/auscensus"
version       <- "aux_files"

#pb_new__release(repo,version)

files_list <- tibble(file=dir_ls(files_dir),
                     Year=str_remove(file,str_c(files_dir,"/")) %>% str_extract(.,"^[0-9]+"))


#create new release
pb_new_release(repo,version)


# upload catalogue items ---
pb_upload(file=files_list$file,repo,version)



