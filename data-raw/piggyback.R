library(piggyback)
library(here)
library(fs)

files_dir      <- here("data-raw","processed")
repo           <- "carlosyanez/auspol"
version        <- "data"

#pb_new_release(repo,version)

files_list <- dir_ls(files_dir)



pb_upload(file=files_list,repo,version)

