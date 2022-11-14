library(piggyback)
library(here)
library(fs)

files_dir      <- here("data-raw","processed")
repo           <- "carlosyanez/auscensus"
version_base        <- "data_"

#pb_new__release(repo,version)

files_list <- tibble(file=dir_ls(files_dir),
                     Year=str_remove(file,str_c(files_dir,"/")) %>% str_extract(.,"^[0-9]+")) %>%
               mutate(Year=if_else(is.na(Year),"common",Year))


#create new releases
for(section in unique(files_list$Year)){
   version <- str_c(version_base,section)
   pb_new_release(repo,version)
}


# upload catalogue items ---

for(section in unique(files_list$Year)){
  version <- str_c(version_base,section)
  files   <- files_list %>% filter(Year==section) %>% pull(file)
  pb_upload(file=files,repo,version)
}


