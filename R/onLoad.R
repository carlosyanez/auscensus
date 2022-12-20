.onLoad <- function(libname, pkgname){
 # packageStartupMessage("Loading {auscensus}")

  #default cache
  home       <- Sys.getenv("HOME")
  cache_path <- file.path(home, ".auscensus_cache")

  if(!dir.exists(cache_path))
    manage_cache_dir(cache_path)

  remove_census_cache_csv()

}
