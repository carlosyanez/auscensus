
# Precompiled vignettes

library(knitr)
library(here)
library(fs)
library(stringr)
library(dplyr)


orig_folder <- here("precompile")
vignettes_folder <- here("vignettes")
dir_create(vignettes_folder)

vignettes <- dir_ls(orig_folder,regexp=".Rmd") |> str_remove_all(str_c(orig_folder,"/"))

for(vig in vignettes){
 knit(here(orig_folder,vig), here(vignettes_folder,vig))
}

#i<-1
#knit(here(orig_folder,vignettes[i]), here(vignettes_folder,vignettes[i]))

pkgdown::build_articles()

##compile site

pngs <-  dir_ls(vignettes_folder,regexp=".png")
article_dir<-here("docs","articles")
#dir_create(article_img)
file_copy(pngs, article_dir,overwrite = TRUE)

article_html <- dir_ls(article_dir,regexp="html")

for(article in article_html){
  file <- tibble(orig=readLines(article))

  file <-file |> mutate(new=if_else(str_detect(orig,"img src=\"articles/"),
                                  str_replace(orig,"img src=\"articles/","img src=\"https://carlosyanez.github.io/auscensus/articles/"),
                                  orig))
  write(file$new,article)
}

