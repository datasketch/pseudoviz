
library(tidyverse)
library(pdftools)
library(purrrlyr)

## Prepare meta from images

txt <- pdf_text("inst/drawings/Pseudo Viz.pdf")

pvMeta <- data_frame(txt = txt, 
                     numid = 1:length(txt),
                     original_img = paste0("Pseudo Viz - ",1:length(txt),".png"))
pvMeta <- pvMeta %>% separate(txt, c("name","id"), sep = "\n")
pvMeta[pvMeta == ""] <- NA
meta_pdf <- pvMeta %>% mutate(group = is.na(id) & !(is.na(name))) %>%
  mutate(group = ifelse(group,name,NA)) %>% 
  tidyr::fill(group,.direction="down") %>%
  mutate(id = gsub(" ","_",id)) %>% 
  filter(!is.na(id), group != "Helpers") %>% 
  mutate(img = paste0(id,".png"))

meta_pdf_dup <- meta_pdf %>% group_by(id) %>% filter(n() > 1)
if(nrow(meta_pdf_dup) > 0) warning("Check dup ids")

## Read manual meta from googlesheets
library(googlesheets)


# 1KGOHJ7isodE8c6DilX4z-BpA-SrU9DSiREAIOBGMeKA
# JSON

# https://spreadsheets.google.com/feeds/list/1KGOHJ7isodE8c6DilX4z-BpA-SrU9DSiREAIOBGMeKA/od6/public/values?alt=json
# https://cors.io/?http://spreadsheets.google.com/feeds/list/1KGOHJ7isodE8c6DilX4z-BpA-SrU9DSiREAIOBGMeKA/od6/public/values?alt=json

s <- gs_url("https://docs.google.com/spreadsheets/d/1KGOHJ7isodE8c6DilX4z-BpA-SrU9DSiREAIOBGMeKA/edit#gid=0")
meta_manual <- gs_read_csv(s) %>% select(-thumbnail)

vars <- c("id","img_url","thumbnail_url","data_restrictions","params","comments","ctypes")
github_raw_img <- "https://raw.githubusercontent.com/jpmarindiaz/pseudoviz/master/inst/imgs/"
github_raw_thumbnail_img <- "https://raw.githubusercontent.com/jpmarindiaz/pseudoviz/master/inst/thumbnails/"

# Fix anti join

anti1 <- anti_join(meta_pdf, meta_manual)
nrow(anti1)
#mop::copy_clipboard(anti1 %>% select(id, name, group, img),col.names = FALSE)


anti2 <- anti_join(meta_manual,meta_pdf)
anti2

meta <- meta_manual %>% 
  mutate(img_url = paste0(github_raw_img,img)) %>% 
  mutate(thumbnail_url = paste0(github_raw_thumbnail_img,img)) %>% 
  select(one_of(vars)) %>% 
  left_join(meta_pdf) %>% 
  select(id = id, name, group, everything()) %>% 
  arrange(group,name)

## Prepare images 600 x 450 and thumbnails
limgs <- meta %>% select(img, original_img) %>% transpose()
ii <- 1

library(imager)
lapply(limgs,function(i){
  #i <- limgs[[1]]
  img <- load.image(paste0("inst/drawings/", i$original_img))
  #plot(img)
  im0 <- imsub(img, y < height(img)*0.75)
  im <- resize(im0,600,450)
  output_file <- paste0("inst/imgs/", i$img)
  save.image(im, output_file)
  im <- resize(im0,200,150)
  output_file <- paste0("inst/thumbnails/", i$img)
  save.image(im, output_file)
})


# library(magick)
# map(limgs, function(i){
#   if(ii%% 20 == 0) message(ii)
#   ii <<- ii + 1
#   #i <- limgs[[1]]
#   img_ori <- image_read(paste0("inst/drawings/", i$original_img))
#   img_info <- image_info(img_ori)
#   w <- img_info$width
#   h <- img_info$height
#   newh <- round(h*0.75)
#   crop <- paste0(h,"x",newh)
#   img <- image_crop(img_ori, crop)
#   img_out <- image_scale(img,paste0(h*.298,"x",newh*.298,"!"))
#   image_write(img_out, paste0("inst/imgs/", i$img))
#   image_write(image_scale(img,paste0(h*.298/6,"x",newh*.298/6,"!")), paste0("inst/thumbnails/", i$img))
# })


# Save Meta

meta <- meta %>% 
  select(-original_img, -numid)
mop::copy_clipboard(meta)
jsonlite::write_json(meta,"inst/pseudoviz-meta.json")
write_csv(meta,"inst/pseudoviz-meta.csv")  

# Create ctypes

meta_ctypes <- meta %>% 
  select(id = id, ctypes) %>% 
  slice_rows("id") %>% 
  by_slice(~strsplit(.$ctypes,"|", fixed = TRUE)[[1]],.to = "ctype") %>% 
  unnest()
write_csv(meta_ctypes,"inst/pseudoviz-meta-ctypes.csv")  



#### 

# https://spreadsheets.google.com/feeds/list/1KGOHJ7isodE8c6DilX4z-BpA-SrU9DSiREAIOBGMeKA/od6/public/values?alt=json
# https://cors.io/?http://spreadsheets.google.com/feeds/list/1KGOHJ7isodE8c6DilX4z-BpA-SrU9DSiREAIOBGMeKA/od6/public/values?alt=json

#http://chriszarate.github.io/sheetrock/
# x <- jsonlite::fromJSON("https://cors.io/?http://spreadsheets.google.com/feeds/list/1KGOHJ7isodE8c6DilX4z-BpA-SrU9DSiREAIOBGMeKA/od6/public/values?alt=json")
# xx <- x$feed$entry
# xx <- xx %>% select(contains("$"))

