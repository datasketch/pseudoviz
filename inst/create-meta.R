
library(tidyverse)
library(pdftools)
library(purrrlyr)

txt <- pdf_text("inst/2017-06-29/Pseudo Viz.pdf")

pvMeta <- data_frame(txt = txt, 
                     numid = 1:length(txt),
                     original_img = paste0("Pseudo Viz - ",1:length(txt),".png"))
pvMeta <- pvMeta %>% separate(txt, c("name","code"), sep = "\n")
#mashups <- match("Mashups",pvMeta[[1]])-1
#pvMeta <- pvMeta[1:mashups,]
meta_pdf <- pvMeta %>% mutate(group = is.na(code)) %>%
  mutate(group = ifelse(group,name,NA)) %>% 
  tidyr::fill(group,.direction="down") %>%
  mutate(code = gsub(" ","_",code)) %>% 
  filter(!is.na(code)) %>% 
  mutate(img = paste0("pseudoviz-",sprintf("%03d",numid),".png"))
write_csv(meta_pdf,"inst/pseudoviz-meta-pdf.csv")

meta_pdf <- read_csv("inst/pseudoviz-meta-pdf.csv")

pvCodes <- unique(meta_pdf$code)

## Read manual meta from googlesheets

meta_manual <- read_csv("inst/pseudoviz-meta-manual.csv")

vars <- c("code","img_url","data_restrictions","params","comments","ctypes")
github_raw_img <- "https://raw.githubusercontent.com/jpmarindiaz/pseudoviz/master/inst/imgs/"

meta <- meta_manual %>% 
  mutate(img_url = paste0(github_raw_img,img)) %>% 
  select(one_of(vars)) %>% 
  left_join(meta_pdf) %>% 
  select(id = code, name, group, everything())
write_csv(meta,"inst/pseudoviz-meta.csv")  

meta_ctypes <- meta_manual %>% 
  select(id = code, ctypes) %>% 
  filter(id %in% pvCodes) %>% 
  slice_rows("id") %>% 
  by_slice(~strsplit(.$ctypes,"|", fixed = TRUE)[[1]],.to = "ctype") %>% 
  unnest()
write_csv(meta_ctypes,"inst/pseudoviz-meta-ctypes.csv")  
