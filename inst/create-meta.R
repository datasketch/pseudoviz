
library(tidyverse)
library(pdftools)


txt <- pdf_text("inst/2017-06-29/Pseudo Viz.pdf")

pvMeta <- data_frame(txt = txt, 
                     numid = 1:length(txt),
                     original_img = paste0("Pseudo Viz - ",1:length(txt),".png"))
pvMeta <- pvMeta %>% separate(txt, c("name","code"), sep = "\n")
mashups <- match("Mashups",pvMeta[[1]])-1
pvMeta <- pvMeta[1:mashups,]
pv <- pvMeta %>% mutate(group = is.na(code)) %>%
  mutate(group = ifelse(group,name,NA)) %>% 
  tidyr::fill(group,.direction="down") %>%
  mutate(code = gsub(" ","_",code)) %>% 
  filter(!is.na(code)) %>% 
  mutate(img = paste0("pseudoviz-",sprintf("%03d",numid),".png"))
write_csv(pv,"inst/pseudoviz-meta.csv")
