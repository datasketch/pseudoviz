
## code to prepare `DATASET` dataset goes here

library(usethis)
library(dsairtable)
library(tidyverse)
library(hdtype)

at <- at_base(Sys.getenv("AIRTABLE_APP"))
at_table_list(at)

viztypes <- at_table(at, "viztypes", linked = TRUE)
#hdtable_types <- at_table(at, "hdtable_type")

pseudoviz <- viztypes |> 
  select(uid = uid, slug = slug_en, title = title_en,  image_url = image,
         hdtable_types,
         vizfun_family)
pseudoviz_info <- pseudoviz


pseudoviz_hdtable_type <- pseudoviz |> 
  select(slug, hdtable_types, vizfun_family) |> 
  mutate(vizfun_family = unlist(vizfun_family)) |> 
  unnest(c(hdtable_types))


# pseudoviz_frtype <- pseudoviz %>% 
#   select(uid, family, frtype_list) %>%
#   mutate(family = as.character(family)) %>% 
#   filter(family != "NULL") %>% 
#   unnest(cols = frtype_list) %>% 
#   mutate(frtype = unlist(frtype_list)) %>% 
#   select(-frtype_list)


usethis::use_data(pseudoviz_hdtable_type, pseudoviz_info, 
                  overwrite = TRUE, internal = TRUE)
