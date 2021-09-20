

library(atblcms)
library(tidyverse)


base_name <- "pseudoviz"
tables <- c("pseudoviz", "frtypes")

merge_records <- list(
  pseudoviz = list(
    list(table = "frtypes",
         by1 = "frtype",
         fields2 = c("frtype"))
  )
)

# filter_tables <- list(
#   apps = list(variable = "code", condition = "in",
#               value = c(""))
# )


atbl <- get_airtable_data(base_name = base_name, tables = tables, tpl_folder = NULL,
                          filter_tables = NULL,
                          merge_records = merge_records)

pseudoviz <- atbl$pseudoviz

pseudoviz_info <- pseudoviz %>% 
  select(slug = slug_en, title = title_en, image_url = image, family)


pseudoviz_frtype <- pseudoviz %>% 
  select(uid, family, frtype_list) %>%
  mutate(family = as.character(family)) %>% 
  filter(family != "NULL") %>% 
  unnest(cols = frtype_list) %>% 
  mutate(frtype = unlist(frtype_list)) %>% 
  select(-frtype_list)


usethis::use_data(pseudoviz_frtype, pseudoviz_info, overwrite = TRUE, internal = TRUE)
