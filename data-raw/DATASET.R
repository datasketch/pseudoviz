
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


####

# Get package functions
#packages <- c("ggmagic", "hcmagic")
packages <- c("hgwordcloud", "hgmagic")
require(ggmagic)
require(hgmagic)
require(hgwordcloud)
#prefixes <- list(ggmagic = "gg_", hcmagic = "hg_")
prefixes <- list(hgwordcloud = "hg_", hgmagic = "hg_")
vizfuns <- purrr::map(packages, 
                      ~ tibble::tibble(vizfun = ls(paste0("package:", .), 
                                                   pattern = prefixes[[.]])))
names(vizfuns) <- packages

has_any_hdtype <- function(hdtable_types){
  basic_hdtypes <- c("Cat", "Dat", "Num", "Yea")
  #hdtable_types <- c("CatDat", "ca_te")
  #hdtable_types <- "CatYea"
  #hdtable_types <- "color"
  purrr::map_lgl(hdtable_types, function(hdtable_type){
    any(purrr::map_lgl(basic_hdtypes, ~ any(grepl(., hdtable_type))))
  })
}

format_hdtable_type <- function(x){
  # split every three
  x <- gsub("(.{3})", "\\1 ", x)
  x <- gsub(" $","", x)
  gsub(" ","-", x)
}


df <- dplyr::bind_rows(vizfuns, .id = "package") |> 
  dplyr::mutate(vizfun_family = extract_between_underscore(vizfun)) |> 
  dplyr::filter(!is.na(vizfun)) |> 
  dplyr::mutate(hdtable_type = stringr::word(vizfun, -1, sep = "_")) |> 
  dplyr::mutate(is_hdtype = has_any_hdtype(hdtable_type)) |> 
  dplyr::mutate(hdtable_type = format_hdtable_type(hdtable_type)) |> 
  dplyr::filter(is_hdtype) |> 
  dplyr::select(-is_hdtype)
package_vizfuns <- df


# save func availables in hgmagic
# library(hgmagic)
# function_hg <- setdiff(
#   grep("^[^_]*_[^_]*_[^_]*$", ls("package:hgmagic"), value = T),
#   c("hc_add_legend", "hc_add_options", "hc_add_tooltip", "hc_data_series" )
# )
# 
# hg_df <- tibble(
#   package = rep("hgmagic", length(function_hg)), 
#   vizfun = function_hg,
#   vizfun_family = sub("^.*?_(.*?)_.*$", "\\1", function_hg), 
#   hdtable_type = sub("^-", "",
#                      gsub("([A-Z])", "-\\1", 
#                           sub("^.*?_.*?_(.*)$", "\\1", function_hg)
#                      )
#   )
# )
# 
# package_vizfuns <- package_vizfuns |> bind_rows(hg_df)

#### SAVE

usethis::use_data(pseudoviz_hdtable_type, pseudoviz_info, 
                  package_vizfuns,
                  overwrite = TRUE, internal = TRUE)
