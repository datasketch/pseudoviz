
#' @export
viz_fun_df <- function(package){
  # package <- "ggmagic"
  # package <- "hgchmagic"
  if(package == "ggmagic"){
    require("ggmagic")
    prefix <- "gg_"
  }
  if(package == "hgchmagic"){
    require("hgchmagic")
    prefix <- "hgch_"
  }
  viz_funs <- ls(paste0("package:", package), pattern = prefix)
  viz_funs_df <- tibble(viz_fun = viz_funs) %>% 
    mutate(family = extract_between_underscore(viz_fun)) %>% 
    filter(!is.na(family)) %>% 
    mutate(frtype = format_frtype(viz_fun))
  viz_funs_df
}

#' @export
viz_fun <- function(package, family = NULL, frtype = NULL, 
                    with_package = FALSE){
  df <- viz_fun_df(package)
  if(!is.null(family)){
    family_filter <- family
    df <- df %>% filter(family %in% family_filter)
  }
  if(!is.null(frtype)){
    frtype_filter <- frtype
    df <- df %>% filter(frtype %in% frtype_filter)
  }
  if(nrow(df) == 0) return()
  fun <- df$viz_fun
  if(with_package) 
    fun <- paste0(package,"::", fun)
  fun
}

