

#' @export
package_vizfun <- function(package = NULL, 
                           data = NULL, hdtable_type = NULL,
                           family = NULL, 
                           with_package = FALSE){
  df <- available_package_vizfuns(package = package, 
                          data = data, hdtable_type = hdtable_type,
                          family = family)
  if(nrow(df) == 0) return()
  fun <- df$vizfun
  if(with_package) 
    fun <- paste0(package,"::", fun)
  fun
}

#' @export
package_vizfun_family <- function(package = NULL, 
                                  data = NULL, hdtable_type = NULL,
                                  family = NULL){

  df <- available_package_vizfuns(package = package, 
                                  data = data, hdtable_type = hdtable_type,
                                  family = family)
  
  if(nrow(df) == 0) return()
  fun_family <- unique(df$vizfun_family)
  fun_family
}




#' @export
available_package_vizfuns <- function(package = NULL, 
                                      data = NULL, hdtable_type = NULL,
                                      family = NULL){
  
  # Get package functions
  packages <- c("ggmagic", "hgchmagic")
  require(ggmagic)
  require(hgchmagic)
  prefixes <- list(ggmagic = "gg_", hgchmagic = "hgch_")
  
  vizfuns <- purrr::map(packages, 
                        ~ tibble::tibble(vizfun = ls(paste0("package:", .), 
                                      pattern = prefixes[[.]])))
  names(vizfuns) <- packages
  
  df <- dplyr::bind_rows(vizfuns, .id = "package") |> 
    dplyr::mutate(vizfun_family = extract_between_underscore(vizfun)) |> 
    dplyr::filter(!is.na(vizfun)) %>% 
    dplyr::mutate(hdtable_type = format_hdtable_type(vizfun)) 
  
  
  # Filter values
  
  if(!is.null(package)){
    package_filter <- package
    df <- df |>  dplyr::filter(package %in% package_filter)
  }
  
  # Get hdtable_type
  hdtable_type_data <- NULL
  if(!is.null(data)){
    hdtable_type_data <- hdtable_type_guess(data)
  }
  hdtable_type <- hdtable_type %||% hdtable_type_data
  
  ## Get package funs
  if(!is.null(family)){
    family_filter <- family
    df <- df |>  dplyr::filter(vizfun_family %in% family_filter)
  }
  
  if(!is.null(hdtable_type)){
    hdtable_type_filter <- hdtable_type
    df <- df |>  dplyr::filter(hdtable_type %in% hdtable_type_filter)
  }
  
  df
  
}
