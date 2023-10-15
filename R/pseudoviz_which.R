
#' @export
pseudoviz_which_family <- function(hdtable_type){
  sort(unique(pseudoviz_which(hdtable_type)$vizfun_family))
}

#' @export
pseudoviz_which <- function(hdtable_type, family = NULL){

  types <- pseudoviz:::pseudoviz_hdtable_type |> 
    dplyr::filter(hdtable_types == hdtable_type)
  
  if(!is.null(family)){
    family_filter <- family
    types <- types |> 
      dplyr::filter(vizfun_family %in% family_filter)
  }
  
  types
  
  
}


