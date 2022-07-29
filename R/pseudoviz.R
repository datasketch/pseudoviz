
#' @export
viz_meta <- function(){
  pseudoviz::pseudoviz_info
}

#' @export
viz_available <- function(){
  meta <- pseudovizMeta()
  meta$id
}

#' @export
viz_frtypes <- function(){
  pseudoviz:::pseudoviz_frtype
}

#' #' @export
#' printPseudoviz <- function(ids = NULL){
#'   avIds <- availablePseudoviz()
#'   if(is.null(ids))
#'     ids <-avIds
#'   else{
#'     if(!all(ids %in% avIds))
#'       stop("pseudoviz ids not found")
#'   }
#'   path <- system.file("imgs",package="pseudoviz", mustWork=TRUE)
#'   meta <- pseudovizMeta()
#'   ids <- meta %>% filter(id %in% ids)
#'   imgNames <- ids %>% pull(img)
#'   psNames <- ids %>% pull(name)
#'   imgPaths <- file.path(path,imgNames)
#'   imgTags <- lapply(1:length(psNames),function(i){
#'     img <- imgPaths[i]
#'     nm <- psNames[i]
#'     src <- knitr::image_uri(img)
#'     tagList(
#'       h3(nm),
#'       img(src=src, width="100%", style="max-width:400px")
#'       )
#'   })
#'   html_print(rev(imgTags))
#' }


