
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

#' @export
viz_which_family <- function(x){
  sort(unique(viz_which(x)$family))
}

#' @export
viz_which <- function(x){
  # if("character" %in% class(x)){
  #   if(length(x) == 1){
  #     ctypes <- strsplit(x,"-")[[1]]
  #   }else{
  #     ctypes <- x
  #   }
  # }
  if("data.frame" %in% class(x)){
    frtype_guess <- homodatum::guess_frType(x, as_string = TRUE)
  }
  if("fringe" %in% class(x)){
    frtype_guess <- as.character(x$frtype)
  }
  
  types <- pseudoviz:::pseudoviz_frtype %>% 
    dplyr::filter(frtype == frtype_guess)
  
  # if(!is.null(names(ctypes))){
  #   possible <- possibleNamedCtypes(ctypes)
  # }else{
  #   #possible <- possibleCtypes(ctypes, combine = TRUE, castable = TRUE)
  #   possible <- possibleCtypes(ctypes, combine = TRUE, castable = FALSE)
  #   names(possible) <- map(possible,paste,collapse="|")
  # }
  
  # ct <- pseudovizCtypes()
  # pids <- map(possible, function(p){
  #   p <- paste(p,collapse = "-")
  #   ct %>% filter(ctype %in% p) %>% pull(id)
  # }) 
  # pids %>% keep(~length(.) > 0)
  
  types
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


