
#' @export
viz_which_fun <- function(x, package, family = NULL){
  viz <- viz_which(x)
  viz_fun(package, frtype = viz$frtype, family = family)
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


