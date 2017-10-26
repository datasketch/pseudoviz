
#' @export
pseudovizMeta <- function(){
  read.csv(system.file("pseudoviz-meta.csv",package="pseudoviz", mustWork=TRUE),
           stringsAsFactors = FALSE)
}

#' @export
availablePseudoviz <- function(){
  meta <- pseudovizMeta()
  meta$id
}

#' @export
pseudovizCtypes <- function(){
  path <- system.file("pseudoviz-meta-ctypes.csv",package="pseudoviz", mustWork=TRUE)
  read.csv(path, stringsAsFactors = FALSE)
}

#' @export
whichPseudoviz <- function(x){
  if("character" %in% class(x)){
    if(length(x) == 1){
      ctypes <- strsplit(x,"-")[[1]]
    }else{
      ctypes <- x
    }
  }
  if("data.frame" %in% class(x)){
    ctypes <- guessCtypes(x,named = TRUE)
  }
  if(!is.null(names(ctypes))){
    possible <- possibleNamedCtypes(ctypes)
  }else{
    #possible <- possibleCtypes(ctypes, combine = TRUE, castable = TRUE)
    possible <- possibleCtypes(ctypes, combine = TRUE, castable = FALSE)
    names(possible) <- map(possible,paste,collapse="|")
  }
  
  ct <- pseudovizCtypes()
  pids <- map(possible, function(p){
    p <- paste(p,collapse = "-")
    ct %>% filter(ctype %in% p) %>% pull(id)
  }) 
  pids %>% keep(~length(.) > 0)
}


#' @export
printPseudoviz <- function(ids = NULL){
  avIds <- availablePseudoviz()
  if(is.null(ids))
    ids <-avIds
  else{
    if(!all(ids %in% avIds))
      stop("pseudoviz ids not found")
  }
  path <- system.file("imgs",package="pseudoviz", mustWork=TRUE)
  meta <- pseudovizMeta()
  ids <- meta %>% filter(id %in% ids)
  imgNames <- ids %>% pull(img)
  psNames <- ids %>% pull(name)
  imgPaths <- file.path(path,imgNames)
  imgTags <- lapply(1:length(psNames),function(i){
    img <- imgPaths[i]
    nm <- psNames[i]
    src <- knitr::image_uri(img)
    tagList(
      h3(nm),
      img(src=src, width="100%", style="max-width:400px")
      )
  })
  html_print(rev(imgTags))
}


