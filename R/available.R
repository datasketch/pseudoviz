

#' @export
availablePseudoviz <- function(){
  meta <- system.file("meta.yaml",package="pseudoviz", mustWork=TRUE)
  l <- yaml.load_file(meta)
  ids <- list.mapv(l, id)
  #ftypes <- list.map(l, fringes[[1]]$ftypes)
  #names(ftypes) <- ids
  #d <- melt(ftypes)
  #list.stack(ftypes)
  ids
}

#' @export
pseudovizFtypes <- function(){
  meta <- system.file("meta.yaml",package="pseudoviz", mustWork=TRUE)
  l <- yaml.load_file(meta)
  ids <- list.mapv(l, id)
  ftypes <- list.map(l, fringes[[1]]$ftypes)
  names(ftypes) <- ids
  d <- melt(ftypes)
  names(d) <- c("ftype","pseudovizId")
  d
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
  pathTpl <- paste0(file.path(path,"{id}"),".png")
  imgTags <- lapply(ids,function(id){
    imgPath <- pystr_format(pathTpl,list(id=id))
    src <- knitr::image_uri(imgPath)
    img(src=src, width="100%", style="max-width:400px")
  })
  html_print(imgTags)
}
