
#' @export
whichPseudoviz <- function(d){
  pf <- pseudovizFtypes()
  possibleFtype <- guessFtype(d)
  #add parsable ftypes
  (pf %>% filter(ftype == possibleFtype))$pseudovizId
}

