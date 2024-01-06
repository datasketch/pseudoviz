

#' @export
hdtable_type_guess <- function(x){
  if("data.frame" %in% class(x)){
    hdtable_type_guess <- hdtable::guess_hdtable_type(x, as_string = TRUE)
  }
  if(hdtable::is_hdtable(x)){
    hdtable_type_guess <- hdtable::hdtable_hdtable_type(x)
  }
  hdtable_type_guess
}



extract_between_underscore <- function (s){
  pattern <- paste0("(?<=", "_", ").*?(?=", "_", ")")
  stringr::str_extract(s, pattern)
}


file_path_sans_ext <- function (x)
{
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

file_ext <- function (x)
{
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}





