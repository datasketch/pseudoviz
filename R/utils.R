

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


format_hdtable_type <- function(x){
  # extract last word
  x <- stringr::word(x,-1, sep = "_")
  # split every three
  x <- gsub("(.{3})", "\\1 ", x)
  x <- gsub(" $","", x)
  gsub(" ","-", x)
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


make_combinations <- function(v, m = 1, colname = NULL){
  combs <- combn(1:length(v),m)
  idx <- as.vector(combs)
  idx <- purrr::map_chr(idx, ~ v[.])
  t(matrix(idx, nrow = m)) |>
    tibble::as_tibble() |>
    dplyr::rename_with(.fn = ~ paste0(colname, .x)) |>
    dplyr::mutate(comb_idx = paste0("m",m,"_", dplyr::row_number()))
}





