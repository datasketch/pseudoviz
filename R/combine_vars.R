

combine_vars <- function(dic, hdtable_types = NULL){
  
  n_cols <- nrow(dic)
  
  vars1 <- NULL
  vars2 <- NULL
  vars3 <- NULL
  
  hdType_V1 <- NULL
  hdType_V2 <- NULL
  hdType_V3 <- NULL
  
  # 1 Var
  
  vars_1 <- make_combinations(dic$id, 1, colname = "id_")
  hdtypes_1 <- make_combinations(dic$hdtype, 1, colname = "hdType_")
  vars1 <- vars_1 |>
    dplyr::left_join(hdtypes_1, by = "comb_idx") |>
    dplyr::mutate(n_vars = 1)
  # 2 Vars
  if (nrow(dic) > 1) {
    vars_2 <- make_combinations(dic$id, 2, colname = "id_")
    hdtypes_2 <- make_combinations(dic$hdtype, 2, colname = "hdType_")
    vars2 <- vars_2 |> dplyr::left_join(hdtypes_2, by = "comb_idx") |>
      dplyr::mutate(n_vars = 2)
    # 3 Vars
    if (nrow(dic) > 2) {
      vars_3 <- make_combinations(dic$id, 3, colname = "id_")
      hdtypes_3 <- make_combinations(dic$hdtype, 3, colname = "hdType_")
      vars3 <- vars_3 |> dplyr::left_join(hdtypes_3, by = "comb_idx") |>
        dplyr::mutate(n_vars = 3)
    }
  }
  vars <- dplyr::bind_rows(list(vars1,vars2,vars3)) |>
    dplyr::select(comb_idx, n_vars,
                  dplyr::starts_with("id"),
                  dplyr::starts_with("hd"))
  if(!"id_V2" %in% names(vars)){
    vars$id_V2 <- NA
  }
  if(!"hdType_V2" %in% names(vars)){
    vars$hdType_V2 <- NA
  }
  if(!"id_V3" %in% names(vars)){
    vars$id_V3 <- NA
  }
  if(!"hdType_V3" %in% names(vars)){
    vars$hdType_V3 <- NA
  }
  
  paste_sort <- function(x, collapse = "-"){
    paste0(sort(x),collapse = collapse)
  }
  
  vars <- vars |>
    dplyr::rowwise() |> 
    dplyr::mutate(
      hdtable_type = paste_sort(dplyr::c_across(hdType_V1:hdType_V3))) |> 
    dplyr::ungroup()
  
  
  if(!is.null(hdtable_types)){
    vars <- vars |>
      dplyr::filter(hdtable_type %in% hdtable_types)
  }
  
  vars
  
}




make_combinations <- function(v, m = 1, colname = NULL){
  # v <- "first"
  combs <- combn(1:length(v),m)
  idx <- as.vector(combs)
  idx <- purrr::map_chr(idx, ~ v[.])
  x <- t(matrix(idx, nrow = m))
  colnames(x) <- paste0("V", 1:ncol(x))
  x |>
    #tibble::as_tibble(.name_repair = "minimal") |>
    tibble::as_tibble() |>
    dplyr::rename_with(.fn = ~ paste0(colname, .x)) |>
    dplyr::mutate(comb_idx = paste0("m",m,"_", dplyr::row_number()))
}






