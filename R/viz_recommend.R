
#' @export
viz_recommend <- function(d = NULL, dic = NULL, package = "ggmagic"){

  # combinations of variables by name

  #d <- palmerpenguins::penguins
  if (is.null(dic)) {
    dic <- hdtable::create_dic(d)
  }

  #av_viz_frt <- c("Cat", "Dat", "Num") #available_viz_frtypes()
  #vars <- combine_vars(dic, frtypes = av_viz_frt)
  vars <- combine_vars(dic)
  
  
  fts_list <- vars |>
    dplyr::group_split(hdtable_type)
  fts_list_names <- vars |>
    dplyr::group_by(hdtable_type) |>
    dplyr::group_keys() |>
    dplyr::pull(hdtable_type)
  fts_recommend_viz <- fts_list |>
    purrr::map(function(x){
      #x <- fts_list[[1]]
      ft <- unique(x$hdtable_type)
      
      # Make sure vars are return in same order as hdtable_type 
      # Example: Num-Cat -> Cat-Num
      xx <- x |>   purrr::transpose()
      vars <- xx |> 
        purrr::map(function(r){
          #r <- xx[[1]]
          t <- tibble::enframe(unlist(r))
          id <- t |> dplyr::filter(grepl("id_V", name)) |> dplyr::pull(value)
          hdtype <- t |> dplyr::filter(grepl("hdType_V", name)) |> dplyr::pull(value)
          tibble::tibble(id = id, hdtype = hdtype) |> 
            dplyr::arrange(hdtype) |> 
            dplyr::filter(!is.na(hdtype)) |> 
            dplyr::pull(id)
        })

      vizfamily <- package_vizfun_family(package, hdtable_type = ft)
      vizfun <- package_vizfun(package, hdtable_type = ft)
      list(hdtable_type = ft, vars = vars, 
           vizfamily = vizfamily,
           vizfun = vizfun)
    })
  names(fts_recommend_viz) <- fts_list_names
  fts_recommend_viz

}

# sample_dic_frGroup <- function(dic, frgroup, as_named_hdTypes = FALSE){
#   # frgroup <- "Cat2-Num2"
#   # frgroup <- "Num2-Cat2"
#   hdtypes <- homodatum::expand_frGroup(frgroup)
# 
#   sample_dic <- dic |>
#     dplyr::arrange(hdType) |>
#     dplyr::filter(hdType %in% hdtypes) |>
#     dplyr::group_by(hdType) |>
#     dplyr::mutate(idx = dplyr::row_number()) |>
#     dplyr::filter(idx <= 2) |>
#     dplyr::select(-idx)
# 
#   if(as_named_hdTypes){
#     return(homodatum::named_hdTypes(sample_dic))
#   }
#   sample_dic
# }

# available_viz_frtypes <- function(){
#   unique(dscharts::vizfuns$frtype)
# }

# which_family <- function(frt){
#   pseudoviz::available_package_vizfuns() |>
#     dplyr::filter(hdtable_type == frt) |>
#     dplyr::pull(vizfun_family) |>
#     unique()
# }


