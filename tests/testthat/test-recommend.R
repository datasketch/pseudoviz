test_that("multiplication works", {
  
  library(hdtb)
  d <- iris
  dic_hdtinfer <-  hdtinfer::hdtinfer(d) |> rename(label = id)
  d <- hdtb::hdtable(d)
  data <- d$data
  dic <- dic_hdtinfer |> bind_cols(tibble(id = names(d$data)))
  names(recommend_visualizations(dic)$available_viz)
  
  data <- sample_data("Cat-Yea-Num")
  dic <-  hdtinfer::hdtinfer(data) 
  dic$label <- dic$id
  names(recommend_visualizations(dic)$available_viz)
  conf <- recommend_visualizations(dic)
  selector_choices_var(conf, "line")
  
  
  
  my_dic <- data.frame(
    id = c("instancia_principal", "tipo_de_sentencia", "orientacion_sexual", "intersexual", "nombre", "ano", "descripcion", "titulo"),
    label = c("Instancia principal", "Tipo de sentencia", "Orientación sexual", "Intersexual", "Nombre", "Año", "Descripción", "Título"),
    hdt = c("Cat", "Cat", "Cat", "Txt", "Gnm", "Dat", "Txt", "Cat")
  )
  
  conf <- recommend_visualizations(my_dic)
  selector_choices_var(conf, "word_cloud")
  
  data <- sample_data("Cat-Cat-Cat-Cat-Cat-Cat-Cat")
  dic <- create_dic(data) 
  dic$num_categories <- map(dic$id, ~dic$stats[[.]]$n_unique) |> unlist()
  recommend_visualizations(dic)
  
  
  
  
})