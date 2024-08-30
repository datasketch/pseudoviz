test_that("multiplication works", {
  dic <- create_dic(iris)
  dic$num_categories <- map(dic$id, ~dic$stats[[.]]$n_unique) |> unlist()
  recommend_visualizations(dic)
  
  
  my_dic <- data.frame(
    id = c("instancia_principal", "tipo_de_sentencia", "orientacion_sexual", "intersexual", "nombre", "ano", "descripcion", "titulo"),
    label = c("Instancia principal", "Tipo de sentencia", "Orientación sexual", "Intersexual", "Nombre", "Año", "Descripción", "Título"),
    hdtype = c("Cat", "Cat", "Cat", "Txt", "Gnm", "Dat", "Txt", "Cat"),
    num_categories = c(15, 30, 3, 300, NA, NA, NA, NA)
  )
  
  conf <- recommend_visualizations(my_dic)
  selector_choices_var(conf, "word_cloud")
  
  data <- sample_data("Cat-Cat-Cat-Cat-Cat-Cat-Cat")
  dic <- create_dic(data) 
  dic$num_categories <- map(dic$id, ~dic$stats[[.]]$n_unique) |> unlist()
  recommend_visualizations(dic)
  

  
  
})
