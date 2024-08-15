test_that("multiplication works", {
  dic <- create_dic(iris)
  dic$num_categories <- map(dic$id, ~dic$stats[[.]]$n_unique) |> unlist()
  recommend_visualizations(dic)
  
  
  my_dic <- data.frame(
    id = c("instancia_principal", "tipo_de_sentencia", "orientacion_sexual", "intersexual", "nombre", "ano", "descripcion", "titulo"),
    label = c("Instancia principal", "Tipo de sentencia", "Orientación sexual", "Intersexual", "Nombre", "Año", "Descripción", "Título"),
    hdtype = c("Cat", "Cat", "Cat", "Cat", "Gnm", "Dat", "Num", "Num"),
    num_categories = c(15, 3, 3, 2, NA, NA, NA, NA)
  )
  
  conf <- recommend_visualizations(my_dic)
  selector_choices_var(conf, "bar")
  
})
