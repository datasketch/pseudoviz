
# Función principal para generar las recomendaciones
recommend_visualizations <- function(dic) {
  config <- yaml::read_yaml("inst/conf/plot-conf.yml")
  
  available_viz <- map(config$rules, function(viz_rules) {
    rule <- keep(viz_rules, ~ validate_viz_conditions(dic, .x$conditions)) |> 
      first()
  if (is.null(rule)) return()  
    generate_viz_structure(dic, rule)
  })
  
  available_viz <- compact(available_viz)
  available_viz

}

# Validar que el diccionario cumple con las condiciones para el gráfico
validate_viz_conditions <- function(dic, conditions) {
  cat_vars <- nrow(dic[dic$hdtype == "Cat", ])
  num_vars <- nrow(dic[dic$hdtype == "Num", ])
  dat_vars <- nrow(dic[dic$hdtype == "Dat", ])
  
  valid_conditions <- map_lgl(conditions, function(condition) {
    min_cat <- condition$`min-cat` %||% 0
    min_num <- condition$`min-num` %||% 0
    min_dat <- condition$`min-dat` %||% 0
    
    cat_vars >= min_cat && num_vars >= min_num && dat_vars >= min_dat
  })
  
  any(valid_conditions)
}


# Generar la estructura para un tipo de visualización específica
generate_viz_structure <- function(dic, rule) {
  # Filtrar las variables según los tipos especificados en las reglas
  cat_vars <- dic[dic$hdtype == "Cat" & dic$num_categories <= rule$max_categories, ]
  num_vars <- dic[dic$hdtype == "Num", ]
  dat_vars <- dic[dic$hdtype == "Dat", ]
  
  # Seleccionar las variables por defecto
  default_vars <- select_default_vars(cat_vars, num_vars, dat_vars, rule)
  
  # Configurar la estructura de salida
  viz_structure <- list(
    `max-var` = rule$`max-var`,
    `default-vars` = default_vars
  )
  
  if (nrow(cat_vars) > 0) {
    viz_structure$`cat-var` <- lapply(seq_len(nrow(cat_vars)), function(i) {
      list(id = cat_vars$id[i], label = cat_vars$label[i])
    })
  }
  
  if (nrow(num_vars) > 0) {
    viz_structure$`num-var` <- lapply(seq_len(nrow(num_vars)), function(i) {
      list(id = num_vars$id[i], label = num_vars$label[i])
    })
  }
  
  if (nrow(dat_vars) > 0) {
    viz_structure$`dat-var` <- lapply(seq_len(nrow(dat_vars)), function(i) {
      list(id = dat_vars$id[i], label = dat_vars$label[i])
    })
  }
  
  viz_structure
}

# Seleccionar las variables por defecto según las reglas
select_default_vars <- function(cat_vars, num_vars, dat_vars, rule) {
  # Determinar la cantidad de variables categóricas y numéricas que se necesitan
  num_cat_needed <- sum(rule$`default-var-posibilities` == "Cat")
  num_num_needed <- sum(rule$`default-var-posibilities` == "Num")
  num_dat_needed <- sum(rule$`default-var-posibilities` == "Dat")
  
  # Seleccionar las variables categóricas necesarias
  default_cat_vars <- if (num_cat_needed > 0 && nrow(cat_vars) > 0) {
    cat_vars$id[seq_len(min(num_cat_needed, nrow(cat_vars)))]
  } else {
    NULL
  }
  
  # Seleccionar las variables numéricas necesarias
  default_num_vars <- if (num_num_needed > 0 && nrow(num_vars) > 0) {
    num_vars$id[seq_len(min(num_num_needed, nrow(num_vars)))]
  } else {
    NULL
  }
  
  default_dat_vars <- if (num_dat_needed > 0 && nrow(dat_vars) > 0) {
    dat_vars$id[seq_len(min(num_dat_needed, nrow(dat_vars)))]
  } else {
    NULL
  }
  
  # Combinar las variables seleccionadas
  default_vars <- c(default_cat_vars, default_num_vars, default_dat_vars)
  
  # Si no se seleccionaron suficientes variables, usar las variables fijas
  if (length(default_vars) < length(rule$`default-var-posibilities`)) {
    num_cat_fixed_needed <- sum(rule$`default-var-fixed` == "Cat") - length(default_cat_vars)
    num_num_fixed_needed <- sum(rule$`default-var-fixed` == "Num") - length(default_num_vars)
    num_dat_fixed_needed <- sum(rule$`default-var-fixed` == "Dat") - length(default_dat_vars)
    
    fixed_cat_vars <- if (num_cat_fixed_needed > 0 && nrow(cat_vars) > length(default_cat_vars)) {
      cat_vars$id[(length(default_cat_vars) + 1):min(length(default_cat_vars) + num_cat_fixed_needed, nrow(cat_vars))]
    } else {
      return()
    }
    
    fixed_num_vars <- if (num_num_fixed_needed > 0 && nrow(num_vars) > length(default_num_vars)) {
      num_vars$id[(length(default_num_vars) + 1):min(length(default_num_vars) + num_num_fixed_needed, nrow(num_vars))]
    } else {
      return()
    }
    
    fixed_dat_vars <- if (num_dat_fixed_needed > 0 && nrow(dat_vars) > length(default_dat_vars)) {
      dat_vars$id[(length(default_dat_vars) + 1):min(length(default_dat_vars) + num_dat_fixed_needed, nrow(dat_vars))]
    } else {
      return()
    }
    
    
    default_vars <- c(default_vars, fixed_cat_vars, fixed_num_vars, fixed_dat_vars)
  }
  
  # Retornar las variables seleccionadas
  default_vars
}