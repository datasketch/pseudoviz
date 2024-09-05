# Función principal para generar las recomendaciones
#' @export
recommend_visualizations <- function(dic) {
  config_path <- system.file("conf", "plot-conf.yml", package = "pseudoviz")
  config <- yaml::read_yaml(config_path)
  
  available_viz <- map(config$rules, function(viz_rules) {
    rule <- keep(viz_rules, ~ {
      if (is.null(.x$conditions)) return(FALSE)
      
      validate_viz_conditions(dic, .x)
    })
    
    if (length(rule) == 0) return()
    
    rule <- rule[[1]]
    generate_viz_structure(dic, rule)
  })
  

  available_viz <- compact(available_viz)
  list(available_viz = available_viz)
}


# Validar que el diccionario cumple con las condiciones para el gráfico
validate_viz_conditions <- function(dic, rule) {
  
  dic$hdtype[grepl("^id_|id|url", dic$id)] <- "Uid"
  

  cat_vars <- if (!is.null(rule$max_categories)) {
    nrow(dic[dic$hdtype %in% c("Cat", "Yea") & dic$num_categories <= rule$max_categories, ])
  } else {
    nrow(dic[dic$hdtype == "Cat", ])
  }
  
  txt_vars <- nrow(dic[dic$hdtype == "Txt", ])
  num_vars <- nrow(dic[dic$hdtype == "Num", ])
  dat_vars <- nrow(dic[dic$hdtype %in% c("Dat", "Yea"), ])
  

  if (!is.null(rule$`strict_conditon`)) {
    strict_cat_needed <- sum(rule$`strict_conditon` %in% c("Cat", "Yea"))
    strict_num_needed <- sum(rule$`strict_conditon` == "Num")
    strict_txt_needed <- sum(rule$`strict_conditon` == "Txt")
    strict_dat_needed <- sum(rule$`strict_conditon` %in% c("Dat", "Yea"))
    
    if (cat_vars < strict_cat_needed || 
        num_vars < strict_num_needed || 
        dat_vars < strict_dat_needed ||
        txt_vars < strict_txt_needed) {
      return(FALSE)
    }
  }
  
  valid_conditions <- map_lgl(rule$conditions, function(condition) {
    max_cat <- condition$`max-cat` %||% Inf
    max_num <- condition$`max-num` %||% Inf
    max_dat <- condition$`max-dat` %||% Inf
    max_txt <- condition$`max-txt` %||% Inf
    
    cat_vars <= max_cat && num_vars <= max_num && dat_vars <= max_dat && txt_vars <= max_txt
  })
  
  any(valid_conditions)
}


# Generar la estructura para un tipo de visualización específica
generate_viz_structure <- function(dic, rule) {
  dic$hdtype[grepl("^id_|id", dic$id)] <- "Uid"
  
  if (!is.null(rule$max_categories)) {
    cat_vars <- dic[dic$hdtype %in% c("Cat", "Yea") & dic$num_categories <= rule$max_categories, ]
  } else {
    cat_vars <- dic[dic$hdtype == "Cat", ]
  }
  
  if (!is.null(rule$possible_names)) {
    if (nrow(dic) > 0) {
    cat_vars <- dic[dic$id %in% rule$possible_names,]
    }
  }
  
  num_vars <- dic[dic$hdtype == "Num", ]
  txt_vars <- dic[dic$hdtype == "Txt", ]
  dat_vars <- dic[dic$hdtype %in% c("Dat", "Yea"), ]
  
  default_vars <- select_default_vars(cat_vars, num_vars, dat_vars, txt_vars, rule)
  
  viz_structure <- list(
    `max-var` = rule$`max-var`,
    `default-vars` = default_vars
  )

  if ("Cat" %in% rule$`default-var-posibilities` && nrow(cat_vars) > 0) {
    viz_structure$`cat-var` <- lapply(seq_len(nrow(cat_vars)), function(i) {
      if (!is.na(cat_vars$id[i])) {
        list(id = cat_vars$id[i], label = cat_vars$label[i])
      } else {
        NULL
      }
    })
    viz_structure$`cat-var` <- Filter(Negate(is.null), viz_structure$`cat-var`)
  } else {
    viz_structure$`cat-var` <- NULL
  }
  
  if ("Num" %in% rule$`default-var-posibilities` && nrow(num_vars) > 0) {
    viz_structure$`num-var` <- lapply(seq_len(nrow(num_vars)), function(i) {
      if (!is.na(num_vars$id[i])) {
        list(id = num_vars$id[i], label = num_vars$label[i])
      } else {
        NULL
      }
    })
    viz_structure$`num-var` <- Filter(Negate(is.null), viz_structure$`num-var`)
  } else {
    viz_structure$`num-var` <- NULL
  }
  
  if ("Txt" %in% rule$`default-var-posibilities` && nrow(txt_vars) > 0) {
    viz_structure$`txt-var` <- lapply(seq_len(nrow(txt_vars)), function(i) {
      if (!is.na(txt_vars$id[i])) {
        list(id = txt_vars$id[i], label = txt_vars$label[i])
      } else {
        NULL
      }
    })
    viz_structure$`txt-var` <- Filter(Negate(is.null), viz_structure$`txt-var`)
  } else {
    viz_structure$`txt-var` <- NULL
  }
  
  if ("Dat" %in% rule$`default-var-posibilities` && nrow(dat_vars) > 0) {
    viz_structure$`dat-var` <- lapply(seq_len(nrow(dat_vars)), function(i) {
      if (!is.na(dat_vars$id[i])) {
        list(id = dat_vars$id[i], label = dat_vars$label[i])
      } else {
        NULL
      }
    })
    viz_structure$`dat-var` <- Filter(Negate(is.null), viz_structure$`dat-var`)
  } else {
    viz_structure$`dat-var` <- NULL
  }
  
  viz_structure
}


# Seleccionar las variables por defecto según las reglas
select_default_vars <- function(cat_vars, num_vars, dat_vars, txt_vars, rule) {
  

  if (!is.null(rule$max_categories)) {
    cat_vars <- cat_vars[cat_vars$num_categories <= rule$max_categories, ]
  }
  
  num_cat_needed <- sum(rule$`default-var-posibilities` %in% c("Cat", "Yea"))
  num_num_needed <- sum(rule$`default-var-posibilities` == "Num")
  num_txt_needed <- sum(rule$`default-var-posibilities` == "Txt")
  num_dat_needed <- sum(rule$`default-var-posibilities` %in% c("Dat", "Yea"))
  
  strict_cat_needed <- if (!is.null(rule$`strict_conditon`)) sum(rule$`strict_conditon` %in% c("Cat", "Yea")) else 0
  strict_num_needed <- if (!is.null(rule$`strict_conditon`)) sum(rule$`strict_conditon` == "Num") else 0
  strict_txt_needed <- if (!is.null(rule$`strict_conditon`)) sum(rule$`strict_conditon` == "Txt") else 0
  #strict_dat_needed <- if (!is.null(rule$`strict_conditon`)) sum(rule$`strict_conditon` %in% c("Dat", "Yea")) else 0
  

  if (strict_cat_needed > 0 && nrow(cat_vars) < strict_cat_needed) return(NULL)
  if (strict_num_needed > 0 && nrow(num_vars) < strict_num_needed) return(NULL)
  if (strict_txt_needed > 0 && nrow(txt_vars) < strict_txt_needed) return(NULL)

  default_cat_vars <- if (num_cat_needed > 0 && nrow(cat_vars) > 0) {
    cat_vars$id[seq_len(min(num_cat_needed, nrow(cat_vars)))]
  } else {
    NULL
  }
  
  default_num_vars <- if (num_num_needed > 0 && nrow(num_vars) > 0) {
    num_vars$id[seq_len(min(num_num_needed, nrow(num_vars)))]
  } else {
    NULL
  }
  
  default_txt_vars <- if (num_txt_needed > 0 && nrow(txt_vars) > 0) {
    txt_vars$id[seq_len(min(num_txt_needed, nrow(txt_vars)))]
  } else {
    NULL
  }
  
  default_dat_vars <- if (num_dat_needed > 0 && nrow(dat_vars) > 0) {
    dat_vars$id[seq_len(min(num_dat_needed, nrow(dat_vars)))]
  } else {
    NULL
  }
  
  # Combinar las variables seleccionadas
  default_vars <- c(default_cat_vars, default_num_vars, default_txt_vars, default_dat_vars)
  
  # Si no se seleccionaron suficientes variables y existen variables fijas, añadirlas
  if (length(default_vars) < length(rule$`default-var-posibilities`)) {
    num_cat_fixed_needed <- sum(rule$`default-var-fixed` %in% c("Cat", "Yea")) - length(default_cat_vars)
    num_num_fixed_needed <- sum(rule$`default-var-fixed` == "Num") - length(default_num_vars)
    num_txt_fixed_needed <- sum(rule$`default-var-fixed` == "Txt") - length(default_txt_vars)
    num_dat_fixed_needed <- sum(rule$`default-var-fixed` %in% c("Dat", "Yea")) - length(default_dat_vars)
    
    fixed_cat_vars <- if (num_cat_fixed_needed > 0 && nrow(cat_vars) > length(default_cat_vars)) {
      cat_vars$id[(length(default_cat_vars) + 1):min(length(default_cat_vars) + num_cat_fixed_needed, nrow(cat_vars))]
    } else {
      NULL
    }
    
    fixed_num_vars <- if (num_num_fixed_needed > 0 && nrow(num_vars) > length(default_num_vars)) {
      num_vars$id[(length(default_num_vars) + 1):min(length(default_num_vars) + num_num_fixed_needed, nrow(num_vars))]
    } else {
      NULL
    }
    
    fixed_txt_vars <- if (num_txt_fixed_needed > 0 && nrow(txt_vars) > length(default_txt_vars)) {
      txt_vars$id[(length(default_txt_vars) + 1):min(length(default_txt_vars) + num_txt_fixed_needed, nrow(txt_vars))]
    } else {
      NULL
    }
    
    fixed_dat_vars <- if (num_dat_fixed_needed > 0 && nrow(dat_vars) > length(default_dat_vars)) {
      dat_vars$id[(length(default_dat_vars) + 1):min(length(default_dat_vars) + num_dat_fixed_needed, nrow(dat_vars))]
    } else {
      NULL
    }
    
    default_vars <- c(default_vars, fixed_cat_vars, fixed_num_vars, fixed_txt_vars, fixed_dat_vars)
  }
  

  if (length(default_vars) > 0) {
    return(default_vars)
  } else {
    return()
  }
}
