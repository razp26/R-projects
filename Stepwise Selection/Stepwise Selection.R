# Proyecto desarrollado para el curso de Econometría de la Maestría en Data Science
# Universidad Galileo
# Rodolfo Zea
# Julio 2019

library (MASS)
library (dplyr)

# Función que contiene el algoritmo de forward_stepwise_selection
forward_stepwise_selection <- function(dataset, yname) {
  
  # Obtenemos los nombres de las features
  col_names <- names(dataset)
  features_disponibles <- setdiff(col_names,yname)
  features_seleccionadas <- c()
  p <- length(features_disponibles)
  lista_se_global <- c()
  
  while (length(features_disponibles) > 0) {
    
    n_features_disponibles <- length(features_disponibles)
    lista_se_iteracion <- c()
    
    for (i in 1:(n_features_disponibles)) {
      
      features_iteracion <- c(features_seleccionadas, features_disponibles[i])
      
      # Creamos la formula para los features de la iteracion
      formula_x <- paste0(features_iteracion, collapse = "+")
      formula_x <- paste0(yname, "~", formula_x, collapse = "")
      
      # Evaluamos el modelo a partir de la formula
      fit <- lm(formula=formula_x, data=dataset)
      test_perd <- predict(fit, dataset)
      
      # Calculamos el error cuadrado
      se <- sum((test_perd - dataset[[yname]])^2)
      
      # Agregamos el error cuadrado del modelo a nuestra lista de errores cuadrados
      lista_se_iteracion <- c(lista_se_iteracion, se)
      
    }
    
    indice_error_minimo_iteracion <- which.min(lista_se_iteracion)
    features_seleccionadas <- c(features_seleccionadas, features_disponibles[indice_error_minimo_iteracion])
    features_disponibles <- setdiff(features_disponibles, features_disponibles[indice_error_minimo_iteracion])
    lista_se_global <- c(lista_se_global, lista_se_iteracion[indice_error_minimo_iteracion])
    
    print('Features: ')
    print(features_seleccionadas)
    print('Error minimo: ')
    print(lista_se_iteracion[indice_error_minimo_iteracion])
  }
  
  indice_error_minimo_global <- which.min(lista_se_global)
  return (features_seleccionadas[1:indice_error_minimo_global])
  
}



# Función que contiene el algoritmo de backward_stepwise_selection
backward_stepwise_selection <- function(dataset, yname) {
  
  # Obtenemos los nombres de las features
  col_names <- names(dataset)
  features_seleccionadas <- setdiff(col_names,yname)
  n_features_seleccionadas = length(features_seleccionadas)
  lista_se_global <- c()
  modelos <- list()
  
  # Iniciamos obteniendo la información cuando se utilizan todos los features
  # Creamos la formula para los features de la iteracion
  formula_x <- paste0(features_seleccionadas, collapse = "+")
  formula_x <- paste0(yname, "~", formula_x, collapse = "")
  
  # Evaluamos el modelo a partir de la formula
  fit <- lm(formula=formula_x, data=dataset)
  test_perd <- predict(fit, dataset)
  
  # Calculamos el error cuadrado
  se <- sum((test_perd - dataset[[yname]])^2)
  
  # Agregamos el modelo que incluye todas las features así como su error
  lista_se_global <- c(lista_se_global, se)
  modelos[[length(lista_se_global)]] <- features_seleccionadas
  n_features_seleccionadas = length(features_seleccionadas)
  
  print('Features: ')
  print(features_seleccionadas)
  print('Error minimo: ')
  print(se)
  
  # Creamos un ciclo a partir del cual vamos eliminando un feature a la vez del modelo
  while(n_features_seleccionadas > 1) {
    
    lista_se_iteracion <- c()
    
    for (j in 1:n_features_seleccionadas) {
      
      features_iteracion <- setdiff(features_seleccionadas, features_seleccionadas[j])
      
      # Creamos la formula para los features de la iteracion
      formula_x <- paste0(features_iteracion, collapse = "+")
      formula_x <- paste0(yname, "~", formula_x, collapse = "")
      
      # Evaluamos el modelo a partir de la formula
      fit <- lm(formula=formula_x, data=dataset)
      test_perd <- predict(fit, dataset)
      
      # Calculamos el error cuadrado
      se <- sum((test_perd - dataset[[yname]])^2)
      
      # Agregamos el error cuadrado del modelo a nuestra lista de errores cuadrados
      lista_se_iteracion <- c(lista_se_iteracion, se)
    }
    
    indice_error_minimo_iteracion <- which.min(lista_se_iteracion)
    features_seleccionadas <- setdiff(features_seleccionadas, features_seleccionadas[indice_error_minimo_iteracion])
    lista_se_global <- c(lista_se_global, lista_se_iteracion[indice_error_minimo_iteracion])
    modelos[[length(lista_se_global)]] <- features_seleccionadas
    n_features_seleccionadas = length(features_seleccionadas)
    
    print('Features: ')
    print(features_seleccionadas)
    print('Error minimo: ')
    print(lista_se_iteracion[indice_error_minimo_iteracion])
    
  }
  
  indice_error_minimo_global <- which.min(lista_se_global)
  return(modelos[[indice_error_minimo_global]])
  
}



# Definimos el dataset o la fuente de datos que utilizaremos 
dataset<-Boston
head(dataset)

# Obtenemos el numero de features que tiene el dataset
n_features<-ncol(Boston) - 1
n_features

# Definimos el nombre del feature, dentro del dataset, que tomaremos como Y
yname <- 'medv'

# Ejecutamos la función de forward_stepwise_selection
print('FORWARD STEPWISE SELECTION')
modelo_forward <- forward_stepwise_selection(dataset, yname)
print(modelo_forward)

# Ejecutamos la función de backward_stepwise_selection
print('BAKWARD STEPWISE SELECTION')
modelo_backward <- backward_stepwise_selection(dataset, yname)
print(modelo_backward)


