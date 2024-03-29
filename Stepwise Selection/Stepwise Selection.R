# Proyecto desarrollado para el curso de Econometr�a de la Maestr�a en Data Science
# Universidad Galileo
# Rodolfo Zea
# Julio 2019

library (MASS)
library (dplyr)

# Funci�n que contiene el algoritmo de forward_stepwise_selection
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
      #test_perd <- predict(fit, dataset)
      
      # Calculamos el error cuadrado
      #se <- sum((test_perd - dataset[[yname]])^2)
      se <- summary(fit)$r.squared
      
      # Agregamos el error cuadrado del modelo a nuestra lista de errores cuadrados
      lista_se_iteracion <- c(lista_se_iteracion, se)
      
    }
    
    indice_r2_maximo_iteracion <- which.max(lista_se_iteracion)
    features_seleccionadas <- c(features_seleccionadas, features_disponibles[indice_r2_maximo_iteracion])
    features_disponibles <- setdiff(features_disponibles, features_disponibles[indice_r2_maximo_iteracion])
    lista_se_global <- c(lista_se_global, lista_se_iteracion[indice_r2_maximo_iteracion])
    
    print('Features: ')
    print(features_seleccionadas)
    print('R2: ')
    print(lista_se_iteracion[indice_r2_maximo_iteracion])
  }
  
  indice_r2_maximo_global <- which.max(lista_se_global)
  return (features_seleccionadas[1:indice_r2_maximo_global])
  
}



# Funci�n que contiene el algoritmo de backward_stepwise_selection
backward_stepwise_selection <- function(dataset, yname) {
  
  # Obtenemos los nombres de las features
  col_names <- names(dataset)
  features_seleccionadas <- setdiff(col_names,yname)
  n_features_seleccionadas = length(features_seleccionadas)
  lista_se_global <- c()
  modelos <- list()
  
  # Iniciamos obteniendo la informaci�n cuando se utilizan todos los features
  # Creamos la formula para los features de la iteracion
  formula_x <- paste0(features_seleccionadas, collapse = "+")
  formula_x <- paste0(yname, "~", formula_x, collapse = "")
  
  # Evaluamos el modelo a partir de la formula
  fit <- lm(formula=formula_x, data=dataset)
  #test_perd <- predict(fit, dataset)
  
  # Calculamos el error cuadrado
  #se <- sum((test_perd - dataset[[yname]])^2)
  se <- summary(fit)$r.squared
  
  # Agregamos el modelo que incluye todas las features as� como su R2
  lista_se_global <- c(lista_se_global, se)
  modelos[[length(lista_se_global)]] <- features_seleccionadas
  n_features_seleccionadas = length(features_seleccionadas)
  
  print('Features: ')
  print(features_seleccionadas)
  print('R2 maximo: ')
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
      #test_perd <- predict(fit, dataset)
      
      # Calculamos el error cuadrado
      #se <- sum((test_perd - dataset[[yname]])^2)
      se <- summary(fit)$r.squared
      
      # Agregamos el error cuadrado del modelo a nuestra lista de errores cuadrados
      lista_se_iteracion <- c(lista_se_iteracion, se)
    }
    
    indice_r2_maximo_iteracion <- which.max(lista_se_iteracion)
    features_seleccionadas <- setdiff(features_seleccionadas, features_seleccionadas[indice_r2_maximo_iteracion])
    lista_se_global <- c(lista_se_global, lista_se_iteracion[indice_r2_maximo_iteracion])
    modelos[[length(lista_se_global)]] <- features_seleccionadas
    n_features_seleccionadas = length(features_seleccionadas)
    
    print('Features: ')
    print(features_seleccionadas)
    print('R2 maximo: ')
    print(lista_se_iteracion[indice_r2_maximo_iteracion])
    
  }
  
  indice_r2_maximo_global <- which.max(lista_se_global)
  return(modelos[[indice_r2_maximo_global]])
  
}



# Definimos el dataset o la fuente de datos que utilizaremos 
dataset<-Boston
head(dataset)

# Obtenemos el numero de features que tiene el dataset
n_features<-ncol(Boston) - 1
n_features

# Definimos el nombre del feature, dentro del dataset, que tomaremos como Y
yname <- 'medv'

# Ejecutamos la funci�n de forward_stepwise_selection
print('FORWARD STEPWISE SELECTION')
modelo_forward <- forward_stepwise_selection(dataset, yname)
print(modelo_forward)

# Ejecutamos la funci�n de backward_stepwise_selection
print('BAKWARD STEPWISE SELECTION')
modelo_backward <- backward_stepwise_selection(dataset, yname)
print(modelo_backward)


