

# Librerias ---------------------------------------------------------------

library(sudoku)
library(data.table)


# 1. Funciones ------------------------------------------------------------

## 1.1 fx_plot_sudoku ----
fx_plot_sudoku <- function(problema = matrix(0, 9, 9),
                           solucion = problema,
                           posibles = matrix(0, 9, 9),
                           terminado = FALSE,
                           num_resaltar = 1,
                           color_problema = "blue",
                           color_error = "red",
                           color_fondo = "turquoise"
) {
  
  # print("solucion")
  # print(solucion)
  # print("posibles")
  # print(posibles)
  
  # Corroborar si el Sudoku está vacío
  suma_columnas <- colSums(problema)
  suma_filas <- rowSums(problema)
  vacio <- (sum(suma_columnas == 0) + sum(suma_filas == 0)) == 18
  
  # Cuadro 27x27
  par(
    # Margenes
    mar = c(0, 0, 0, 0),   
    # Dimensiones en pulgadas
    fin = c(6.2, 6.2)
  ) 
  plot(0.5:27.5, 0.5:27.5, type = "n", axes = FALSE, xlab = "", ylab = "")
  
  # Dibujar el marco
  if(isTRUE(vacio)) {
    # Líneas verticales gruesas
    segments(c(0, 27) + 0.5, rep(0.5, 2), 
             c(0, 27) + 0.5, rep(27.5, 2), lwd = 4)
    # Líneas horizontales gruesas
    segments(rep(0.5, 2), c(0, 27) + 0.5, 
             rep(27.5, 2), c(0, 27) + 0.5, lwd = 4)
    
    # Mensaje de iniciar el Sudoku
    text(5 * 3 - 1, 30 - (4 * 3 + 1), "Press the `Play` button", cex = 3)
    text(5 * 3 - 1, 30 - (5 * 3 + 1), "to enjoy a new game!", cex = 3)
    return()
    
  } else {
    # Líneas verticales
    segments(seq(0.5 , 27.5, by = 3), rep(0.5, 10), 
             seq(0.5 , 27.5, by = 3), rep(27.5, 10), 
             col = "grey")
    # Líneas horizontales
    segments(rep(0.5, 10), seq(0.5 , 27.5, by = 3), 
             rep(27.5, 10), seq(0.5 , 27.5, by = 3), 
             col = "grey")
    # Líneas verticales gruesas
    segments(c(0, 9, 18, 27) + 0.5, rep(0.5, 4), 
             c(0, 9, 18, 27) + 0.5, rep(27.5, 4), lwd = 3)
    # Líneas horizontales gruesas
    segments(rep(0.5, 4), c(0, 9, 18, 27) + 0.5, rep(27.5, 4), 
             c(0, 9, 18, 27) + 0.5, lwd = 3)
  }
  
  # Dibujar el problema
  if(isTRUE(terminado)) {
    mat_colores <- matrix(color_problema, 9, 9)
  } else {
    mat_colores <- ifelse(problema, color_problema, "black")
  }
  
  # Llenado de la gráfica
  for (fila in 1:9) for (columna in 1:9) {
    
    # Dibujar la solucion
    if (solucion[fila, columna]) {
      
      xx <- 3 * columna - 1
      yy <- 30 - (3 * fila + 1)
      
      # Marco si corresponde al número a resaltar
      if (solucion[fila, columna] == num_resaltar) {
        # print(paste("Resaltar", num_resaltar, "en", fila, ",", columna))
        rect(xx - 1.5, yy - 1.5, xx + 1.5, yy + 1.5, col = color_fondo,
             lwd = 3)
      }
      
      # Llenado de los números de la solución
      text(xx, yy, solucion[fila, columna], 
           col = mat_colores[fila, columna], 
           font = ifelse(mat_colores[fila, columna] == color_problema, 2, 1), 
           cex = ifelse(mat_colores[fila, columna] == color_problema, 2.4, 2.2)) #2 - 1.8
      
    } else { 
      
      # Marcar  los números posibles
      actual <- as.integer(intToBits(posibles[fila, columna])[1:9])
      
      for (bit_posible in 1:9) {
        # Números posibles
        if (actual[bit_posible]) {
          
          xxx <- ifelse(bit_posible %% 3 > 0, columna * 3 - 3 + (bit_posible %% 3), 
                      columna * 3)
          yyy <- (10 - fila) * 3 - floor((bit_posible - 1) / 3)
          
          
          # Marco chico si corresponde al número a resaltar
          if (bit_posible == num_resaltar) {
            # print(paste("Resaltar posible", num_resaltar, "en", fila, ",", 
            #             columna, "x:", xxx, ", y:", yyy))
            rect(xxx - 0.45, yyy - 0.45, xxx + 0.45, yyy + 0.45, col = color_fondo,
                 border = NA)
          }
          
          text(xxx, yyy, bit_posible, font = 1, cex = 0.9)
          
        } # Fin si es necesario el número
      } # Fin marcar de 1 a 9
    } # Fin llenado múmeros posibles
  } # Fin recorrido de la matriz 9x9
  
  # Revisión de números incorrectos
  repetidos <- fx_repetidos(problema, solucion)
  if (nrow(repetidos) > 0) {
    for (fila in 1:nrow(repetidos)) {
      points(repetidos[fila, xx], repetidos[fila, yy], 
             col = repetidos[fila, color], cex = 7, lwd = 3)
    }
  }
}

# Test
# fx_plot_sudoku()
# fx_plot_sudoku(matrix(c(1:9, rep(0, 72)), 9, 9, byrow = TRUE),
#                matrix(c(1:9, 9:1, rep(0, 54), 1:9), 9, 9, byrow = TRUE),
#                matrix(c(273, rep(0, 8)), 9, 9, byrow = TRUE),
#                FALSE, 1)
# ss1 <- generateSudoku()
# fx_plot_sudoku(ss1)
# fx_plot_sudoku(ss1, solveSudoku(ss1))
# fx_plot_sudoku(ss1, solveSudoku(ss1), terminado = TRUE)


## 1.2 fx_faltan ----
#' Encuentra cuantas apariciones de cada número faltan por llenar en el Sudoku
#' 
#' @param solucion Matriz 9x9 con la solución al Sudoku
#' @returns Vector con el número de faltantes del 1 al 9
#' @examples
#' fx_faltan()
#' fx_faltan(generateSudoku())
#' fx_faltan(matrix(c(1:9, 9:1, rep(0, 54), 1:9), 9, 9, byrow = TRUE))
#' 
fx_faltan <- function(solucion = matrix(0, 9, 9)) {
  faltan <- rep(9, 9)
  for (digito in 1:9) {
    faltan[digito] <- 9 - sum(solucion == digito)
  }
  
  # print(paste("Faltan:", paste(faltan, collapse = ", ")))
  faltan
}

# Test 
# fx_faltan()
# fx_faltan(generateSudoku())
# fx_faltan(matrix(c(1:9, 9:1, rep(0, 54), 1:9), 9, 9, byrow = TRUE))


## 1.3 fx_repetidos ----
#' Encuentra números repetidos en filas, columnas o casillas
#' 
#' @param problema Matriz 9x9 con el Sudoku problema
#' @param solucion Matriz 9x9 con la solución al Sudoku
#' @param color_problema Color para los círculos correctos
#' @param color_error Color para los círculos en conflicto
#' @returns data.table con las coordenadas y colores de los círculos a dibujar
#' @examples
#' fx_repetidos()
#' fx_repetidos(generateSudoku())
#' fx_repetidos()
#' fx_repetidos(matrix(c(1:9, rep(0, 72)), 9, 9, byrow = TRUE), 
#' matrix(c(1:9, 9:1, rep(0, 54), 1:9), 9, 9, byrow = TRUE))

#' 
fx_repetidos <- function(problema = matrix(0, 9, 9),
                         solucion = problema,
                         color_problema = "blue",
                         color_error = "red"
) {
  
  # Matriz base
  repetidos <- data.table(filas = rep(1:9, each = 9),
                          columnas = rep(1:9, 9),
                          casillas = c(rep(rep(1:3, each = 3), 3), 
                                       rep(rep(4:6, each = 3), 3), 
                                       rep(rep(7:9, each = 3), 3)),
                          color = color_error,
                          numeros = 0,
                          cierto = 0
  )
  
  # Llenado de la matriz
  for (fila in 1:9) {
    for (columna in 1:9) {
      repetidos[filas == fila & columnas == columna, `:=` (
        numeros = solucion[fila, columna], cierto = problema[fila, columna]
      )]
    }
  }
  repetidos[cierto > 0, color := color_problema]
  repetidos <- repetidos[numeros > 0]
  
  final <- data.table(filas = integer(0), columnas = integer(0), 
                      color = character(0))
  
  # Loop de números del 1 al 9
  for (numero in 1:9) {
    # Loop de columnas del 1 al 3: filas, columnas, casillas
    for (grupo in 1:3) {
      
      grupos <- unlist(repetidos[, ..grupo])
      repetidos_numero <- repetidos[, .(filas, columnas, grupos = grupos,
                                        color, numeros)]
      # Loop de revisión del grupo 1 al 9
      for (grupo in 1:9) {
        repetidos_grupo <- repetidos_numero[grupos == grupo & numeros == numero]
        
        if (nrow(repetidos_grupo) > 1) {
          final <- rbind(final, repetidos_grupo[, .(filas, columnas, color)])
        } 
      }
    }
  }
  
  # Calculo de coordenadas
  final <- unique(final[])
  final[, .(filas, columnas, color, xx = columnas * 3 - 1, 
            yy = 29 - 3 * filas)]
  
}

# fx_repetidos()
fx_repetidos(matrix(c(1:9, rep(0, 72)), 9, 9, byrow = TRUE),
             matrix(c(1:9, 9:1, rep(0, 54), 1:9), 9, 9, byrow = TRUE))

