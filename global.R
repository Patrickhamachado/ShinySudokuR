

# Librerias ---------------------------------------------------------------

library(sudoku)
library(binaryLogic)


# 1. Funciones ------------------------------------------------------------

## 1.1 fx_plot_sudoku ----
fx_plot_sudoku <- function(problema = matrix(0, 9, 9),
                           solucion = problema,
                           posibles = matrix(0, 9, 9),
                           terminado = FALSE,
                           color_problema = "blue",
                           color_error = "red"
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
  
  # Llenadado de la gráfica
  for (fila in 1:9) for (columna in 1:9) {
    
    # Dibujar la solucion
    if (solucion[fila, columna]) {
      
      if (mat_colores[fila, columna] == color_error) 
        text(3 * columna - 1, 30 - (3*fila + 1), "X", 
             col = "pink", cex = 3)
      text(3 * columna - 1, 30 - (3*fila + 1), solucion[fila, columna], 
           col = mat_colores[fila, columna], 
           font = ifelse(mat_colores[fila, columna] == color_problema, 2, 1), 
           cex = ifelse(mat_colores[fila, columna] == color_problema, 2.4, 2.2)) #2 - 1.8
    } else { 
      
      # Marcar  los números posibles
      actual <- as.binary(posibles[fila, columna], 
                          littleEndian = TRUE, n = 9)
      
      for (bit_posible in 1:9) {
        # Números posibles
        if (actual[bit_posible]) {
          text(ifelse(bit_posible %% 3 > 0, columna * 3 - 3 + (bit_posible %% 3), 
                      columna * 3),
               (10 - fila) * 3 - floor((bit_posible - 1) / 3), 
               bit_posible, font = 1, cex = 0.9)
        } # Fin si es necesario el número
      } # Fin marcar de 1 a 9
    } # Fin llenado múmeros posibles
  } # Fin recorrido de la matriz 9x9
  
}

# Test
# fx_plot_sudoku()
# fx_plot_sudoku(matrix(c(1:9, rep(0, 72)), 9, 9, byrow = TRUE),
#                matrix(c(1:9, 9:1, rep(0, 54), 1:9), 9, 9, byrow = TRUE),
#                matrix(c(7, rep(0, 8)), 9, 9, byrow = TRUE))
# ss1 <- generateSudoku()
# fx_plot_sudoku(ss1)
# fx_plot_sudoku(ss1, solveSudoku(ss1))
# fx_plot_sudoku(ss1, solveSudoku(ss1), terminado = TRUE)
