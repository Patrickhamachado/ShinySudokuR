

# Librerias ---------------------------------------------------------------

library(sudoku)
# library(data.table)


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
# fx_repetidos(matrix(c(1:9, rep(0, 72)), 9, 9, byrow = TRUE),
#              matrix(c(1:9, 9:1, rep(0, 54), 1:9), 9, 9, byrow = TRUE))


## 1.4 fx_iniciar_dt ----
#' Inicializar los datos de una partida
#' 
#' @param dificultad Dificultad deseada para el Sudoku problema
#' @returns data.table con el estado del Sudoku. Columnas: filas, columnas, casillas, 
#' problema, solucion, probables, errores
#' @examples

fx_iniciar_dt <- function(dificultad = 41, 
                          sudoku_problema = NULL, 
                          sudoku_solucion = NULL, 
                          sudoku_posibles = NULL,
                          sudoku_errores = NULL) {
  
  # Generación del Sudoku
  if (is.null(sudoku_problema)) {
    sudoku_problema <- generateSudoku(Nblank = dificultad)
  }
  
  # Generación de la Solucion
  if (is.null(sudoku_solucion)) {
    sudoku_solucion <- sudoku_problema
  }
  
  # Generación de los Posibles
  if (is.null(sudoku_posibles)) {
    sudoku_posibles <- matrix(0, 9, 9)
  }
  
  # Generación de los Errores
  if (is.null(sudoku_errores)) {
    sudoku_errores <- matrix(FALSE, 9, 9)
  }
  
  # Tabla base
  sudoku_dt <- data.table(filas = rep(1:9, each = 9),
                          columnas = rep(1:9, 9),
                          casillas = c(rep(rep(1:3, each = 3), 3), 
                                       rep(rep(4:6, each = 3), 3), 
                                       rep(rep(7:9, each = 3), 3)),
                          problema = 0, 
                          solucion = 0, 
                          posibles = 0, 
                          errores = FALSE
  )
  
  # Llenado de la Tabla
  for (fila in 1:9) {
    for (columna in 1:9) {
      sudoku_dt[filas == fila & columnas == columna, `:=` (
        problema = sudoku_problema[fila, columna],
        solucion = sudoku_solucion[fila, columna],
        posibles = sudoku_posibles[fila, columna],
        errores = sudoku_errores[fila, columna]
        )]
    }
  }
  sudoku_dt[, problema := (problema > 0)]
  
  sudoku_dt[]
  
}


# fx_iniciar_dt()
# fx_iniciar_dt(1000,
#               matrix(c(1:9, rep(0, 72)), 9, 9, byrow = TRUE),
#                matrix(c(1:9, 9:1, rep(0, 54), 1:9), 9, 9, byrow = TRUE),
#                matrix(c(273, rep(0, 8)), 9, 9, byrow = TRUE))


## 1.5 fx_revisar_dt ----
#' Revisa los datos de una partida y marca si hay números errados
#' 
#' @param sudoku data.table con el estado del Sudoku
#' @returns data.table con el estado del Sudoku y con la revisión en la columna
#'  `errores`. Columnas: filas, columnas, casillas, problema, solucion, probables, 
#'  errores
#' @examples

fx_revisar_dt <- function(sudoku) {
  
  final <- data.table(filas = integer(0), columnas = integer(0))
  
  # Loop de números del 1 al 9
  for (numero in 1:9) {
    # Loop de columnas del 1 al 3: filas, columnas, casillas
    for (grupo in 1:3) {
      
      grupos <- unlist(sudoku[, ..grupo])
      repetidos_numero <- sudoku[, .(filas, columnas, grupos = grupos, solucion)]
      # Loop de revisión del grupo 1 al 9
      for (grupo in 1:9) {
        repetidos_grupo <- repetidos_numero[grupos == grupo & solucion == numero]
        
        if (nrow(repetidos_grupo) > 1) {
          final <- rbind(final, repetidos_grupo[, .(filas, columnas)])
        } 
      }
    }
  }
  
  # Llenado de la columna de errores
  if (nrow(final) > 0) {
    for (grupo in seq_len(nrow(final))) {
      sudoku[filas == final[grupo, filas] & columnas == final[grupo, columnas],
             errores := TRUE]
    }
  }
  
  # Return
  sudoku[]
}

# fx_revisar_dt(fx_iniciar_dt())
# fx_revisar_dt(fx_iniciar_dt(1000, matrix(c(1:9, rep(0, 72)), 9, 9, byrow = TRUE),
#              matrix(c(1:9, 9:1, rep(0, 54), 1:9), 9, 9, byrow = TRUE),
#              matrix(c(273, rep(0, 8)), 9, 9, byrow = TRUE)))



## 1.6 fx_plot_sudoku_dt ----
fx_plot_sudoku_dt <- function(sudoku = NULL,
    # problema = matrix(0, 9, 9),
    #                        solucion = problema,
    #                        posibles = matrix(0, 9, 9),
                           terminado = FALSE,
                           num_resaltar = 1,
                           color_problema = "blue",
                           color_error = "red",
                           color_fondo = "turquoise"
) {
  
  # Corroborar si el Sudoku está vacío
  # suma_columnas <- sudoku[, sum(solucion), by = filas][, V1]
  # suma_filas <- sudoku[, sum(solucion), by = columnas][, V1]
  # vacio <- (sum(suma_columnas == 0) + sum(suma_filas == 0)) == 18
  
  # Cuadro 27x27
  par(
    # Margenes
    mar = c(0, 0, 0, 0),   
    # Dimensiones en pulgadas
    fin = c(6.2, 6.2)
  ) 
  plot(0.5:27.5, 0.5:27.5, type = "n", axes = FALSE, xlab = "", ylab = "")
  
  # Dibujar el marco
  if(is.null(sudoku)) {
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
  
  # Llenado de la gráfica
  for (fila in 1:9) for (columna in 1:9) {
    
    # Dibujar la solucion
    if (sudoku[filas == fila & columnas == columna, solucion]) {
      
      xx <- 3 * columna - 1
      yy <- 30 - (3 * fila + 1)
      
      # Marco si corresponde al número a resaltar
      if (sudoku[filas == fila & columnas == columna, solucion] == num_resaltar) {
        # print(paste("Resaltar", num_resaltar, "en", fila, ",", columna))
        rect(xx - 1.5, yy - 1.5, xx + 1.5, yy + 1.5, col = color_fondo,
             lwd = 3)
      }
      
      # Llenado de los números de la solución
      text(xx, yy, sudoku[filas == fila & columnas == columna, solucion], 
           col = ifelse(sudoku[filas == fila & columnas == columna, problema],
                        color_problema, "black"),
           font = ifelse(sudoku[filas == fila & columnas == columna, problema], 
                         2, 1), 
           cex = ifelse(sudoku[filas == fila & columnas == columna, problema], 
                        2.4, 2.2)
           )
      
      # Circulos de números erróneos
      if (sudoku[filas == fila & columnas == columna, errores]) {
        xx <- columna * 3 - 1
        yy <- 29 - 3 * fila
        # points(repetidos[fila, xx], repetidos[fila, yy], 
        #        col = repetidos[fila, color], cex = 7, lwd = 3)
        points(xx, yy,  
               col = ifelse(sudoku[filas == fila & columnas == columna, problema],
                            color_problema, color_error), 
               cex = 7, lwd = 3)
      }
      
      
    } else { 
      
      # Marcar  los números posibles
      actual <- as.integer(
        intToBits(sudoku[filas == fila & columnas == columna, posibles])[1:9])
      
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
  
}

# Test
# fx_plot_sudoku_dt()
# sudoku <- fx_revisar_dt(fx_iniciar_dt(1000, matrix(c(1:9, rep(0, 72)), 9, 9, byrow = TRUE),
#                matrix(c(1:9, 9:1, rep(0, 54), 1:9), 9, 9, byrow = TRUE),
#                matrix(c(273, rep(0, 8)), 9, 9, byrow = TRUE)))
# fx_plot_sudoku_dt(sudoku, FALSE, 9)
# ss1 <- generateSudoku()
# fx_plot_sudoku_dt(ss1)
# fx_plot_sudoku_dt(ss1, solveSudoku(ss1))
# fx_plot_sudoku_dt(ss1, solveSudoku(ss1), terminado = TRUE)




# 2. Clases ---------------------------------------------------------------
## 2.1 Clase Sudoku ----
Sudoku <- setRefClass(
  Class = "Sudoku",
  fields = c(
    estado = "data.frame",
    deshacer = "data.frame",
    rehacer = "data.frame",
    no_movimientos = "integer",
    no_rehacer = "integer",
    terminado = "logical",
    faltan = "integer"
  ),
  methods = list(
    
    show = function() {
      cat("Estado del Sudoku: \n")
      print(estado[estado$solucion > 0, ])
      if (nrow(deshacer) > 1) {
        cat("Jugadas para deshacer: \n")
        print(deshacer)
      }
      if (nrow(rehacer) > 1) {
        cat("Jugadas para rehacer: \n")
        print(rehacer)
      }
    },
    
    play = function(llenar_con,
                    llenar_fila,
                    llenar_columna,
                    definitivo) {
      
      # Revisar click dentro del tablero
      if (!is.na(llenar_fila) & !is.na(llenar_columna) &
          llenar_fila > 0 & llenar_fila < 10 &
          llenar_columna > 0 & llenar_columna < 10) {
        
        # Si el Sudoku está terminado, resetear estado
        if (terminado) {
          # print("Inicializar a Sudoku vacío!")
          iniciar(16, vacio = TRUE)
          return()
        }
        
        # Índice de la celda a modificar
        idx <- which(
          estado$filas == llenar_fila &
            estado$columnas == llenar_columna
        )
        # print(paste("Índice de la celda a modificar:", idx))
        
        # Revisar si es una celda modificable
        if (length(idx) > 0 && !estado$problema[idx]) {
          
          cambios(llenar_con, llenar_fila, llenar_columna, definitivo)
          
          # Último movimiento
          ultimo <- deshacer[deshacer$movimiento == no_movimientos, ]
          
          # Actualizar estado con base R
          estado$solucion[idx] <<- ultimo$solucion
          estado$posibles[idx] <<- ultimo$posibles
          
          # Actualizar errores y faltantes
          revisar()
          
          # Verificar si terminó
          suma_col <- tapply(estado$solucion, estado$filas, sum)
          suma_fil <- tapply(estado$solucion, estado$columnas, sum)
          suma_cas <- tapply(estado$solucion, estado$casillas, sum)
          
          terminado <<- (
            sum(suma_col == 45) + sum(suma_fil == 45) + sum(suma_cas == 45)
          ) == 27
          
          if (terminado) {
            sendSweetAlert(
              title = "Congrats !!",
              text = "You have solved the Sudoku just fine!",
              type = "success"
            )
          }
        } # Fin Revisar si es una celda modificable
      } # Fin Revisar click dentro del tablero
    },
    
    cambios = function(llenar_con,
                       llenar_fila,
                       llenar_columna,
                       definitivo) {
      
      no_movimientos <<- no_movimientos + 1L
      
      idx <- which(
        estado$filas == llenar_fila &
          estado$columnas == llenar_columna
      )
      
      if (definitivo) {
        sol_actual <- estado$solucion[idx]
        nueva_sol <- ifelse(sol_actual == llenar_con, 0, llenar_con)
        
        mov <- data.frame(
          movimiento = no_movimientos,
          filas = llenar_fila,
          columnas = llenar_columna,
          solucion = nueva_sol,
          posibles = estado$posibles[idx],
          solucion_ant = sol_actual,
          posibles_ant = estado$posibles[idx]
        )
      } else {
        posibles_act <- estado$posibles[idx]
        bits <- as.integer(intToBits(posibles_act)[1:9])
        bits[llenar_con] <- !bits[llenar_con]
        nuevo_posibles <- sum(2^((1:9)[as.logical(bits)] - 1))
        
        mov <- data.frame(
          movimiento = no_movimientos,
          filas = llenar_fila,
          columnas = llenar_columna,
          solucion = estado$solucion[idx],
          posibles = nuevo_posibles,
          solucion_ant = estado$solucion[idx],
          posibles_ant = posibles_act
        )
      }
      
      deshacer <<- rbind(deshacer, mov)
    },
    
    revisar = function() {
      # Resetear errores
      estado$errores <<- FALSE
      faltantes <- rep(9L, 9)
      
      # Loop de números del 1 al 9
      for (numero in 1:9) {
        
        # Calculo de faltantes
        faltantes[numero] <- 9L - sum(estado$solucion == numero)
        
        # Loop de cada tipo de grupo
        for (tipo in c("filas", "columnas", "casillas")) {
          grupos <- estado[[tipo]]
          for (g in 1:9) {
            idx <- which(grupos == g & estado$solucion == numero)
            if (length(idx) > 1) {
              estado$errores[idx] <<- TRUE
            }
          }
        }
      }
      # Actualización de faltantes
      faltan <<- faltantes
      
    },
    
    iniciar = function(dificultad = 41, vacio = FALSE) {
      
      # Tabla base
      base <- data.frame(
        filas = rep(1:9, each = 9),
        columnas = rep(1:9, 9),
        casillas = c(
          rep(rep(1:3, each = 3), 3),
          rep(rep(4:6, each = 3), 3),
          rep(rep(7:9, each = 3), 3)
        ),
        problema = FALSE,
        solucion = 0L,
        posibles = 0L,
        errores = FALSE
      )
      
      # Base faltantes
      faltantes <- rep(9L, 9)
      
      if (vacio) {
        base <- base[0, ]
      } else {
        sudoku_problema <- generateSudoku(Nblank = dificultad)
        
        # Llenado de la tabla
        for (fila in 1:9) {
          for (columna in 1:9) {
            idx <- which(
              base$filas == fila & base$columnas == columna
            )
            base$solucion[idx] <- sudoku_problema[fila, columna]
          }
        }
        base$problema <- base$solucion > 0
        
        # Cálculo de faltantes
        for (fila in 1:9) {
          faltantes[fila] <- 9L - sum(base$solucion == fila)
        }
      }
        
      estado <<- base
      deshacer <<- data.frame()
      rehacer <<- data.frame()
      no_movimientos <<- 0L
      no_rehacer <<- 0L
      terminado <<- FALSE
      faltan <<- faltantes
    },
    
    plotSudoku = function(num_resaltar = 1,
                          color_problema = "blue",
                          color_error = "red",
                          color_fondo = "turquoise") {
      
      par(mar = c(0, 0, 0, 0), fin = c(6.2, 6.2))
      plot(0.5:27.5, 0.5:27.5, type = "n", axes = FALSE, xlab = "", ylab = "")
      
      if (sum(estado$solucion) == 0) {
        segments(c(0, 27) + 0.5, rep(0.5, 2), 
                 c(0, 27) + 0.5, rep(27.5, 2), lwd = 4)
        segments(rep(0.5, 2), c(0, 27) + 0.5, 
                 rep(27.5, 2), c(0, 27) + 0.5, lwd = 4)
        text(5 * 3 - 1, 30 - (4 * 3 + 1), "Press the `Play` button", cex = 3)
        text(5 * 3 - 1, 30 - (5 * 3 + 1), "to enjoy a new game!", cex = 3)
        return()
      } else {
        segments(seq(0.5, 27.5, by = 3), rep(0.5, 10), 
                 seq(0.5, 27.5, by = 3), rep(27.5, 10), col = "grey")
        segments(rep(0.5, 10), seq(0.5, 27.5, by = 3), 
                 rep(27.5, 10), seq(0.5, 27.5, by = 3), col = "grey")
        segments(c(0, 9, 18, 27) + 0.5, rep(0.5, 4), 
                 c(0, 9, 18, 27) + 0.5, rep(27.5, 4), lwd = 3)
        segments(rep(0.5, 4), c(0, 9, 18, 27) + 0.5, 
                 rep(27.5, 4), c(0, 9, 18, 27) + 0.5, lwd = 3)
      }
      
      for (fila in 1:9) for (columna in 1:9) {
        idx <- which(
          estado$filas == fila & estado$columnas == columna
        )
        
        if (estado$solucion[idx] > 0) {
          xx <- 3 * columna - 1
          yy <- 30 - (3 * fila + 1)
          
          if (estado$solucion[idx] == num_resaltar) {
            rect(xx - 1.5, yy - 1.5, xx + 1.5, yy + 1.5, 
                 col = color_fondo, lwd = 3)
          }
          
          text(xx, yy, estado$solucion[idx], 
               col = ifelse(estado$problema[idx], color_problema, "black"),
               font = ifelse(estado$problema[idx], 2, 1), 
               cex = ifelse(estado$problema[idx], 2.4, 2.2))
          
          if (estado$errores[idx]) {
            xx2 <- columna * 3 - 1
            yy2 <- 29 - 3 * fila
            points(xx2, yy2, 
                   col = ifelse(estado$problema[idx], color_problema, color_error), 
                   cex = 7, lwd = 3)
          }
        } else {
          posibles_act <- estado$posibles[idx]
          bits <- as.integer(intToBits(posibles_act)[1:9])
          
          for (bit_posible in 1:9) {
            if (bits[bit_posible]) {
              xxx <- ifelse(bit_posible %% 3 > 0, 
                            columna * 3 - 3 + (bit_posible %% 3), 
                            columna * 3)
              yyy <- (10 - fila) * 3 - floor((bit_posible - 1) / 3)
              
              if (bit_posible == num_resaltar) {
                rect(xxx - 0.45, yyy - 0.45, xxx + 0.45, yyy + 0.45, 
                     col = color_fondo, border = NA)
              }
              
              text(xxx, yyy, bit_posible, font = 1, cex = 1.1)
              
            }
          }
        }
      }
    }
  )
)


# Test de la clase R6 
# rm(sudoku1, sudoku2)
# sudoku1 <- Sudoku$new(
#   estado = data.frame(solucion = 0, problema = FALSE, posibles = 0),
#   deshacer = data.frame(), # (Deshacer = 1:3),
#   rehacer = data.frame(), # (Rehacer = 4:6)
#   no_movimientos = 0L,
#   no_rehacer = 0L,
#   terminado = FALSE,
#   faltan = rep(9L, 9)
# )
# 
# sudoku1$plotSudoku()
# 
# sudoku1$iniciar(16)
# sudoku1$plotSudoku()
# 
# sudoku2 <- sudoku1$copy()
# 
# sudoku1$play(7, 1, 1, TRUE)
# sudoku1$play(7, 2, 1, FALSE)
# sudoku1$play(8, 3, 1, TRUE)
# sudoku1$plotSudoku()
# 
# 
# sudoku2$plotSudoku()
# identical(sudoku1, sudoku2)
# 
# # 
# sudoku1$iniciar(16)
# sudoku1$estado
# 
# sudoku1$iniciar(16, vacio = TRUE)
# sudoku1$estado

