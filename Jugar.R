

# Libreria Sudoku ---------------------------------------------------------

library(sudoku)

# Generar Sudoku
set.seed(Sys.time())
ss1 <- generateSudoku(print.it = TRUE)
# ss1 <- generateSudoku(Nblank = 40, print.it = TRUE)



# Jugar
playSudoku(ss1, display = "tk")

# Imprimir
printSudoku(ss1)

# Pista
cat(hintSudoku(ss1, 5, 1))

# Resolver
solveSudoku(ss1, verbose = TRUE)

# Traer juego
fetchSudokuUK()

# Función para imprimir ----
fx_plot <- function(matSudoku, 
                    matColores = ifelse(matSudoku, "blue", "black"), 
                    matPosibles = matSudoku * 0,
                    SudokuOK = FALSE) {
  
  # Corroborar si ya está solucionado o está vacío
  suma_columnas <- colSums(matSudoku)
  suma_filas <- rowSums(matSudoku)
  
  vacio <- (sum(suma_columnas == 0) + sum(suma_filas == 0)) == 18
  solved <- (sum(suma_columnas == 45) + sum(suma_filas == 45)) == 18
  
  if(isTRUE(solved)) {
    matColores <- 
      matrix(data = "blue", nrow = 9, ncol = 9)
    matY <<- matColores
  }
  
  # Cuadro 27x27
  par(
    # Margenes
    mar = c(0, 0, 0, 0),   
    # bg = "white", # ifelse(solved == 18, "white", "green"),
    # Dimensiones
    fin = c(6.2, 6.2)) # fin / pin = c(6.2, 4.2)
  plot(0.5:27.5, 0.5:27.5, type = "n", axes = FALSE, 
       xlab = "", ylab = "")
  # cusr <<- par("usr")
  # cplt <<- par("plt")
  
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
  
  
  # Llenado de los números fijos
  for (i in 1:9) for (j in 1:9) {
    
    if (matSudoku[i, j]) {
      
      if (matColores[i, j] == "red") 
        text(3 * j - 1, 30 - (3*i + 1), "X", 
             col = "pink", cex = 3)
      text(3 * j - 1, 30 - (3*i + 1), matSudoku[i, j], 
           col = matColores[i, j], 
           font = ifelse(matColores[i, j] == "blue", 2, 1), 
           cex = ifelse(matColores[i, j] == "blue", 2.4, 2.2)) #2 - 1.8
    } else { 
      
      # Marcar  los números posibles
      actual <- as.binary(matPosibles[i, j], 
                          littleEndian = TRUE, n = 9)
      
      for (h in 1:9) {
        # Números posibles
        if (actual[h]) {
          text(ifelse(h %% 3 > 0, j * 3 - 3 + (h %% 3), 
                      j * 3),
               (10 - i) * 3 - floor((h - 1) / 3), 
               h, font = 1, cex = 0.9)
        } # Fin si es necesario el número
      } # Fin marcar de 1 a 9
    } # Fin llenado múmeros posibles
  } # Fin recorrido de la matriz 9x9
  
  # Mensaje si Sudoku terminado
  if(solved & !SudokuOK) {
    
    sendSweetAlert(
      # session = session,
      title = "Congrats !!",
      text = "You have solved the Sudoku just fine!",
      type = "success"
    )
    
    SudokuOK <<- TRUE
  }
  
} # Fin función fx_plot

# Test
fx_plot(generateSudoku())
fx_plot(matrix(0, 9, 9))
# fx_plot(matrix(1, 9, 9))
# fx_plot(matrix(1, 9, 9), SudokuOK = TRUE)



# Function playSudoku -----------------------------------------------------

replot <- function(z) {
    par(mar = c(0, 0, 0, 0), bg = "white")
    plot(0.5:9.5, 0.5:9.5, type = "n", axes = FALSE, 
         xlab = "", ylab = "")
    cusr <<- par("usr")
    cplt <<- par("plt")
    
     # Asignación de colores
    cols <- ifelse(ss1, "blue", "black")
    
    
    segments(0.5:9.5, rep(0.5, 10), 0.5:9.5, rep(9.5, 10), 
             col = "grey")
    segments(rep(0.5, 10), 0.5:9.5, rep(9.5, 10), 0.5:9.5, 
             col = "grey")
    segments(c(0, 3, 6, 9) + 0.5, rep(0.5, 4), c(0, 3, 6, 
                                                 9) + 0.5, rep(9.5, 4), lwd = 3)
    segments(rep(0.5, 4), c(0, 3, 6, 9) + 0.5, rep(9.5, 4), 
             c(0, 3, 6, 9) + 0.5, lwd = 3)
    for (i in 1:9) for (j in 1:9) if (z[i, j]) {
        if (cols[i, j] == "red") 
            text(j, 10 - i, "X", col = "pink", 
                 cex = 3)
        text(j, 10 - i, z[i, j], col = cols[i, j], font = ifelse(cols[i, 
                                                                      j] == "blue", 2, 1), cex = ifelse(cols[i, 
                                                                                                             j] == "blue", 2, 1.8))
    }
}

replot(ss1)



# Libreria sudokuAlt ------------------------------------------------------

library(sudokuAlt)


# Traer juego
fetchUKGame() # El mismo que fetchSudokuUK()
fetchUKGame(day = 10) # El mismo que fetchSudokuUK()

fetchAUGame(day = 10, difficulty = "easy")
fetchAUGame( difficulty = "medium")
fetchAUGame( difficulty = "hard")
fetchAUGame( difficulty = "tough")

# Generar Sudoku
set.seed(54321)
ss2 <- makeGame()
ss2
# makeGame(n = 4)

# Mostrar juego original
originalGame(ss2)

# Imprimir Sudoku cómo gráfico
plot(as.sudoku(ss1))
plot(ss2)

ss3 <- solveGame(ss2)
plot(ss2)
plot(ss3)
plot(ss2, colGame = "#808080", colSolution = "blue")
plot(ss3, colGame = "#808080", colSolution = "blue")

# Imprimir Sudoku en consola
print(ss1)
print(as.sudoku(ss1))
printSudoku(ss1)
print(ss2)


# Original game
originalGame(ss3)

# regulariseGame
ss3
regulariseGame(ss3)
regulariseGame(ss3, "col")
regulariseGame(ss3, "row")
regulariseGame(ss3, "row") |> originalGame()


# Libreria tcltk ------------------------------------------------------------

library(tcltk)
demo(tcltk::tkcanvas)
demo(tcltk::tkdensity)
demo(tcltk::tkfaq)
demo(tcltk::tkttest)


