

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



# Actualización manifest --------------------------------------------------

rsconnect::writeManifest(appDir = ".")
