
# Librerias ---------------------------------------------------------------

library(shiny)
# library(shinydashboard)
# library(shinydashboardPlus)
library(bs4Dash)
library(shinyWidgets)
library(sudoku)
library(binaryLogic)



# Data --------------------------------------------------------------------

# Generar Sudoku
set.seed(Sys.time())
sudoku <- generateSudoku(Nblank = 4, print.it = TRUE) # 42


# Dificultad:
# https://github.com/sepehr-rs/sudoku-engine

# Difficulty guide:

# Difficulty	Missing cells (9x9)	Approximate feel
# 0.2	~16	Very easy
# 0.35	~28	Easy
# 0.5	~41	Medium
# 0.6	~49	Hard
# 0.7	~57	Very hard


# Funciones ---------------------------------------------------------------

# Función para imprimir ----
fx_plot <- function(matSudoku, 
                    matColores = ifelse(matSudoku, "blue", "black"), 
                    matPosibles = matSudoku * 0,
                    SudokuOK = FALSE) {
    
    # Corroborar si ya está solucionado
    solved <- (sum(colSums(matSudoku) == 45) + 
                   sum(rowSums(matSudoku) == 45)) # == 18
    if(solved == 18) {
        matColores <- 
            matrix(data = "blue", nrow = 9, ncol = 9)
        matY <<- matColores
    }
    
    # Cuadro 27x27
    par(mar = c(0, 0, 0, 0),
        # bg = "white", # ifelse(solved == 18, "white", "green"),
        fin = c(6.2, 6.2)) # fin / pin = c(6.2, 4.2)
    plot(0.5:27.5, 0.5:27.5, type = "n", axes = FALSE, 
         xlab = "", ylab = "")
    # cusr <<- par("usr")
    # cplt <<- par("plt")
    segments(seq(0.5 , 27.5, by = 3), rep(0.5, 10), 
             seq(0.5 , 27.5, by = 3), rep(27.5, 10), 
             col = "grey")
    segments(rep(0.5, 10), seq(0.5 , 27.5, by = 3), 
             rep(27.5, 10), seq(0.5 , 27.5, by = 3), 
             col = "grey")
    segments(c(0, 9, 18, 27) + 0.5, rep(0.5, 4), 
             c(0, 9, 18, 27) + 0.5, rep(27.5, 4), lwd = 3)
    segments(rep(0.5, 4), c(0, 9, 18, 27) + 0.5, rep(27.5, 4), 
             c(0, 9, 18, 27) + 0.5, lwd = 3)
    
    
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
    if(solved == 18 & !SudokuOK) {
        
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
# fx_plot(generateSudoku())


# Shiny Server ------------------------------------------------------------

shinyServer(function(input, output, session) {
    
    # Inicialización Matriz con los valores definitivos ----
    matX <- sudoku
    
    # Inicialización Matriz con colores
    matY <- ifelse(matX, "blue", "black")
    
    # Inicialización Matriz con las posibilidades
    matZ <- matX * 0
    
    # Inicialización de indicador de Sudoku terminado
    SudokuOK <- FALSE
    

    # Función para imprimir ----
    fx.plot <- function(matSudoku, matColores, matPosibles) {
        
        # Corroborar si ya está solucionado
        solved <- (sum(colSums(matSudoku) == 45) + 
                       sum(rowSums(matSudoku) == 45)) # == 18
        if(solved == 18) {
            matColores <- 
                matrix(data = "blue", nrow = 9, ncol = 9)
            matY <<- matColores
        }
        
        # Cuadro 27x27
        par(mar = c(0, 0, 0, 0),
            # bg = "white", # ifelse(solved == 18, "white", "green"),
            fin = c(6.2, 6.2)) # fin / pin = c(6.2, 4.2)
        plot(0.5:27.5, 0.5:27.5, type = "n", axes = FALSE, 
             xlab = "", ylab = "")
        # cusr <<- par("usr")
        # cplt <<- par("plt")
        segments(seq(0.5 , 27.5, by = 3), rep(0.5, 10), 
                 seq(0.5 , 27.5, by = 3), rep(27.5, 10), 
                 col = "grey")
        segments(rep(0.5, 10), seq(0.5 , 27.5, by = 3), 
                 rep(27.5, 10), seq(0.5 , 27.5, by = 3), 
                 col = "grey")
        segments(c(0, 9, 18, 27) + 0.5, rep(0.5, 4), 
                 c(0, 9, 18, 27) + 0.5, rep(27.5, 4), lwd = 3)
        segments(rep(0.5, 4), c(0, 9, 18, 27) + 0.5, rep(27.5, 4), 
                 c(0, 9, 18, 27) + 0.5, lwd = 3)
        
        
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
        if(solved == 18 & !SudokuOK) {
            
            sendSweetAlert(
                session = session,
                title = "Congrats !!",
                text = "You have solved the Sudoku just fine!",
                type = "success"
            )
            
            SudokuOK <<- TRUE
        }
        
        
        
    } # Fin función fx.plot
    
    
    # Valor seleccionado para llenar ----
    # Cambia solo con el click del gráfico
    fx.llenar <- reactive({

        llenar.con <- isolate({input$fill})
        llenar.en <- input$plot_click
        return(c(llenar.con, 
                 10 - round((input$plot_click$y + 1)/ 3),
                 round((input$plot_click$x + 1)/ 3)
        ))
    })
    
    # Plot output ----
    output$plotSudoku <- renderPlot(
        width = 498, height = 448,
        {
        
        # Traer datos del click
        Llenar <- fx.llenar()
        Llenar <- as.integer(Llenar)
        
        # Traer dato del selector Fill/Mark
        bitLlenar <- isolate({input$marking})
        
        # Actualizar matX o matZ
        if (!is.na(Llenar[2]) & !is.na(Llenar[3]) &
            (Llenar[2] > 0) & (Llenar[2] < 10) &
            (Llenar[3] > 0) & (Llenar[3] < 10)
        ) {
            if (matY[Llenar[2], Llenar[3]] != "blue") {
                
                if (bitLlenar) {
                    
                    # Editar la matriz de números fijos
                    if (matX[Llenar[2], Llenar[3]] == Llenar[1]) {
                        matX[Llenar[2], Llenar[3]] <<- 0
                    } else {
                        matX[Llenar[2], Llenar[3]] <<- Llenar[1]
                    }
                } else {
                    if(matX[Llenar[2], Llenar[3]] == 0) {
                        
                        # Editar la matriz de números opcionados
                        actual <- as.binary(matZ[Llenar[2], Llenar[3]],
                                            littleEndian = TRUE, n = 9)
                        actual[Llenar[1]] <- ! actual[Llenar[1]]
                        actual <- sum(2 ^ ((1:9)[actual] - 1))
                        matZ[Llenar[2], Llenar[3]] <<- actual
                        
                    } # Fin marcar si no hay número fijo
                    
                } # Fin cambio en las matrices
            } # Fin llenado si no es un número original
        } # Fin actualización de matrices
        
        # Llamar función Plot
        # fx.plot(matX, matY, matZ)
        fx_plot(matX, matY, matZ, SudokuOK)
        
    }) # Fin renderPlot output
    
    
    # Plot UI para detectar el click ----
    output$plotui <- renderUI({
        plotOutput("plotSudoku",
                   click = "plot_click"
        )
    })
    
    # Keyboard action ----
    observeEvent(input$myKeyboard, {
        newKeyboard <- input$myKeyboard[1]
        
        # cat("\nTecla: ", newKeyboard)
        
        # Shift y control para cambiar entre Fill / Mark
        if (newKeyboard == 16 | newKeyboard == 17) {
            
            # cat("\nTecla:", ifelse(newKeyboard == 16, 'Shift', 'Ctrl'))
            bitLlenar <- isolate({input$marking})
            updateSwitchInput(session,
                              inputId = "marking",
                              value = !bitLlenar)

        } else {
            # Números entre 1 y 9 para seleccionar
            if (newKeyboard > 48 & newKeyboard < 58) {
                
                # cat("\nNúmero:", newKeyboard - 48)
                updateRadioGroupButtons(session = session, 
                                        inputId = "fill",
                                   selected = as.character(newKeyboard - 48)
                )
                
                # updateRadioGroupButtons(
                #     session = session, inputId = "somevalue",
                #     selected = input$updateselected
                # )
                
            }
        }
        
    })
    
    
    # The play button ----
    observeEvent(input$play, {
      cat(paste('Dificultad:', input$dificultad, "\n"))
      
    })
    
    
    # The Done button closes the app ----
    observeEvent(input$done, {
        cat('Bye people!\n')
        stopApp()
        
    })
    
}) # Fin server
