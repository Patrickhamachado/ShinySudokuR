
# Librerias ---------------------------------------------------------------

library(shiny)
# library(shinydashboard)
# library(shinydashboardPlus)
# library(bs4Dash)
library(shinyWidgets)
library(sudoku)
library(binaryLogic)


# Data --------------------------------------------------------------------

# Generar Sudoku
# set.seed(Sys.time())
# sudoku <- generateSudoku(Nblank = 4, print.it = TRUE) # 42
# sudoku <- matrix(0, 9, 9)


# Dificultad:
# https://github.com/sepehr-rs/sudoku-engine

# Difficulty guide:

# Difficulty	Missing cells (9x9)	Approximate feel
# 0.2	~16	Very easy
# 0.35	~28	Easy
# 0.5	~41	Medium
# 0.6	~49	Hard
# 0.7	~57	Very hard



# Shiny Server ------------------------------------------------------------

shinyServer(function(input, output, session) {
    
    # Inicialización valores reactivos ----
    sudoku_problema <- reactiveVal(matrix(0, 9, 9))
    sudoku_solucion <- reactiveVal(matrix(0, 9, 9))
    sudoku_posibles <- reactiveVal(matrix(0, 9, 9))
    SudokuOK <- reactiveVal(FALSE)
    
    # matX <- matrix(0, 9, 9)  #sudoku
    
    # Inicialización Matriz con colores
    # matY <- ifelse(matX, "blue", "black")
    
    # Inicialización Matriz con las posibilidades
    # matZ <- matX * 0
    
    # Inicialización de indicador de Sudoku terminado
    
   
    # Valor seleccionado para llenar ----
    # Cambia solo con el click del gráfico
    # fx.llenar <- reactive({
    # 
    #     llenar.con <- isolate({input$fill})
    #     llenar.en <- input$plot_click
    #     return(c(llenar.con, 
    #              10 - round((input$plot_click$y + 1)/ 3),
    #              round((input$plot_click$x + 1)/ 3)
    #     ))
    # })
    
    
    # observeEvent plot_click ----
    observeEvent(input$plot_click, {
      
      llenar_con <- as.integer(isolate({input$fill}))
      llenar_fila <- 10 - round((input$plot_click$y + 1)/ 3)
      llenar_columna <- round((input$plot_click$x + 1)/ 3)
      definitivo <- isolate({input$marking})
      problema <- isolate(sudoku_problema())
      solucion <- isolate(sudoku_solucion())
      solucion_actual <- solucion
      posibles <- isolate(sudoku_posibles())
      posibles_actual <- posibles
      
      # print(paste(ifelse(definitivo, 'Llenar con:', 'Marcar con:'), 
      #             llenar_con, "en", llenar_fila, ",", llenar_columna))

      # Actualizar solucion o posibles
      if (!is.na(llenar_fila) & !is.na(llenar_columna) &
          (llenar_fila > 0) & (llenar_fila < 10) &
          (llenar_columna > 0) & (llenar_columna < 10)
      ) {
        # Revisar si es una celda modificable
        if (problema[llenar_fila, llenar_columna] == 0) {
          
          if (definitivo) {
            
            # Editar la matriz de números fijos
            if (solucion[llenar_fila, llenar_columna] == llenar_con) {
              solucion[llenar_fila, llenar_columna] <- 0
            } else {
              solucion[llenar_fila, llenar_columna] <- llenar_con
            }
            
            # Actualizar el valor reactivo si hubo cambios
            if(!identical(solucion_actual, solucion)) {
              sudoku_solucion(solucion)
              
              # Corroborar si ya está solucionado o está vacío
              suma_columnas <- colSums(solucion)
              suma_filas <- rowSums(solucion)
              terminado <- (sum(colSums(solucion) == 45) + 
                              sum(rowSums(solucion) == 45)) == 18
              
              # Mensaje si Sudoku terminado
              if(terminado & !SudokuOK()) {
                
                sendSweetAlert(
                  # session = session,
                  title = "Congrats !!",
                  text = "You have solved the Sudoku just fine!",
                  type = "success"
                )
                SudokuOK(TRUE)
              }
              
              
            }
            
          } else {
            if(solucion[llenar_fila, llenar_columna] == 0) {
              
              # Editar la matriz de números opcionados
              actual <- as.binary(posibles[llenar_fila, llenar_columna],
                                  littleEndian = TRUE, n = 9)
              actual[llenar_con] <- ! actual[llenar_con]
              actual <- sum(2 ^ ((1:9)[actual] - 1))
              posibles[llenar_fila, llenar_columna] <- actual
              
              # Actualizar el valor reactivo si hubo cambios
              if(!identical(posibles_actual, solucion)) sudoku_posibles(posibles)
              
            } # Fin marcar si no hay número fijo
            
          } # Fin cambio en las matrices
          
          
          
        }
        
      }
      
    })
    
    # Plot output ----
    output$plotSudoku <- renderPlot(
        width = 498, height = 448,
        {
        
        # Llamar función Plot
        fx_plot_sudoku(sudoku_problema(), sudoku_solucion(), sudoku_posibles(), 
                       SudokuOK())
        
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
    
    
    # observeEvent play button ----
    observeEvent(input$play, {
      
      # print('sudoku_problema antes:')
      # print(sudoku_problema())
      
      # Inicialización de los datos del nuevo juego 
      sudoku_problema(generateSudoku(Nblank = as.integer(input$dificultad)))
      sudoku_solucion(sudoku_problema())
      sudoku_posibles(matrix(0, 9, 9))

      print("Solución:")
      solveSudoku(sudoku_problema())
      
      
      # Actualización del Sudoku
      # matX <<- sudoku_problema()
      # # Inicialización Matriz con colores
      # matY <<- ifelse(matX, "blue", "black")
      # # Inicialización Matriz con las posibilidades
      # matZ <<- matX * 0
      
    })
    
    
    # The Done button closes the app ----
    observeEvent(input$done, {
        cat('Bye people!\n')
        stopApp()
        
    })
    
}) # Fin server
