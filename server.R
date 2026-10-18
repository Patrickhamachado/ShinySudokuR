
# Librerias ---------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(sudoku)
library(binaryLogic)


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
    sudoku_resuelto <- reactiveVal(FALSE)
    
    
    # observeEvent plot_click ----
    observeEvent(input$plot_click, {
      
      # Si ya se resolvió el juego, mostrar pantalla inicial
      if(sudoku_resuelto()) {
        sudoku_problema(matrix(0, 9, 9))
        sudoku_solucion(matrix(0, 9, 9))
        sudoku_posibles(matrix(0, 9, 9))
        sudoku_resuelto(FALSE)
      }
      
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
              if(terminado & !sudoku_resuelto()) {
                
                sendSweetAlert(
                  # session = session,
                  title = "Congrats !!",
                  text = "You have solved the Sudoku just fine!",
                  type = "success"
                )
                sudoku_resuelto(TRUE)
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
                       sudoku_resuelto())
        
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
            }
        }
        
    })
    
    
    # observeEvent play button ----
    observeEvent(input$play, {
      
      # print('sudoku_problema antes:')
      # print(sudoku_problema())
      
      # Revisar si ya se empezó a resolver el juego
      solucion_inicida <- !identical(sudoku_problema(), sudoku_solucion())
      posibles_inicidos <- !identical(sudoku_posibles(), matrix(0, 9, 9))
      problema_vacio <- identical(sudoku_problema(), matrix(0, 9, 9))
      if((isFALSE(sudoku_resuelto()) & (solucion_inicida | posibles_inicidos)) &
         !problema_vacio) {
        
        confirmSweetAlert(
          # session = session,
          inputId = "confirmar_nuevo",
          title = "New game",
          text = "Are you sure you want to lose your progress and start a new game?",
          type = "warning",
          btn_labels = c("Cancel", "Yes"),
          btn_colors = c("#03c7e8", "#03c7e8"),
          allowEscapeKey = TRUE
        )
        
      } else {
        # Inicialización de los datos del nuevo juego 
        sudoku_problema(generateSudoku(Nblank = as.integer(input$dificultad)))
        sudoku_solucion(sudoku_problema())
        sudoku_posibles(matrix(0, 9, 9))
        sudoku_resuelto(FALSE)
        showNotification("Enjoy your Sudoku!", type = "message")
        # shinyWidgets::show_toast("Enjoy your Sudoku!", type = "success")
        
        # print("Solución:")
        # solveSudoku(sudoku_problema())
      }
    })
    
    ## observeEvent confirmar_nuevo
    observeEvent(input$confirmar_nuevo, {
      
      if (isTRUE(input$confirmar_nuevo)) {
        # Inicialización de los datos del nuevo juego 
        sudoku_problema(generateSudoku(Nblank = as.integer(input$dificultad)))
        sudoku_solucion(sudoku_problema())
        sudoku_posibles(matrix(0, 9, 9))
        sudoku_resuelto(FALSE)
        showNotification("Enjoy your new Sudoku!", type = "message")
      }
      
    })
    
}) # Fin server
