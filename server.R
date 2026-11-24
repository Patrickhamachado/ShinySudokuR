
# Librerias ---------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(sudoku)
# library(data.table)


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
  
  # bs_themer()
  
  ## Inicialización valores reactivos ----
  sudoku_solucion <- reactiveVal(
    Sudoku$new(
      estado = data.frame(solucion = 0, problema = FALSE, posibles = 0,
                          filas = 0, columnas = 0

                          ),
      # estado = fx_iniciar_dt(16, matrix(0, 9, 9)),
      movimientos = data.frame(), # (Deshacer = 1:3),
      mov_rehacer = data.frame(), # (Rehacer = 4:6)
      no_movimientos = 0L,
      no_rehacer = 0L,
      terminado = FALSE,
      faltan = rep(9L, 9)
    ))
  
  
  ## Plot output ----
  output$plotSudoku <- renderPlot(
    width = 498, height = 448,
    {
      # sudoku <- sudoku_solucion()$copy()
      
      # Llamar función Plot
      sudoku_solucion()$plotSudoku(as.integer(input$fill))
      
      
    }) # Fin renderPlot output
  
  
  ## observeEvent play button ----
  observeEvent(input$play, {
    
    sudoku <- sudoku_solucion()$copy()
    
    # Revisar si ya se empezó a resolver el juego
    solucion_iniciada <- sum(sudoku$estado$solucion[!sudoku$estado$problema]) > 0
    posibles_iniciados <- sum(sudoku$estado$posibles[!sudoku$estado$problema]) > 0
    problema_vacio <- sum(sudoku$estado$solucion) == 0
    
    if((isFALSE(sudoku$terminado) & (solucion_iniciada | posibles_iniciados)) &
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
      sudoku$iniciar(dificultad = as.integer(input$dificultad))
      sudoku_solucion(sudoku)
      
      # sudoku_problema(generateSudoku(Nblank = as.integer(input$dificultad)))
      # sudoku_solucion(sudoku_problema())
      # sudoku_posibles(matrix(0, 9, 9))
      # sudoku_resuelto(FALSE)
      # faltan(fx_faltan(sudoku_problema()))
      
      showNotification("Enjoy your Sudoku!", type = "message")
      # shinyWidgets::show_toast("Enjoy your Sudoku!", type = "success")
      
      # print("Solución:")
      # solveSudoku(sudoku_problema())
    }
    
    
  })
  
  
  ## observeEvent confirmar_nuevo ----
  observeEvent(input$confirmar_nuevo, {
    
    if (isTRUE(input$confirmar_nuevo)) {
      
      # Inicialización de los datos del nuevo juego 
      sudoku <- sudoku_solucion()$copy()
      sudoku$iniciar(dificultad = as.integer(input$dificultad))
      sudoku_solucion(sudoku)
      
      # Inicialización de los datos del nuevo juego 
      # sudoku_problema(generateSudoku(Nblank = as.integer(input$dificultad)))
      # sudoku_solucion(sudoku_problema())
      # sudoku_posibles(matrix(0, 9, 9))
      # sudoku_resuelto(FALSE)
      # faltan(fx_faltan(sudoku_problema()))

      showNotification("Enjoy your new Sudoku!", type = "message")
      
    }
    
  })
  
  
  ## observeEvent plot_click ----
  observeEvent(input$plot_click, {
    
    llenar_con <- as.integer(isolate({input$fill}))
    llenar_fila <- 10 - round((input$plot_click$y + 1)/ 3)
    llenar_columna <- round((input$plot_click$x + 1)/ 3)
    definitivo <- isolate({input$marking})
    
    # print(paste("Numero:", llenar_con, "- Posicion: ", llenar_fila, ",",
    #       llenar_columna, ifelse(definitivo, "- Llenar", "- Marcar")
    #       ))
    
    sudoku <- sudoku_solucion()$copy()
    sudoku$play(llenar_con, llenar_fila, llenar_columna, definitivo)
    sudoku_solucion(sudoku)
    
 
  })
  
  
  ## observeEvent btn_deshacer ----
  observeEvent(input$btn_deshacer, {
    
    sudoku <- sudoku_solucion()$copy()
    
    if (sudoku$no_movimientos > 0) {
      sudoku$deshacer()
      sudoku_solucion(sudoku)
    }
    
    # print(paste("-------- Comando Deshacer - Historial de movimientos:", 
    #             sudoku$no_movimientos))
    # print(sudoku$movimientos)
    # print(paste("Movimientos a rehacer:", sudoku$no_rehacer))
    # print(sudoku$mov_rehacer)
    
  })
  
  
  ## observeEvent btn_rehacer ----
  observeEvent(input$btn_rehacer, {
    
    sudoku <- sudoku_solucion()$copy()
    
    if (nrow(sudoku$mov_rehacer) > 0) {
      sudoku$rehacer()
      sudoku_solucion(sudoku)
    }
    
    # print(paste("-------- Comando Rehacer - Historial de movimientos:", 
    #             sudoku$no_movimientos))
    # print(sudoku$movimientos)
    # print(paste("Movimientos a rehacer:", sudoku$no_rehacer))
    # print(sudoku$mov_rehacer)
    
  })
  
  
  ## Keyboard action ----
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
  
  
  ## observe sudoku_solucion(): faltantes ----
  observe({
    
    # Actualización de la indicación de faltantes
    faltantes <- sudoku_solucion()$faltan
    
    rotulos <- paste0(1:9, " (", faltantes, ")")
    valor <- isolate(input$fill)
    
    updateRadioGroupButtons(
      session = session,
      inputId = "fill",
      label = "Fill/mark with:",
      # choices = 1:9,
      selected = valor, 
      status = "primary",
      checkIcon = list(yes = icon("ok", lib = "glyphicon")),
      choiceNames = rotulos,
      choiceValues = 1:9
    )
    
    # Habilitar botón de deshacer
    hab_deshacer <- sudoku_solucion()$no_movimientos > 0
    if (hab_deshacer) {
      updateActionButton(inputId = "btn_deshacer", disabled = FALSE)
    } else {
      updateActionButton(inputId = "btn_deshacer", disabled = TRUE)
    }
    
    # Habilitar botón de rehacer
    hab_rehacer <- sudoku_solucion()$no_rehacer > 0
    if (hab_rehacer) {
      updateActionButton(inputId = "btn_rehacer", disabled = FALSE)
    } else {
      updateActionButton(inputId = "btn_rehacer", disabled = TRUE)
    }
    
  })
  
}) # Fin server