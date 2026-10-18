


# Librerías ---------------------------------------------------------------

library(shiny)
library(bslib)
library(shinyWidgets)


# UI ----------------------------------------------------------------------

page_navbar( 
  nav_panel("Game", icon = icon('gamepad'),
            
            # Keyboard pressed script
            tags$script('
      $(document).on("keyup", function (e) {
         Shiny.onInputChange("myKeyboard", [e.which,e.timeStamp]);
      });
                  '
            ),
            
            fluidRow(
              
              column(3, 
                     # Botón Play
                     actionBttn("play", label = "Play",
                                icon = icon("play-circle"),
                                style = "fill"
                     )),
              
              column(3,
                     selectInput("dificultad", "Level:",
                                 c("Very easy" = 16,
                                   "Easy" = 28,
                                   "Medium" = 41,
                                   "Hard" = 49,
                                   "Very hard" = 57),
                                 selected = 41
                     )),
              
              column(3, offset = 1,
                     # Selector de Marcar
                     switchInput(
                       inputId = "marking",
                       label = "Action",
                       value = TRUE,
                       # labelWidth = "80px",
                       onLabel = "Fill",
                       offLabel = "Mark",
                       onStatus = "primary",
                       offStatus = "info"
                     ))
            
            ),
            
            # Botones para llenar
            radioGroupButtons(
              inputId = "fill",
              label = "Fill/mark with:",
              choices = 1:9,
              selected = 1, 
              status = "primary",
              checkIcon = list(
                yes = icon("ok", 
                           lib = "glyphicon"))
            ),
            
            # Plot Sudoku
            uiOutput("plotui")

            
            ), # Fin panel Game 
  
  nav_panel("?", includeMarkdown("instructions.Rmd")),
  title = strong("Shiny Sudoku"), 
  id = "navbar", 
) 
