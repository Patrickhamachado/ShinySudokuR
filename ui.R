


# Librerías ---------------------------------------------------------------

library(shiny)
library(bslib)
library(shinyWidgets)
library(markdown)


# UI ----------------------------------------------------------------------

page_navbar( 
  nav_panel("Game", icon = icon('gamepad'),
            
            # Keyboard pressed script
            tags$script('
      $(document).on("keyup", function (e) {
         Shiny.onInputChange("myKeyboard", [e.which, e.timeStamp]);
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
              # choices = 1:9,
              selected = 1, 
              status = "primary",
              checkIcon = list(yes = icon("ok", lib = "glyphicon")),
              choiceNames = paste(1:9, "(0)"),
              choiceValues = 1:9
            ),
            
            # Plot Sudoku
            # uiOutput("plotui")
            fluidRow(plotOutput("plotSudoku", click = "plot_click")),
            
            br(),
            
            # Botones de Deshacer y Rehacer ----
            fluidRow(
              column(width = 2, offset = 2,
                     # Botón Undo
                     actionButton("btn_deshacer", label = "Undo",
                                icon = icon("arrow-rotate-left"),
                                class = "btn-primary"
                     )
                     # actionBttn("btn_deshacer", label = "Undo",
                     #            icon = icon("arrow-rotate-left"),
                     #            style = "fill"
                     # )
              ),
              
              column(width = 2, 
                     # Botón Redo
                     # actionBttn("btn_rehacer", label = "Redo",
                     #            icon = icon("arrow-rotate-right"),
                     #            style = "fill"
                     # )
                     actionButton("btn_rehacer", label = "Redo",
                                  icon = icon("arrow-rotate-right"),
                                  class = "btn-primary"
                     )
              )
              
            )
            
            
            
            ), # Fin panel Game
  
  nav_panel("?", includeMarkdown("instructions.Rmd")),
  title = strong("Shiny Sudoku"), 
  id = "navbar", 
  theme = bs_theme(version = 5, bootswatch = "flatly")
) 
