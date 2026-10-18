


# Librerías ---------------------------------------------------------------

library(shiny)
library(bslib)
# library(bs4Dash)
# library(markdown)
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



# Define UI for application that draws a histogram
# shinyUI(dashboardPage(  
#         #dashboardPagePlus(
#   
#   #   # Keyboard pressed script
#   #   tags$script('
#   #   $(document).on("keyup", function (e) {
#   #      Shiny.onInputChange("myKeyboard", e.which);
#   #   });
#   # '),
#   
#   # Header ----
#   dashboardHeader(
#     actionButton("taca", "Taca"),
#     title = "Shiny Sudoku", 
#                       fixed = FALSE
#                   
#                       ),
#   
#   # Sidebar ----
#   dashboardSidebar(),
#   
#   ## dashboardBody ----
#   dashboardBody(
#     
#     # Keyboard pressed script
#     tags$script('
#       $(document).on("keyup", function (e) {
#          Shiny.onInputChange("myKeyboard", [e.which,e.timeStamp]);
#       });
#                   '
#     ),
#     
#     # Selector de temas
#     # shinythemes::themeSelector(),
#     
#     # Pestañas
#     tabItems(
#       
#       ## Tab 1: Play ----
#       tabItem(tabName = "Play",
#               fluidRow(
#                 box(width = 10,
#                         height = 600,
#                         # title = "Controls",
#                         # status = "primary",
#                         
#                         # Botones para llenar
#                         radioGroupButtons(
#                           inputId = "fill",
#                           label = "Fill/mark with:",
#                           choices = 1:9,
#                           selected = 1, 
#                           status = "primary",
#                           checkIcon = list(
#                             yes = icon("ok", 
#                                        lib = "glyphicon"))
#                         ),
#                         
#                         # Selector de Marcar
#                         switchInput(
#                           inputId = "marking",
#                           label = "Action",
#                           value = TRUE,
#                           # labelWidth = "80px",
#                           onLabel = "Fill",
#                           offLabel = "Mark",
#                           onStatus = "primary",
#                           offStatus = "info"
#                         ),
#                         
#                         # Plot Sudoku
#                         # plotOutput("plotSudoku",
#                         #            width = "498px",
#                         #            height = "448px"),
#                         
#                         uiOutput("plotui")
#                         
#                 ) # Fin box
#               ) # Fin fluidRow
#               
#       ), # Fin Tab 1: Play
#       
#       ## Tab 2: Instructions ----
#       tabItem(tabName = "Instructions",
#               # h2("Instructions tab content"),
#               includeMarkdown("instructions.Rmd")
#       ) # Fin Tab 2: Instructions
#     ) # Fin tabItems
#   ), # Fin dashboardBody
#   
#   
#   ## controlbar ----
#   controlbar = bs4DashControlbar(
#     
#     selectInput("dificultad", "Difficulty:",
#                 c("Very easy" = 16,
#                   "Easy" = 28,
#                   "Medium" = 41,
#                   "Hard" = 49,
#                   "Very hard" = 57),
#                 selected = 41
#                 ),
#     br(),
#     hr(),
#     
#     h3("Version: 0.0.2")
#     
#     # h1("Disfruta jugando Sudoku en Shiny!"),
#     # h3("Instructions:"),
#     # li("Select the number you want to use for filling or marking in the upper selection buttons"),
#     # p("**TIP:** Pres a number in the keyboard to select it"),
#     # 
#     # li("In the button `Action` select if you want to Fill or Mark"),
#     # p("**TIP:** Pres [Ctrl] or [Shift] in the keyboard to toggle the selection"),
#     # 
#     # li("Click in the field you want to fill or mark with the selected number"),  
#     # 
#     # li("Enjoy!")
#     
#     
#     # id = NULL,
#     # disable = FALSE,
#     # width = 250,
#     # collapsed = TRUE,
#     # overlay = TRUE,
#     # skin = NULL,
#     # pinned = NULL
#   )
#   
#   # Otros parámetros dashboardPage ----
#   # skin = "red", # "blue", "blue-light", "black", "black-light",
#   # "purple", "purple-light", "green", "green-light", 
#   # "red", "red-light", "yellow", "yellow-light"
#   # collapse_sidebar = TRUE
#   # sidebar_background = 'light'
#   # sidebar_fullCollapse = FALSE
#   
# ) # Fin fluid page
# 
# 
# ) # Fin shinyUI
